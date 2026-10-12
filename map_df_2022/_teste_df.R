# ==========================================================================
# Geolocalizacao dos locais de votacao do TSE + celulas H3 (res. 6, 7 e 8)
# ==========================================================================
# Estrategia:
#   1. A base nacional de secoes (votacao_secao_2022_BR.csv) tem o endereco
#      do local de votacao repetido em MUITAS linhas (uma por secao x cargo
#      x candidato). Geocodificar linha a linha seria desperdicio: extraimos
#      primeiro os locais de votacao UNICOS (chave = UF + municipio + zona +
#      numero do local) e geocodificamos so essa tabela, bem menor.
#   2. Secoes no exterior (SG_UF == "ZZ") ficam fora do escopo do CNEFE/IBGE
#      e sao separadas antes de geocodificar.
#   3. O campo DS_LOCAL_VOTACAO_ENDERECO e um endereco livre em uma unica
#      coluna (ex: "AV. DOM HELDER CAMARA, 5597"). O {geocodebr} exige
#      logradouro e numero em colunas separadas -> padronizar_endereco_tse()
#      faz esse parsing (trata "S/N" em varias grafias, sufixos como
#      "- ZONA RURAL"/"- CONJUNTO X", e uma eventual localidade/bairro
#      apos o numero).
#   4. Geocodificacao com {geocodebr} (usa o CNEFE do IBGE via duckdb).
#      O proprio geocode() ja calcula as celulas H3 quando se passa
#      h3_res = c(6, 7, 8) -- nao e necessario o pacote h3jsr.
#   5. O resultado (lat/lon, precisao, h3_06/07/08) e unido de volta a base
#      completa pela chave do local de votacao.
#   6. Reportamos a distribuicao de "precisao" para controle de qualidade,
#      pois nem todo endereco encontra correspondencia no nivel de numero.
# ==========================================================================

suppressMessages({
  library(data.table)
  library(stringr)
  library(geocodebr)
})

# ---- 0. Parametros -------------------------------------------------------
diretorio_base   <- "C:/Users/Espada/Documents/R/tse_project"
arquivo_entrada  <- file.path(diretorio_base, "dados", "votacao_secao_2022_BR.csv")
ufs_alvo         <- c("DF")
saida_locais     <- file.path(diretorio_base, "dados", "locais_votacao_geocodificados.csv")
saida_completa   <- file.path(diretorio_base, "dados", "votacao_secao_2022_BR_geo.parquet")

# ---- 1. Padronizacao do endereco livre em logradouro / numero / localidade ----
padronizar_endereco_tse <- function(endereco) {
  x <- str_squish(toupper(trimws(endereco)))
  # um " - " quase sempre introduz uma qualificacao (zona urbana/rural,
  # conjunto, setor, povoado etc.) que atrapalha o parsing do logradouro
  x <- str_squish(str_remove(x, "\\s+-\\s+.*$"))

  partes <- str_split(x, "\\s*,\\s*")

  sn_regex  <- "(?i)^(S\\s*/?\\s*N[\u00baO]?\\.?|SEM\\s+N[\u00daU]MERO\\.?)$"
  num_regex <- "^[0-9]+[A-Za-z]?$"

  parse_uma <- function(p) {
    p <- p[p != ""]
    if (length(p) == 0) return(c(NA_character_, NA_character_, NA_character_))
    logradouro <- p[1]
    numero <- NA_character_
    localidade <- NA_character_

    if (length(p) >= 2) {
      if (str_detect(p[2], sn_regex)) {
        numero <- NA_character_
        if (length(p) >= 3) localidade <- paste(p[3:length(p)], collapse = ", ")
      } else if (str_detect(p[2], num_regex)) {
        numero <- p[2]
        if (length(p) >= 3) localidade <- paste(p[3:length(p)], collapse = ", ")
      } else {
        resto <- paste(p[2:length(p)], collapse = ", ")
        m <- str_match(resto, "(?i)^(.*?)[,]?\\s*(S\\s*/?\\s*N[\u00baO]?\\.?|SEM\\s+N[\u00daU]MERO\\.?|[0-9]+[A-Za-z]?)$")
        if (!is.na(m[1, 1])) {
          logradouro <- str_squish(paste(logradouro, m[1, 2]))
          cand <- m[1, 3]
          numero <- if (str_detect(cand, sn_regex)) NA_character_ else cand
        } else {
          logradouro <- str_squish(paste(logradouro, resto, sep = ", "))
        }
      }
    } else {
      m <- str_match(logradouro, "(?i)^(.*?)\\s+(S\\s*/?\\s*N[\u00baO]?\\.?|SEM\\s+N[\u00daU]MERO\\.?|[0-9]+[A-Za-z]?)$")
      if (!is.na(m[1, 1])) {
        logradouro <- m[1, 2]
        cand <- m[1, 3]
        numero <- if (str_detect(cand, sn_regex)) NA_character_ else cand
      }
    }
    c(logradouro, numero, localidade)
  }

  res <- t(vapply(partes, parse_uma, character(3)))
  localidade <- str_squish(res[, 3])
  localidade[localidade == ""] <- NA_character_  # evita o bug do ifelse() virar logical quando tudo é NA
  data.table(
    logradouro = str_squish(res[, 1]),
    numero     = res[, 2],
    localidade = localidade
  )
}

# ---- 2. Leitura e deduplicacao dos locais de votacao ----------------------
cols_locais <- c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA",
                  "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "DS_LOCAL_VOTACAO_ENDERECO")

dt <- fread(arquivo_entrada, select = cols_locais, sep = ";", quote = "\"",
            encoding = "Latin-1")
# fread(encoding = "Latin-1") apenas MARCA a codificacao (Encoding() == "latin1"),
# sem retranscodificar os bytes. Como o geocodebr roda a geocodificacao num
# subprocesso (callr) e casa os enderecos via DuckDB, essa marcação se perde
# na travessia do processo e a comparação de strings falha silenciosamente
# (nem o fallback a nivel de municipio funciona). Por isso, convertemos
# explicitamente para UTF-8 "de verdade" com enc2utf8().
cols_texto <- names(dt)[vapply(dt, is.character, logical(1))]
dt[, (cols_texto) := lapply(.SD, enc2utf8), .SDcols = cols_texto]

if (!is.null(ufs_alvo)) dt <- dt[SG_UF %in% ufs_alvo]

dt[, chave_local := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = "_")]

exterior <- dt[SG_UF == "ZZ"]
if (nrow(exterior) > 0) {
  message(sprintf(
    "Aviso: %d linha(s) de secoes no exterior (SG_UF == 'ZZ') ficarao sem geocodificacao (fora do escopo do CNEFE/IBGE).",
    nrow(exterior)))
}

locais <- unique(dt[SG_UF != "ZZ"], by = "chave_local")
cat(sprintf("Locais de votacao unicos a geocodificar: %d (de %d linhas na base)\n",
            nrow(locais), nrow(dt)))

# ---- 3. Parsing do endereco -------------------------------------------------
locais <- cbind(locais, padronizar_endereco_tse(locais$DS_LOCAL_VOTACAO_ENDERECO))

# ---- 4. Geocodificacao com {geocodebr} + celulas H3 nativas ----------------
resultado <- geocode(
  enderecos = locais,
  campos_endereco = definir_campos(
    estado     = "SG_UF",
    municipio  = "NM_MUNICIPIO",
    logradouro = "logradouro",
    numero     = "numero",
    localidade = "localidade"
  ),
  resultado_completo = FALSE,  # resultado_completo = TRUE tem bug conhecido na v0.6.4 (coluna "empate")
  resultado_sf        = FALSE,
  h3_res              = c(6, 7, 8),
  cache               = TRUE,
  verboso             = TRUE
)
setDT(resultado)

# ---- 5. Controle de qualidade ----------------------------------------------
cat("\nDistribuicao de precisao da geocodificacao:\n")
print(resultado[, .(n = .N, pct = round(100 * .N / nrow(resultado), 1)), by = precisao][order(-n)])

# celulas H3 so fazem sentido em resolucoes finas quando a precisao chegou
# a nivel de logradouro/numero; abaixo disso (cep/localidade/municipio) o
# ponto e so um centroide administrativo, entao marcamos essa limitacao
resultado[, h3_confiavel := precisao %in% c("numero", "numero_aproximado", "logradouro")]

# ---- 6. Salvar tabela de locais unicos geocodificados ----------------------
fwrite(resultado, saida_locais)
cat(sprintf("\nTabela de locais geocodificados salva em: %s\n", saida_locais))

# ---- 7. Unir de volta a base completa (nivel secao) ------------------------
cols_geo <- c("chave_local", "lat", "lon", "precisao", "tipo_resultado",
              "desvio_metros", "endereco_encontrado", "h3_06", "h3_07", "h3_08",
              "h3_confiavel")

base_completa <- fread(arquivo_entrada, sep = ";", quote = "\"", encoding = "Latin-1")
cols_texto_bc <- names(base_completa)[vapply(base_completa, is.character, logical(1))]
base_completa[, (cols_texto_bc) := lapply(.SD, enc2utf8), .SDcols = cols_texto_bc]
base_completa[, chave_local := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = "_")]
base_completa <- merge(base_completa, resultado[, ..cols_geo], by = "chave_local", all.x = TRUE)

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(base_completa, saida_completa)
  cat(sprintf("Base completa geocodificada salva em: %s\n", saida_completa))
} else {
  saida_completa_csv <- sub("\\.parquet$", ".csv", saida_completa)
  fwrite(base_completa, saida_completa_csv)
  cat(sprintf("Pacote 'arrow' nao encontrado; base completa salva em CSV: %s\n", saida_completa_csv))
}
