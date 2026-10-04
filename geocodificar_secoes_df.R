# ==========================================================================
# Geolocalizacao dos locais de votacao do DF (TSE 2022) enriquecida com o
# Censo Escolar 2023 (INEP) + celulas H3 (res. 6, 7 e 8)
# ==========================================================================
# Por que o Censo Escolar ajuda:
#   A maioria dos locais de votacao SAO escolas, mas o endereco livre do TSE
#   (DS_LOCAL_VOTACAO_ENDERECO) usa o sistema de quadras de Brasilia
#   (SQN/SQS/EQNP/CL/AE etc.), que o CNEFE (base do {geocodebr}) quase nunca
#   reconhece no nivel de logradouro -> testamos e 100% dos locais do DF
#   ficavam presos na precisao "municipio" (so o centroide da cidade).
#   O Censo Escolar do INEP tem, para cada escola, endereco JA ESTRUTURADO
#   (logradouro, numero, bairro e principalmente CEP). Casando o local de
#   votacao com a escola correspondente por NOME, conseguimos usar esse
#   endereco estruturado no geocodebr -> precisao sobe de "municipio" para
#   "cep"/"localidade" na maioria dos casos (validado: 56% em "cep", 5% em
#   "localidade", vs. 0% antes do enriquecimento).
#
# Risco a evitar no casamento por nome: nomes de escola no DF sao
# numerados (EC 41, CEF 104 Norte...) e a mesma sigla se repete em
# varias regioes administrativas com numeros DIFERENTES (EC 41, EC 42...).
# Uma distancia de string ingenua (Jaro-Winkler puro) pode achar mais
# "parecido" o numero ERRADO (ex.: confundir "104 NORTE" com "410 NORTE").
# Por isso o casamento aqui:
#   1. Canoniza abreviacoes (CENTRO DE ENSINO FUNDAMENTAL -> CEF etc.)
#   2. Extrai o(s) numero(s) do nome e EXIGE numero identico como
#      condicao dura sempre que o nome tiver numero.
#   3. So usa fuzzy-text puro (sem numero) quando o nome NAO tem numero,
#      e mesmo assim so aceita se o 1o colocado for claramente melhor
#      que o 2o (margem de seguranca), para nao "adivinhar" errado.
# ==========================================================================

suppressMessages({
  library(data.table)
  library(stringr)
  library(stringdist)
  library(geocodebr)
})

# ---- 0. Parametros ---------------------------------------------------------
diretorio_base   <- "C:/Users/Espada/Documents/R/tse_project"
arquivo_votacao  <- file.path(diretorio_base, "dados", "votacao_secao_2022_BR.csv")
arquivo_escolas  <- file.path(diretorio_base, "dados", "microdados_ed_basica_2023.csv")
saida_locais     <- file.path(diretorio_base, "dados", "secoes_df_locais_geocodificados.csv")
saida_completa   <- file.path(diretorio_base, "dados", "secoes_df_geo.parquet")

# ---- 1. Normalizacao de nomes p/ casamento TSE x INEP ----------------------
dic_tipos <- c(
  "CENTRO DE ENSINO FUNDAMENTAL"       = "CEF",
  "ESCOLA DE ENSINO FUNDAMENTAL"       = "CEF",
  "CENTRO DE ENSINO MEDIO"             = "CEM",
  "ESCOLA DE ENSINO MEDIO"             = "CEM",
  "ESCOLA CLASSE"                      = "EC",
  "CENTRO EDUCACIONAL"                 = "CED",
  "CENTRO DE EDUCACAO INFANTIL"        = "CEI",
  "JARDIM DE INFANCIA"                 = "JI",
  "CENTRO DE ENSINO ESPECIAL"          = "CEE",
  "CENTRO INTERESCOLAR DE LINGUAS"     = "CIL",
  "CENTRO DE ENSINO EM PERIODO INTEGRAL" = "CEPI",
  "CENTRO DE ATENCAO INTEGRAL A CRIANCA" = "CAIC"
)

normalizar_nome <- function(x) {
  x <- toupper(trimws(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- str_squish(x)
  for (i in seq_along(dic_tipos)) {
    x <- str_replace(x, paste0("^", names(dic_tipos)[i], "\\b"), dic_tipos[i])
  }
  x
}
extrair_numeros <- function(x) {
  nums <- str_extract_all(x, "[0-9]+")
  vapply(nums, function(n) paste(sort(as.integer(n)), collapse = "-"), character(1))
}
extrair_tipo  <- function(x) word(x, 1)
extrair_resto <- function(x) str_squish(str_remove_all(x, "[0-9]+"))

# ---- 2. Padronizacao do endereco livre do TSE (fallback p/ quem nao casar) -
padronizar_endereco_tse <- function(endereco) {
  x <- str_squish(toupper(trimws(endereco)))
  x <- str_squish(str_remove(x, "\\s+-\\s+.*$"))
  partes <- str_split(x, "\\s*,\\s*")
  sn_regex  <- "(?i)^(S\\s*/?\\s*N[\u00baO]?\\.?|SEM\\s+N[\u00daU]MERO\\.?)$"
  num_regex <- "^[0-9]+[A-Za-z]?$"
  parse_uma <- function(p) {
    p <- p[p != ""]
    if (length(p) == 0) return(c(NA_character_, NA_character_, NA_character_))
    logradouro <- p[1]; numero <- NA_character_; localidade <- NA_character_
    if (length(p) >= 2) {
      if (str_detect(p[2], sn_regex)) {
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
  localidade <- str_squish(res[, 3]); localidade[localidade == ""] <- NA_character_
  data.table(logradouro = str_squish(res[, 1]), numero = res[, 2], localidade = localidade)
}

# ---- 3. Le e deduplica os locais de votacao do DF --------------------------
cols_locais <- c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA",
                  "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "DS_LOCAL_VOTACAO_ENDERECO")
dt <- fread(arquivo_votacao, select = cols_locais, sep = ";", quote = "\"", encoding = "Latin-1")
cols_texto <- names(dt)[vapply(dt, is.character, logical(1))]
dt[, (cols_texto) := lapply(.SD, enc2utf8), .SDcols = cols_texto]  # fread(encoding="Latin-1") so marca, nao retranscodifica
dt <- dt[SG_UF == "DF"]
dt[, chave_local := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = "_")]
secoes_df <- unique(dt, by = "chave_local")
secoes_df[, nome_norm := normalizar_nome(NM_LOCAL_VOTACAO)]
secoes_df[, `:=`(tipo = extrair_tipo(nome_norm), numero = extrair_numeros(nome_norm), resto = extrair_resto(nome_norm))]
cat(sprintf("Locais de votacao unicos no DF: %d\n", nrow(secoes_df)))

# ---- 4. Le as escolas ativas do DF (Censo Escolar 2023) --------------------
cols_esc <- c("NO_ENTIDADE", "CO_ENTIDADE", "SG_UF", "CO_MUNICIPIO", "NO_MUNICIPIO",
              "TP_SITUACAO_FUNCIONAMENTO", "DS_ENDERECO", "NU_ENDERECO",
              "DS_COMPLEMENTO", "NO_BAIRRO", "CO_CEP")
esc <- fread(arquivo_escolas, select = cols_esc, sep = ";", quote = "\"", encoding = "Latin-1")
cols_texto_e <- names(esc)[vapply(esc, is.character, logical(1))]
esc[, (cols_texto_e) := lapply(.SD, enc2utf8), .SDcols = cols_texto_e]
esc_df <- esc[SG_UF == "DF" & TP_SITUACAO_FUNCIONAMENTO == 1]
esc_df[, nome_norm := normalizar_nome(NO_ENTIDADE)]
esc_df[, `:=`(tipo = extrair_tipo(nome_norm), numero = extrair_numeros(nome_norm), resto = extrair_resto(nome_norm))]
cat(sprintf("Escolas ativas no DF (Censo Escolar 2023): %d\n", nrow(esc_df)))

# ---- 5. Casamento por chave dura tipo+numero -------------------------------
tem_numero <- secoes_df$numero != ""
chave_tse <- secoes_df[tem_numero, .(chave_local, tipo, numero, resto, nome_norm)]
chave_esc <- esc_df[numero != "", .(tipo, numero, CO_ENTIDADE, resto, nome_norm)]
cand <- merge(chave_tse, chave_esc, by = c("tipo", "numero"), suffixes = c("_tse", "_esc"), allow.cartesian = TRUE)
cand[, dist_resto := stringdist(resto_tse, resto_esc, method = "jw", p = 0.1)]
setorder(cand, chave_local, dist_resto)
match_numero <- cand[, .SD[1], by = chave_local][, .(chave_local, CO_ENTIDADE, metodo = "tipo_numero")]

# ---- 6. Fuzzy seguro (so texto) para nomes sem numero ----------------------
sem_numero <- secoes_df[numero == ""]
fuzzy_seguro <- data.table(chave_local = character(0), CO_ENTIDADE = integer(0), metodo = character(0))
if (nrow(sem_numero) > 0) {
  dmat <- stringdistmatrix(sem_numero$nome_norm, esc_df$nome_norm, method = "jw", p = 0.1)
  for (i in seq_len(nrow(dmat))) {
    ord <- order(dmat[i, ])
    d1 <- dmat[i, ord[1]]
    d2 <- if (length(ord) >= 2) dmat[i, ord[2]] else Inf
    if (d1 <= 0.08 && (d2 - d1) >= 0.05) {
      fuzzy_seguro <- rbind(fuzzy_seguro, data.table(
        chave_local = sem_numero$chave_local[i], CO_ENTIDADE = esc_df$CO_ENTIDADE[ord[1]],
        metodo = "fuzzy_texto"))
    }
  }
}

todos_matches <- rbind(match_numero, fuzzy_seguro)
cat(sprintf("Casamento total com o Censo Escolar: %d / %d (%.1f%%)\n",
            nrow(todos_matches), nrow(secoes_df), 100 * nrow(todos_matches) / nrow(secoes_df)))

# ---- 7. Monta o endereco final: Censo Escolar quando casou, fallback TSE caso contrario ----
secoes_df <- merge(secoes_df, todos_matches, by = "chave_local", all.x = TRUE)
secoes_df <- merge(secoes_df,
                    esc_df[, .(CO_ENTIDADE, NO_ENTIDADE, DS_ENDERECO, NU_ENDERECO, DS_COMPLEMENTO, NO_BAIRRO, CO_CEP)],
                    by = "CO_ENTIDADE", all.x = TRUE)

fallback <- padronizar_endereco_tse(secoes_df$DS_LOCAL_VOTACAO_ENDERECO)
secoes_df[, `:=`(
  logradouro_final = fifelse(!is.na(CO_ENTIDADE), str_squish(paste(DS_ENDERECO, DS_COMPLEMENTO)), fallback$logradouro),
  numero_final     = fifelse(!is.na(CO_ENTIDADE), NU_ENDERECO, fallback$numero),
  localidade_final = fifelse(!is.na(CO_ENTIDADE), NO_BAIRRO, fallback$localidade),
  cep_final        = fifelse(!is.na(CO_ENTIDADE), as.character(CO_CEP), NA_character_)
)]
secoes_df[, numero_final := fifelse(numero_final %in% c("", "S/N", "0"), NA_character_, numero_final)]

# ---- 8. Geocodificacao com {geocodebr} + celulas H3 nativas ----------------
resultado <- geocode(
  enderecos = secoes_df,
  campos_endereco = definir_campos(
    estado = "SG_UF", municipio = "NM_MUNICIPIO",
    logradouro = "logradouro_final", numero = "numero_final",
    localidade = "localidade_final", cep = "cep_final"
  ),
  resultado_completo = FALSE,  # resultado_completo = TRUE tem bug conhecido na v0.6.4 (coluna "empate")
  resultado_sf = FALSE,
  h3_res = c(6, 7, 8),
  cache = TRUE,
  verboso = TRUE
)
setDT(resultado)

# ---- 9. Controle de qualidade -----------------------------------------------
cat("\nDistribuicao de precisao da geocodificacao (com Censo Escolar):\n")
print(resultado[, .(n = .N, pct = round(100 * .N / nrow(resultado), 1)), by = precisao][order(-n)])

resultado[, h3_confiavel := precisao %in% c("numero", "numero_aproximado", "logradouro", "cep")]
resultado[, casado_censo_escolar := !is.na(CO_ENTIDADE)]

fwrite(resultado, saida_locais)
cat(sprintf("\nTabela de locais (DF) geocodificados salva em: %s\n", saida_locais))

# ---- 10. Une de volta a base completa de secoes do DF (nivel secao) -------
cols_geo <- c("chave_local", "lat", "lon", "precisao", "tipo_resultado", "desvio_metros",
              "endereco_encontrado", "h3_06", "h3_07", "h3_08", "h3_confiavel",
              "casado_censo_escolar", "metodo")

base_secoes_df <- fread(arquivo_votacao, sep = ";", quote = "\"", encoding = "Latin-1")
base_secoes_df <- base_secoes_df[SG_UF == "DF"]
cols_texto_bc <- names(base_secoes_df)[vapply(base_secoes_df, is.character, logical(1))]
base_secoes_df[, (cols_texto_bc) := lapply(.SD, enc2utf8), .SDcols = cols_texto_bc]
base_secoes_df[, chave_local := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = "_")]
base_secoes_df <- merge(base_secoes_df, resultado[, ..cols_geo], by = "chave_local", all.x = TRUE)

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(base_secoes_df, saida_completa)
  cat(sprintf("Base completa de secoes do DF geocodificada salva em: %s\n", saida_completa))
} else {
  fwrite(base_secoes_df, sub("\\.parquet$", ".csv", saida_completa))
}

