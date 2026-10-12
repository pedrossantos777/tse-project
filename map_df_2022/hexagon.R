# ==========================================================================
# Hexagonos H3 (res. 7, 8 e 9) para as secoes eleitorais do DF (TSE 2022)
# a partir da geometria oficial do geobr::read_polling_places()
# ==========================================================================
# Por que este script muda de abordagem em relacao a geocodificar_secoes_df.R:
#   Antes, sem coordenadas prontas, foi preciso GEOCODIFICAR o endereco livre
#   do TSE (parsing de logradouro/numero + {geocodebr} + casamento com o
#   Censo Escolar) para so entao localizar cada local de votacao no espaco.
#   Agora usamos geobr::read_polling_places(), que ja entrega um ponto
#   (geometry sf) pronto para cada local de votacao, combinando a coordenada
#   original do TSE com uma geocodificacao de reforco (coluna coords_source
#   informa qual das duas foi usada). Com o ponto em maos, achar o hexagono
#   H3 de cada linha e so um point-in-cell (h3jsr::point_to_cell) -- nao
#   ha mais casamento fuzzy de nome nem parsing de endereco.
#
# Chave de juncao: geo_pool tem 1 LINHA POR SECAO dentro do local de votacao
# (mesmo local+zona aparece varias vezes, uma por secao), mas a coordenada
# e IDENTICA em todas as linhas de um mesmo (nr_zona, nr_local_votacao) --
# verificado: 0 grupos com mais de 1 coordenada distinta. Por isso deduplicamos
# geo_pool por (nr_zona, nr_local_votacao_original) ANTES de juntar, exatamente
# como geocodificar_secoes_df.R fazia com os enderecos (geocodifica so os
# locais unicos, depois espalha o resultado de volta pelas secoes).
# ==========================================================================

suppressMessages({
  library(data.table)
  library(sf)
  library(h3jsr)
  library(geobr)
  library(arrow)
})

diretorio_base <- "C:/Users/Espada/Documents/R/tse_project"
arquivo_votacao <- file.path(diretorio_base, "dados", "votacao_secao_2022_BR.csv")
saida_secoes    <- file.path(diretorio_base, "dados", "secoes_df_geo.parquet")
saida_votos_hex <- file.path(diretorio_base, "dados", "votos_por_hexagono.parquet")

resolucoes <- c(7, 8, 9)

# ---- 1. Le a base nacional de votacao por secao e filtra o DF --------------
# select= evita carregar colunas que nao usamos (a base nacional tem ~1.6 GB);
# como a base tem 1 linha por secao x cargo x candidato, isso ja e o nivel de
# granularidade final -- nao ha o que deduplicar aqui.
cols_votacao <- c("DT_GERACAO", "HH_GERACAO", "ANO_ELEICAO", "CD_TIPO_ELEICAO",
                   "NM_TIPO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "DS_ELEICAO",
                   "DT_ELEICAO", "TP_ABRANGENCIA", "SG_UF", "SG_UE", "NM_UE",
                   "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO",
                   "CD_CARGO", "DS_CARGO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS",
                   "NR_LOCAL_VOTACAO", "SQ_CANDIDATO", "NM_LOCAL_VOTACAO",
                   "DS_LOCAL_VOTACAO_ENDERECO")

secoes_df <- fread(arquivo_votacao, select = cols_votacao, sep = ";", quote = "\"",
                    encoding = "Latin-1")
cols_texto <- names(secoes_df)[vapply(secoes_df, is.character, logical(1))]
secoes_df[, (cols_texto) := lapply(.SD, enc2utf8), .SDcols = cols_texto]
secoes_df <- secoes_df[SG_UF == "DF"]
cat(sprintf("Linhas de votacao do DF (secao x cargo x candidato): %d\n", nrow(secoes_df)))

# ---- 2. Locais de votacao unicos (chave: zona + numero do local) -----------
# A geometria e por LOCAL de votacao, nao por linha de voto -- extraimos os
# pares unicos para juntar com o geobr so uma vez por local (mais barato e
# evita distorcer a contagem de "locais" por causa das repeticoes de cargo).
locais_unicos <- unique(secoes_df[, .(NR_ZONA, NR_LOCAL_VOTACAO)])
cat(sprintf("Locais de votacao unicos no DF: %d\n", nrow(locais_unicos)))

# ---- 3. Geometria oficial dos locais de votacao (geobr) --------------------
geo_pool <- read_polling_places(
  year = 2022, code_muni = "DF", output = "sf",
  showProgress = FALSE, cache = TRUE, verbose = FALSE
)
cat(sprintf("Locais de votacao (linhas) retornados pelo geobr: %d\n", nrow(geo_pool)))

# ---- 4. Deduplica geo_pool para 1 linha por (zona, local) -------------------
# geobr entrega SIRGAS 2000 (EPSG:4674); H3 espera lon/lat em WGS84 (EPSG:4326)
# -- reprojetamos ANTES de extrair lon/lat para nao carregar a classe sf (e a
# coluna-lista de geometria) pelos merges de data.table daqui em diante, que
# nao sabem lidar com colunas sf (erro "Not compatible with requested type").
geo_pool <- st_transform(geo_pool, crs = 4326)
coords <- st_coordinates(geo_pool)
geo_pool <- setDT(st_drop_geometry(geo_pool))
geo_pool[, `:=`(lon = coords[, "X"], lat = coords[, "Y"])]

setorder(geo_pool, nr_zona, nr_local_votacao_original)
locais_geo <- geo_pool[!duplicated(geo_pool, by = c("nr_zona", "nr_local_votacao_original")),
                        .(nr_zona, nr_local_votacao_original, lon, lat,
                          coords_source, precisao_geocodebr, desvio_metros_geocodebr)]
cat(sprintf("Locais unicos com geometria (geobr, deduplicado): %d\n", nrow(locais_geo)))

# ---- 5. Junta os locais unicos do DF com a geometria deduplicada -----------
# left join: mantem TODOS os locais do DF, mesmo os que o geobr nao cobriu
# (ficam com lon/lat NA e sao reportados no QC abaixo).
locais_unicos <- merge(
  locais_unicos, locais_geo,
  by.x = c("NR_ZONA", "NR_LOCAL_VOTACAO"),
  by.y = c("nr_zona", "nr_local_votacao_original"),
  all.x = TRUE
)

n_sem_geo <- sum(is.na(locais_unicos$lon))
cat(sprintf("Locais do DF sem geometria no geobr: %d / %d (%.1f%%)\n",
            n_sem_geo, nrow(locais_unicos), 100 * n_sem_geo / nrow(locais_unicos)))

# ---- 6. Hexagono H3 de cada local unico, nas resolucoes 7, 8 e 9 -----------
# 1 chamada com res = c(7,8,9) e mais barata que 3 chamadas separadas
# (h3jsr reaproveita a mesma varredura de pontos para as 3 resolucoes).
com_geo <- locais_unicos[!is.na(lon)]
hex <- point_to_cell(data.frame(lon = com_geo$lon, lat = com_geo$lat),
                      res = resolucoes, simple = TRUE)
setDT(hex)
setnames(hex, sprintf("h3_resolution_%d", resolucoes), sprintf("h3_res%d", resolucoes))
locais_hex <- cbind(com_geo[, .(NR_ZONA, NR_LOCAL_VOTACAO)], hex)

# ---- 7. Espalha os hexagonos de volta para a base completa (nivel secao) ---
# "encontre os hexagonos de cada linha da base secoes_df_geo": cada linha de
# voto herda o hexagono do LOCAL a que pertence.
secoes_df_geo <- merge(secoes_df, locais_hex, by = c("NR_ZONA", "NR_LOCAL_VOTACAO"), all.x = TRUE)

cat("\nCobertura de hexagono por linha de voto:\n")
for (r in resolucoes) {
  col <- sprintf("h3_res%d", r)
  n_ok <- sum(!is.na(secoes_df_geo[[col]]))
  cat(sprintf("  res %d: %d / %d linhas (%.1f%%)\n",
              r, n_ok, nrow(secoes_df_geo), 100 * n_ok / nrow(secoes_df_geo)))
}

arrow::write_parquet(secoes_df_geo, saida_secoes)
cat(sprintf("\nsecoes_df_geo salva em: %s\n", saida_secoes))

# ---- 8. Agrega votos por candidato dentro de cada hexagono ------------------
# "agregue os numeros de votos de cada candidato por hexagono": soma QT_VOTOS
# por hexagono, mantendo turno/cargo/candidato separados (um DF em branco por
# resolucao seria dificil de comparar; formato longo com coluna `resolucao`
# deixa as 3 resolucoes numa unica tabela, filtravel por quem for usar).
agregar_uma_resolucao <- function(res) {
  col <- sprintf("h3_res%d", res)
  agg <- secoes_df_geo[!is.na(get(col)),
    .(votos = sum(QT_VOTOS)),
    by = c(col, "NR_TURNO", "CD_CARGO", "DS_CARGO", "NR_VOTAVEL", "NM_VOTAVEL")]
  setnames(agg, col, "h3")
  agg[, resolucao := res]
  agg
}
votos_por_hexagono <- rbindlist(lapply(resolucoes, agregar_uma_resolucao))
setcolorder(votos_por_hexagono, c("resolucao", "h3", "NR_TURNO", "CD_CARGO", "DS_CARGO",
                                   "NR_VOTAVEL", "NM_VOTAVEL", "votos"))

arrow::write_parquet(votos_por_hexagono, saida_votos_hex)
cat(sprintf("votos_por_hexagono salva em: %s\n", saida_votos_hex))

cat("\nHexagonos distintos por resolucao:\n")
print(votos_por_hexagono[, .(hexagonos = uniqueN(h3)), by = resolucao])
