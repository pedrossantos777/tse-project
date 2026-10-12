# ==========================================================================
# Geolocalizacao das secoes de SP presentes em secoes_tarcisio_lula,
# usando geobr::read_polling_places() -- mesma abordagem validada em
# tse_project/hexagon.R (DF, 2022): geometria oficial do local de votacao,
# sem geocodificacao de endereco.
# ==========================================================================
# depende de dfsp_gov e secoes_tarcisio_lula ja no ambiente (scripts
# anteriores: exploratory_export_to_parquet.R e secoes_lula_tarcisio.R)

suppressMessages({
  library(data.table)
  library(sf)
  library(h3jsr)
  library(geobr)
  library(arrow)
})

resolucoes <- c(7, 8, 9)
chave_secao <- c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO")

# ---- 1. NR_LOCAL_VOTACAO de cada secao de interesse -------------------------
# secoes_tarcisio_lula nao carrega NR_LOCAL_VOTACAO (foi agregado so pela
# chave da secao); cada secao pertence a exatamente 1 local de votacao, entao
# recuperamos o local em dfsp_gov e juntamos so nas secoes de interesse.
locais_secoes <- unique(as.data.table(dfsp_gov)[, c(chave_secao, "NR_LOCAL_VOTACAO"), with = FALSE])

secoes_alvo <- merge(
  as.data.table(secoes_tarcisio_lula),
  locais_secoes,
  by = chave_secao,
  all.x = TRUE
)

n_sem_local <- sum(is.na(secoes_alvo$NR_LOCAL_VOTACAO))
cat(sprintf("Secoes de secoes_tarcisio_lula sem NR_LOCAL_VOTACAO: %d / %d\n",
            n_sem_local, nrow(secoes_alvo)))

# ---- 2. Geometria oficial dos locais de votacao de SP (geobr) --------------
geo_pool <- read_polling_places(
  year = 2026, code_muni = "SP", output = "sf",
  showProgress = FALSE, cache = TRUE, verbose = FALSE
)
cat(sprintf("Locais de votacao (linhas) retornados pelo geobr para SP: %d\n", nrow(geo_pool)))

# geobr entrega SIRGAS 2000 (EPSG:4674); H3 espera lon/lat em WGS84 (EPSG:4326)
# -- reprojeta e extrai lon/lat ANTES de virar data.table, para nao carregar a
# coluna-lista de geometria (sf) pelos merges de data.table a seguir.
geo_pool <- st_transform(geo_pool, crs = 4326)
coords <- st_coordinates(geo_pool)
geo_pool <- setDT(st_drop_geometry(geo_pool))
geo_pool[, `:=`(lon = coords[, "X"], lat = coords[, "Y"])]

# ---- 3. Deduplica geo_pool para 1 linha por (zona, local) ------------------
# a geometria e por LOCAL de votacao -- varias secoes compartilham o mesmo
# local, entao deduplicamos antes de juntar (mesma chave validada no DF).
setorder(geo_pool, nr_zona, nr_local_votacao_original)
locais_geo <- geo_pool[!duplicated(geo_pool, by = c("nr_zona", "nr_local_votacao_original")),
                        .(nr_zona, nr_local_votacao_original, lon, lat,
                          coords_source, precisao_geocodebr, desvio_metros_geocodebr)]

# ---- 4. Junta as secoes de interesse com a geometria deduplicada ----------
# left join: mantem todas as secoes de secoes_tarcisio_lula, mesmo as que o
# geobr nao cobriu (ficam com lon/lat NA, reportadas no QC abaixo).
secoes_alvo_geo <- merge(
  secoes_alvo, locais_geo,
  by.x = c("NR_ZONA", "NR_LOCAL_VOTACAO"),
  by.y = c("nr_zona", "nr_local_votacao_original"),
  all.x = TRUE
)

n_sem_geo <- sum(is.na(secoes_alvo_geo$lon))
cat(sprintf("Secoes de interesse sem geometria no geobr: %d / %d (%.1f%%)\n",
            n_sem_geo, nrow(secoes_alvo_geo), 100 * n_sem_geo / nrow(secoes_alvo_geo)))

# ---- 5. Hexagono H3 de cada secao, nas resolucoes 7, 8 e 9 -----------------
com_geo <- secoes_alvo_geo[!is.na(lon)]
hex <- point_to_cell(data.frame(lon = com_geo$lon, lat = com_geo$lat),
                      res = resolucoes, simple = TRUE)
setDT(hex)
setnames(hex, sprintf("h3_resolution_%d", resolucoes), sprintf("h3_res%d", resolucoes))

secoes_tarcisio_lula_geo <- cbind(com_geo, hex)

arrow::write_parquet(secoes_tarcisio_lula_geo,
                      "dados/secoes_tarcisio_lula_geo.parquet")
cat(sprintf("\nsecoes_tarcisio_lula_geo salva: %d secoes geolocalizadas (de %d).\n",
            nrow(secoes_tarcisio_lula_geo), nrow(secoes_alvo_geo)))
