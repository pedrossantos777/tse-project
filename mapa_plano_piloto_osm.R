# ==========================================================================
# Mapa coropletico interativo do Plano Piloto (2o turno, Presidente 2022),
# resolucoes H3 8 e 9, sobre fundo OpenStreetMap (via {mapgl} / MapLibre GL).
# Vermelho = Lula > Bolsonaro na celula | Azul = Bolsonaro > Lula
# ==========================================================================
# Mesma base do mapa de satelite, trocando so o estilo do mapa base para um
# estilo vetorial derivado do OpenStreetMap (OpenFreeMap "liberty" -- dados
# OSM, gratuito, sem token, mais nitido em qualquer zoom que tiles raster).
# ==========================================================================

suppressMessages({
  library(data.table)
  library(arrow)
  library(h3jsr)
  library(sf)
  library(mapgl)
  library(htmlwidgets)
})

diretorio_base <- "C:/Users/Espada/Documents/R/tse_project"

# ---- 1. bbox aproximado do "Plano Piloto" (nucleo urbano central) ----------
xlim_pp <- c(-47.955, -47.855)
ylim_pp <- c(-15.870, -15.715)

# ---- 2. carrega secoes do DF geocodificadas e calcula H3 (res 8 e 9) ------
dt <- as.data.table(read_parquet(file.path(diretorio_base, "dados", "secoes_df_geo.parquet")))
locais_unicos <- unique(dt[!is.na(lat), .(chave_local, lat, lon)], by = "chave_local")
locais_unicos[, `:=`(
  h3_08 = point_to_cell(data.frame(lon = lon, lat = lat), res = 8, simple = TRUE),
  h3_09 = point_to_cell(data.frame(lon = lon, lat = lat), res = 9, simple = TRUE)
)]
dt[, h3_08 := NULL]
dt <- merge(dt, locais_unicos[, .(chave_local, lat2 = lat, lon2 = lon, h3_08, h3_09)], by = "chave_local", all.x = TRUE)

votos <- dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22) & h3_confiavel == TRUE &
              lon2 >= xlim_pp[1] & lon2 <= xlim_pp[2] & lat2 >= ylim_pp[1] & lat2 <= ylim_pp[2]]
cat(sprintf("Locais unicos no Plano Piloto (confiaveis): %d\n", uniqueN(votos$chave_local)))

# ---- 3. agrega por celula H3 numa resolucao e devolve sf com propriedades --
agregar_h3 <- function(col_h3) {
  agg <- votos[, .(votos = sum(QT_VOTOS)), by = c("NR_VOTAVEL", col_h3)]
  setnames(agg, col_h3, "h3")
  agg_wide <- dcast(agg, h3 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
  setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))
  agg_wide[, `:=`(
    vencedor = fifelse(lula > bolsonaro, "Lula", fifelse(bolsonaro > lula, "Bolsonaro", "Empate")),
    total = lula + bolsonaro,
    pct_lula = round(100 * lula / (lula + bolsonaro), 1)
  )]
  poligonos <- cell_to_polygon(agg_wide$h3, simple = FALSE)
  merge(poligonos, agg_wide, by.x = "h3_address", by.y = "h3")
}

hex_08 <- agregar_h3("h3_08")
hex_09 <- agregar_h3("h3_09")

cat("\nResolucao 8 -- celulas por vencedor:\n"); print(table(hex_08$vencedor))
cat("\nResolucao 9 -- celulas por vencedor:\n"); print(table(hex_09$vencedor))

# ---- 4. mapa MapLibre com fundo OpenStreetMap (OpenFreeMap "liberty") ------
centro <- c(mean(xlim_pp), mean(ylim_pp))
cores_expr <- match_expr(column = "vencedor", values = c("Lula", "Bolsonaro", "Empate"),
                          stops = c("#c0392b", "#2563eb", "#9ca3af"), default = "#9ca3af")

mapa <- maplibre(style = openfreemap_style("liberty"), center = centro, zoom = 13.3, pitch = 0) |>
  fit_bounds(c(xlim_pp[1], ylim_pp[1], xlim_pp[2], ylim_pp[2]), animate = FALSE) |>
  add_source(id = "hex08_src", data = hex_08) |>
  add_fill_layer(
    id = "hex_08", source = "hex08_src",
    fill_color = cores_expr, fill_opacity = 0.55, fill_outline_color = "white",
    tooltip = "vencedor"
  ) |>
  add_source(id = "hex09_src", data = hex_09) |>
  add_fill_layer(
    id = "hex_09", source = "hex09_src",
    fill_color = cores_expr, fill_opacity = 0.55, fill_outline_color = "white",
    tooltip = "vencedor", visibility = "none"
  ) |>
  add_layers_control(layers = c("hex_08", "hex_09"), position = "top-right") |>
  add_categorical_legend(
    legend_title = "Mais votado (2º turno)",
    values = c("Lula", "Bolsonaro"),
    colors = c("#c0392b", "#2563eb"),
    position = "bottom-left"
  ) |>
  add_navigation_control() |>
  add_fullscreen_control()

# ---- 5. salva HTML interativo -----------------------------------------------
arquivo_html <- file.path(diretorio_base, "mapa_plano_piloto_osm.html")
saveWidget(mapa, arquivo_html, selfcontained = TRUE, title = "Plano Piloto - Presidente 2022 (2o turno)")
cat(sprintf("\nMapa interativo salvo em: %s\n", arquivo_html))

# ---- 6. tenta gerar um PNG estatico p/ conferencia rapida ------------------
arquivo_png <- file.path(diretorio_base, "mapa_plano_piloto_osm.png")
ok_png <- tryCatch({
  save_map(mapa, arquivo_png, width = 1400, height = 1000, delay = 3)
  TRUE
}, error = function(e) { cat("Aviso: nao foi possivel gerar PNG automatico:", conditionMessage(e), "\n"); FALSE })
if (ok_png) cat(sprintf("PNG de conferencia salvo em: %s\n", arquivo_png))
