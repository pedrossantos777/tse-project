# ==========================================================================
# Mapa interativo (MapLibre GL via {mapgl}) das secoes de SP em que Tarcisio
# (Governador) e Lula (Presidente) foram os mais votados simultaneamente.
# Base OpenStreetMap + hexagonos H3 (res. 7) sobrepostos, com legenda por
# quebras naturais (Jenks). Gera 1 HTML interativo + 3 PNGs estaticos
# (RMSP, Baixada Santista, interior paulista).
# ==========================================================================
# le dados/secoes_tarcisio_lula_geo.parquet (gerado por
# geolocalizar_secoes_tarcisio_lula.R) -- nao depende de objetos em memoria.
#
# Nota: uma tentativa anterior usou uma camada raster de satelite (Esri World
# Imagery) sobre um estilo MapLibre "em branco" (sources/layers vazios) -- um
# estilo em branco faz o mapa nunca disparar o evento de carregamento e
# save_map() trava. Por isso este script usa so o estilo default do maplibre
# (ja baseado em OpenStreetMap/CARTO). Segundo problema encontrado: passar
# st_bbox() (vetor NOMEADO c(xmin=,ymin=,xmax=,ymax=)) direto para fit_bounds()
# tambem trava o save_map (serializa errado para JSON) -- por isso
# as.numeric(st_bbox(...)) em toda chamada de fit_bounds abaixo.

suppressMessages({
  library(dplyr)
  library(sf)
  library(h3jsr)
  library(arrow)
  library(mapgl)
  library(RColorBrewer)
})

secoes <- read_parquet("dados/secoes_tarcisio_lula_geo.parquet")

# geobr usa lon/lat = c(-1, -1) como sentinela para locais sem coordenada --
# 16 das 12914 linhas; removidas antes de agregar (ver mapa_tarcisio_lula_res7.R).
secoes <- secoes |> filter(!(lon == -1 & lat == -1))

# ---- 1. Agrega por hexagono H3 res. 7 --------------------------------------
por_hex <- secoes |>
  summarise(
    n_secoes   = n(),
    votos_gov  = sum(QT_VOTOS_GOV),
    votos_pres = sum(QT_VOTOS_PRES),
    .by = h3_res7
  )

hex_geom <- cell_to_polygon(por_hex$h3_res7, simple = FALSE)
hex_geom <- st_sf(h3_res7 = por_hex$h3_res7, geometry = st_geometry(hex_geom))
mapa_dados <- left_join(hex_geom, por_hex, by = "h3_res7") |> st_transform(4326)

# ---- 2. Regioes de referencia (geobr) ---------------------------------------
metro_areas <- geobr::read_metro_area(year = 2018, showProgress = FALSE)
rmsp <- st_transform(metro_areas |> filter(name_metro == "Rm São Paulo"), 4326)
baixada_santista <- st_transform(metro_areas |> filter(name_metro == "Rm Baixada Santista"), 4326)

mapa_dados$dentro_rmsp <- lengths(st_intersects(mapa_dados, rmsp)) > 0
mapa_dados$dentro_baixada <- lengths(st_intersects(mapa_dados, baixada_santista)) > 0
mapa_dados$regiao <- case_when(
  mapa_dados$dentro_rmsp    ~ "RMSP",
  mapa_dados$dentro_baixada ~ "Baixada Santista",
  TRUE                      ~ "Interior"
)

# bbox do interior = so os hexagonos de fato fora da RMSP/Baixada (recorte
# mais justo do que o estado inteiro, mesma logica usada para RMSP/Baixada)
bbox_interior <- st_bbox(mapa_dados[mapa_dados$regiao == "Interior", ])

# ---- 3. Classificacao por quebras naturais (Jenks) --------------------------
# uma UNICA classificacao para o dataset inteiro -- assim a legenda e as
# cores sao as mesmas nos 3 recortes (comparaveis entre regioes).
paleta_blues <- RColorBrewer::brewer.pal(5, "Blues")
jk_class <- step_jenks(data = mapa_dados, column = "n_secoes", n = 5, colors = paleta_blues)

# ---- 4. Mapa base: OpenStreetMap (estilo default do maplibre) + hexagonos --
construir_mapa <- function() {
  maplibre(projection = "mercator") |>
    add_fill_layer(
      id = "hexagonos",
      source = mapa_dados,
      fill_color = jk_class$expression,
      fill_opacity = 0.75,
      fill_outline_color = "white",
      tooltip = "{n_secoes} seções · Tarcísio: {votos_gov} votos · Lula: {votos_pres} votos"
    ) |>
    add_legend(
      legend_title = "Seções\n(Tarcísio + Lula)",
      values = get_legend_labels(jk_class),
      colors = get_legend_colors(jk_class),
      type = "categorical",
      position = "top-left"
    ) |>
    add_scale_control(position = "bottom-left") |>
    add_fullscreen_control(position = "top-right") |>
    add_navigation_control(position = "top-right")
}

# ---- 5. HTML interativo (estado inteiro) -------------------------------------
mapa_html <- construir_mapa() |> fit_bounds(as.numeric(st_bbox(mapa_dados)), animate = FALSE)
htmlwidgets::saveWidget(mapa_html, "dados/mapa_tarcisio_lula_osm.html", selfcontained = TRUE)
cat("HTML salvo em dados/mapa_tarcisio_lula_osm.html\n")

# ---- 6. Prints estaticos por regiao (RMSP, Baixada Santista, interior) -----
save_map(construir_mapa() |> fit_bounds(as.numeric(st_bbox(rmsp)), animate = FALSE),
         "dados/mapa_tarcisio_lula_osm_rmsp.png", width = 1100, height = 850)
cat("PNG salvo: dados/mapa_tarcisio_lula_osm_rmsp.png\n")

save_map(construir_mapa() |> fit_bounds(as.numeric(st_bbox(baixada_santista)), animate = FALSE),
         "dados/mapa_tarcisio_lula_osm_baixada_santista.png", width = 1100, height = 850)
cat("PNG salvo: dados/mapa_tarcisio_lula_osm_baixada_santista.png\n")

save_map(construir_mapa() |> fit_bounds(as.numeric(bbox_interior), animate = FALSE),
         "dados/mapa_tarcisio_lula_osm_interior.png", width = 1100, height = 850)
cat("PNG salvo: dados/mapa_tarcisio_lula_osm_interior.png\n")
