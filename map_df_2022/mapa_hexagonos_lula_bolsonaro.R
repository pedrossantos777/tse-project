# ==========================================================================
# Mapa interativo do DF (Presidente 2022, 2o turno) por hexagono H3,
# nas resolucoes 7, 8 e 9, sobre fundo OpenStreetMap (via {mapgl}/MapLibre).
# Vermelho = Lula teve mais votos que Bolsonaro no hexagono
# Azul     = Bolsonaro teve mais votos que Lula no hexagono
# ==========================================================================
# Ponto de partida: hexagon.R ja calculou os hexagonos (h3_res7/8/9) e a
# agregacao de votos por candidato/hexagono/resolucao (ver MEMORIA.md, secao
# "hexagon.R"). Este script SO plota -- nao recalcula hexagono nem agrega
# voto de novo, para nao duplicar logica ja validada.
#
# As 3 resolucoes viram 3 camadas no MESMO mapa, com um controle de camadas
# (add_layers_control) para ligar/desligar cada uma -- res. 8 comeca visivel
# (mesmo padrao ja usado em mapa_df_osm_res8.R para o DF inteiro), res. 7 e
# 9 comecam ocultas mas ficam a um clique de distancia.
# ==========================================================================

suppressMessages({
  library(data.table)
  library(arrow)
  library(h3jsr)
  library(sf)
  library(mapgl)
  library(htmlwidgets)
  library(geobr)
})

diretorio_base <- "C:/Users/Espada/Documents/R/tse_project"
resolucoes <- c(7, 8, 9)
cores <- c(Lula = "#c0392b", Bolsonaro = "#2563eb", Empate = "#9ca3af")

# ---- 1. Carrega os votos ja agregados por hexagono (hexagon.R) ------------
votos_hex <- as.data.table(read_parquet(file.path(diretorio_base, "dados", "votos_por_hexagono.parquet")))

# Presidente, 2o turno, so Lula (13) x Bolsonaro (22) -- mesmo recorte usado
# nos mapas presidenciais anteriores do projeto.
votos_hex <- votos_hex[CD_CARGO == 1 & NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]

# % dos votos (Lula+Bolsonaro, 2o turno) representado nos hexagonos: o
# denominador vem de secoes_df_geo (inclui as linhas SEM hexagono, ver
# limitacao dos 10,4% de locais sem geometria no geobr, documentada no
# MEMORIA.md); o numerador e igual em qualquer resolucao (mesmas linhas tem
# ou nao tem h3 nas 3 resolucoes simultaneamente).
secoes <- as.data.table(read_parquet(file.path(diretorio_base, "dados", "secoes_df_geo.parquet")))
total_votos <- sum(secoes[CD_CARGO == 1 & NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]$QT_VOTOS)
pct_representado <- 100 * votos_hex[resolucao == 8, sum(votos)] / total_votos
cat(sprintf("Votos (Lula+Bolsonaro, 2o turno) representados nos hexagonos: %.1f%%\n", pct_representado))

# ---- 2. Para cada resolucao: tabela larga (lula/bolsonaro) + poligonos ----
montar_hexagonos <- function(res) {
  agg <- votos_hex[resolucao == res]
  agg_wide <- dcast(agg, h3 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
  setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))
  agg_wide[, `:=`(
    vencedor = fifelse(lula > bolsonaro, "Lula", fifelse(bolsonaro > lula, "Bolsonaro", "Empate")),
    total = lula + bolsonaro
  )]
  poligonos <- cell_to_polygon(agg_wide$h3, simple = FALSE)
  poligonos <- merge(poligonos, agg_wide, by.x = "h3_address", by.y = "h3")
  cat(sprintf("Resolucao %d: %d hexagonos (%s)\n", res, nrow(poligonos),
              paste(sprintf("%s=%d", names(table(poligonos$vencedor)), table(poligonos$vencedor)), collapse = ", ")))
  poligonos
}
hex_por_resolucao <- setNames(lapply(resolucoes, montar_hexagonos), sprintf("res%d", resolucoes))

# ---- 3. Limites do DF (enquadramento do mapa) ------------------------------
df_boundary <- tryCatch(read_state(code_state = "DF", year = 2020, showProgress = FALSE), error = function(e) NULL)
bb <- if (!is.null(df_boundary)) st_bbox(df_boundary) else st_bbox(hex_por_resolucao$res8)
centro <- c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])))

# ---- 4. Mapa MapLibre com fundo OpenStreetMap (OpenFreeMap "liberty") -----
cores_expr <- match_expr(column = "vencedor", values = c("Lula", "Bolsonaro", "Empate"),
                          stops = unname(cores[c("Lula", "Bolsonaro", "Empate")]), default = cores[["Empate"]])

# center+zoom fixos (em vez de fit_bounds()) -- com bbox grande (DF inteiro)
# o fit_bounds() trava o save_map()/chromote esperando a camera "assentar"
# antes do screenshot (mesma observacao registrada em mapa_df_osm_res8.R).
mapa <- maplibre(style = openfreemap_style("liberty"), center = centro, zoom = 9.6, pitch = 0)

for (res in resolucoes) {
  poligonos <- hex_por_resolucao[[sprintf("res%d", res)]]
  mapa <- mapa |>
    add_source(id = sprintf("hex_src_%d", res), data = poligonos) |>
    add_fill_layer(
      id = sprintf("hex_res%d", res), source = sprintf("hex_src_%d", res),
      fill_color = cores_expr, fill_opacity = 0.6, fill_outline_color = "white",
      visibility = if (res == 8) "visible" else "none",
      tooltip = "Vencedor: {vencedor}<br>Lula: {lula} votos<br>Bolsonaro: {bolsonaro} votos"
    )
}

mapa <- mapa |>
  add_layers_control(
    position = "top-right",
    layers = setNames(sprintf("hex_res%d", resolucoes), sprintf("Resolução %d", resolucoes))
  ) |>
  add_categorical_legend(
    legend_title = "Mais votado no hexágono (2º turno)",
    values = c("Lula", "Bolsonaro"),
    colors = unname(cores[c("Lula", "Bolsonaro")]),
    position = "bottom-left"
  ) |>
  add_navigation_control() |>
  add_fullscreen_control()

# ---- 5. Salva HTML interativo -----------------------------------------------
arquivo_html <- file.path(diretorio_base, "mapa_hexagonos_lula_bolsonaro.html")
saveWidget(mapa, arquivo_html, selfcontained = TRUE, title = "DF - Presidente 2022 (2o turno) - Hexagonos H3 (res. 7, 8, 9)")
cat(sprintf("\nMapa interativo salvo em: %s\n", arquivo_html))

# ---- 6. PNG estatico p/ conferencia (camada visivel: res. 8) --------------
arquivo_png <- file.path(diretorio_base, "mapa_hexagonos_lula_bolsonaro.png")
ok_png <- tryCatch({
  save_map(mapa, arquivo_png, width = 1000, height = 800, delay = 5)
  TRUE
}, error = function(e) { cat("Aviso: nao foi possivel gerar PNG automatico:", conditionMessage(e), "\n"); FALSE })
if (ok_png) cat(sprintf("PNG de conferencia salvo em: %s\n", arquivo_png))
