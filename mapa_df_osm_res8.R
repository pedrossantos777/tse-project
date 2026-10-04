# ==========================================================================
# Mapa coropletico interativo do DF INTEIRO (2o turno, Presidente 2022),
# resolucao H3 8, sobre fundo OpenStreetMap (via {mapgl} / MapLibre GL).
# Vermelho = Lula > Bolsonaro na celula | Azul = Bolsonaro > Lula
# ==========================================================================
# Mesma logica dos mapas do Plano Piloto, mas sem o recorte de bbox: usa
# todos os locais de votacao do DF com geocodificacao confiavel (nivel
# "cep"/"localidade" -- os de nivel "municipio" continuam excluidos, ver
# geocodificar_secoes_df.R).
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

# ---- 1. carrega secoes do DF geocodificadas e calcula H3 (res 8) ----------
dt <- as.data.table(read_parquet(file.path(diretorio_base, "dados", "secoes_df_geo.parquet")))
locais_unicos <- unique(dt[!is.na(lat), .(chave_local, lat, lon)], by = "chave_local")
locais_unicos[, h3_08b := point_to_cell(data.frame(lon = lon, lat = lat), res = 8, simple = TRUE)]
dt[, h3_08 := NULL]
dt <- merge(dt, locais_unicos[, .(chave_local, h3_08 = h3_08b)], by = "chave_local", all.x = TRUE)

votos <- dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22) & h3_confiavel == TRUE]
cat(sprintf("Locais unicos no DF (confiaveis): %d\n", uniqueN(votos$chave_local)))

pct_confiavel <- 100 * sum(votos$QT_VOTOS) / sum(dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]$QT_VOTOS)
cat(sprintf("Votos (Lula+Bolsonaro, 2o turno) representados: %.1f%%\n", pct_confiavel))

# ---- 2. agrega por celula H3 res.8 -----------------------------------------
agg <- votos[, .(votos = sum(QT_VOTOS)), by = .(h3_08, NR_VOTAVEL)]
agg_wide <- dcast(agg, h3_08 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))
agg_wide[, `:=`(
  vencedor = fifelse(lula > bolsonaro, "Lula", fifelse(bolsonaro > lula, "Bolsonaro", "Empate")),
  total = lula + bolsonaro,
  pct_lula = round(100 * lula / (lula + bolsonaro), 1)
)]
cat("\nResolucao 8 (DF inteiro) -- celulas por vencedor:\n"); print(table(agg_wide$vencedor))

hex_08 <- cell_to_polygon(agg_wide$h3_08, simple = FALSE)
hex_08 <- merge(hex_08, agg_wide, by.x = "h3_address", by.y = "h3_08")

# ---- 3. limites do DF (p/ enquadrar o mapa) --------------------------------
df_boundary <- tryCatch(read_state(code_state = "DF", year = 2020, showProgress = FALSE), error = function(e) NULL)
bb <- if (!is.null(df_boundary)) st_bbox(df_boundary) else st_bbox(hex_08)
centro <- c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])))

# ---- 4. mapa MapLibre com fundo OpenStreetMap ------------------------------
cores_expr <- match_expr(column = "vencedor", values = c("Lula", "Bolsonaro", "Empate"),
                          stops = c("#c0392b", "#2563eb", "#9ca3af"), default = "#9ca3af")

# nota: usamos center+zoom fixos em vez de fit_bounds() -- com bbox grande
# (DF inteiro) o fit_bounds() deixava o save_map()/chromote travar esperando
# a camera "assentar" antes do screenshot (timeout). Zoom 9.6 cobre o DF
# inteiro com uma margem parecida com o que o fit_bounds() produziria.
mapa <- maplibre(style = openfreemap_style("liberty"), center = centro, zoom = 9.6, pitch = 0) |>
  add_source(id = "hex08_src", data = hex_08) |>
  add_fill_layer(
    id = "hex_08", source = "hex08_src",
    fill_color = cores_expr, fill_opacity = 0.55, fill_outline_color = "white",
    tooltip = "Vencedor: {vencedor}<br>Lula: {lula} votos<br>Bolsonaro: {bolsonaro} votos"
  ) |>
  add_categorical_legend(
    legend_title = "Mais votado no hexágono (2º turno)",
    values = c("Lula", "Bolsonaro"),
    colors = c("#c0392b", "#2563eb"),
    position = "bottom-left"
  ) |>
  add_navigation_control() |>
  add_fullscreen_control()

# ---- 5. salva HTML interativo -----------------------------------------------
arquivo_html <- file.path(diretorio_base, "mapa_df_osm_res8.html")
saveWidget(mapa, arquivo_html, selfcontained = TRUE, title = "DF - Presidente 2022 (2o turno) - H3 res 8")
cat(sprintf("\nMapa interativo salvo em: %s\n", arquivo_html))

# ---- 6. PNG estatico p/ conferencia -----------------------------------------
arquivo_png <- file.path(diretorio_base, "mapa_df_osm_res8.png")
ok_png <- tryCatch({
  save_map(mapa, arquivo_png, width = 1000, height = 800, delay = 5)
  TRUE
}, error = function(e) { cat("Aviso: nao foi possivel gerar PNG automatico:", conditionMessage(e), "\n"); FALSE })
if (ok_png) cat(sprintf("PNG de conferencia salvo em: %s\n", arquivo_png))
