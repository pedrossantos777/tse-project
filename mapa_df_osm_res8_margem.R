# ==========================================================================
# Mapa coropletico interativo do DF INTEIRO (2o turno, Presidente 2022),
# resolucao H3 8, sobre fundo OpenStreetMap -- versao com GRADIENTE DE MARGEM
# ==========================================================================
# A versao "vencedor leva tudo" (mapa_df_osm_res8.R) exagera visualmente o
# candidato cujo eleitorado esta mais disperso geograficamente: mesmo uma
# celula ganha por 51%-49% pinta 100% da cor do vencedor. Isso fez o mapa
# parecer 190 x 15 quando o resultado real do DF foi 59%-41%.
#
# Aqui a cor e um GRADIENTE continuo (diverging: azul <-> cinza neutro <->
# vermelho) proporcional ao % de votos do Lula na celula: azul saturado =
# Bolsonaro domina, cinza claro = disputa proxima de 50/50, vermelho
# saturado = Lula domina. Isso preserva a intensidade real da disputa em
# vez de reduzir tudo a "quem ganhou".
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

# ---- 2. agrega por celula H3 res.8 e calcula a MARGEM (% Lula) ------------
agg <- votos[, .(votos = sum(QT_VOTOS)), by = .(h3_08, NR_VOTAVEL)]
agg_wide <- dcast(agg, h3_08 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))
agg_wide[, `:=`(
  total = lula + bolsonaro,
  pct_lula = round(100 * lula / (lula + bolsonaro), 1)
)]
agg_wide[, pct_bolsonaro := round(100 - pct_lula, 1)]

cat("\nDistribuicao do % Lula por celula (res. 8):\n")
print(summary(agg_wide$pct_lula))

hex_08 <- cell_to_polygon(agg_wide$h3_08, simple = FALSE)
hex_08 <- merge(hex_08, agg_wide, by.x = "h3_address", by.y = "h3_08")

# ---- 3. limites do DF (p/ enquadrar o mapa) --------------------------------
df_boundary <- tryCatch(read_state(code_state = "DF", year = 2020, showProgress = FALSE), error = function(e) NULL)
bb <- if (!is.null(df_boundary)) st_bbox(df_boundary) else st_bbox(hex_08)
centro <- c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])))

# ---- 4. escala de cor DIVERGENTE continua (azul <-> cinza <-> vermelho) ----
paleta <- c("#2563eb", "#f0f0f0", "#c0392b")  # 0% Lula, 50% Lula, 100% Lula
cores_expr <- interpolate(column = "pct_lula", values = c(0, 50, 100), stops = paleta)

mapa <- maplibre(style = openfreemap_style("liberty"), center = centro, zoom = 10, pitch = 0) |>
  fit_bounds(c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]), animate = FALSE) |>
  add_source(id = "hex08_src", data = hex_08) |>
  add_fill_layer(
    id = "hex_08", source = "hex08_src",
    fill_color = cores_expr, fill_opacity = 0.65, fill_outline_color = "white",
    tooltip = "Lula: {pct_lula}% ({lula} votos)<br>Bolsonaro: {pct_bolsonaro}% ({bolsonaro} votos)<br>Total: {total} votos"
  ) |>
  add_continuous_legend(
    legend_title = "% Lula (2º turno)",
    values = c("0% (Bolsonaro)", "50%", "100% (Lula)"),
    colors = paleta,
    position = "bottom-left"
  ) |>
  add_navigation_control() |>
  add_fullscreen_control()

# ---- 5. salva HTML interativo -----------------------------------------------
arquivo_html <- file.path(diretorio_base, "mapa_df_osm_res8_margem.html")
saveWidget(mapa, arquivo_html, selfcontained = TRUE, title = "DF - Presidente 2022 (2o turno) - Margem H3 res 8")
cat(sprintf("\nMapa interativo salvo em: %s\n", arquivo_html))

# ---- 6. PNG estatico p/ conferencia (best-effort) --------------------------
arquivo_png <- file.path(diretorio_base, "mapa_df_osm_res8_margem.png")
ok_png <- tryCatch({
  save_map(mapa, arquivo_png, width = 1400, height = 1200, delay = 8)
  TRUE
}, error = function(e) { cat("Aviso: nao foi possivel gerar PNG automatico:", conditionMessage(e), "\n"); FALSE })
if (ok_png) cat(sprintf("PNG de conferencia salvo em: %s\n", arquivo_png))
