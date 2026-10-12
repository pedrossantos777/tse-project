# ==========================================================================
# Mapa coropletico do DF (2o turno, Presidente 2022) por celula H3 res. 9
# Vermelho = Lula teve mais votos que Bolsonaro na celula
# Azul     = Bolsonaro teve mais votos que Lula na celula
# ==========================================================================
# Nota importante de qualidade: locais cuja geocodificacao so chegou a
# precisao "municipio" (ver geocodificar_secoes_df.R) caem todos no MESMO
# ponto (o centroide de Brasilia) -> na resolucao 9 (hexagonos de ~0,1 km2)
# isso empilharia votos de dezenas de locais fisicamente diferentes numa
# unica celula, dando uma falsa impressao de precisao espacial. Por isso
# este mapa usa so os locais com precisao "cep"/"localidade" (flag
# h3_confiavel), que sao ~83% dos votos do DF. O restante (~17%,
# nivel "municipio") fica de fora e e reportado separadamente.
# ==========================================================================

suppressMessages({
  library(data.table)
  library(arrow)
  library(h3jsr)
  library(sf)
  library(ggplot2)
  library(geobr)
  library(cowplot)
})

diretorio_base <- "C:/Users/Espada/Documents/R/tse_project"

# ---- 1. Carrega a base de secoes do DF ja geocodificada --------------------
dt <- as.data.table(read_parquet(file.path(diretorio_base, "dados", "secoes_df_geo.parquet")))

# ---- 2. H3 res. 9: calcula 1x por local unico (mais barato) e junta de volta ----
locais_unicos <- unique(dt[!is.na(lat), .(chave_local, lat, lon)], by = "chave_local")
locais_unicos[, h3_09 := point_to_cell(data.frame(lon = lon, lat = lat), res = 9, simple = TRUE)]
dt <- merge(dt, locais_unicos[, .(chave_local, h3_09)], by = "chave_local", all.x = TRUE)

# ---- 3. Filtra 2o turno, Lula x Bolsonaro, e so pontos confiaveis ----------
votos <- dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22) & h3_confiavel == TRUE]

pct_confiavel <- 100 * sum(dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]$QT_VOTOS[
  dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]$h3_confiavel]) /
  sum(dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22)]$QT_VOTOS)
cat(sprintf("Votos (Lula+Bolsonaro, 2o turno) em locais com geocodificacao confiavel: %.1f%%\n", pct_confiavel))

# ---- 4. Agrega votos por celula H3 e candidato -----------------------------
agg <- votos[, .(votos = sum(QT_VOTOS)), by = .(h3_09, NR_VOTAVEL)]
agg_wide <- dcast(agg, h3_09 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))

agg_wide[, vencedor := fifelse(lula > bolsonaro, "Lula",
                          fifelse(bolsonaro > lula, "Bolsonaro", "Empate"))]
agg_wide[, total := lula + bolsonaro]

cat("\nCelulas H3 (res. 9) por vencedor:\n")
print(agg_wide[, .N, by = vencedor])

# ---- 5. Poligonos dos hexagonos --------------------------------------------
poligonos <- cell_to_polygon(agg_wide$h3_09, simple = FALSE)
poligonos <- merge(poligonos, agg_wide, by.x = "h3_address", by.y = "h3_09")

# ---- 5b. contorno do DF p/ contexto espacial (geobr) -----------------------
contorno_df <- tryCatch(read_state(code_state = "DF", year = 2020, showProgress = FALSE),
                         error = function(e) NULL)

# recorta no bbox dos DADOS (+ margem) -- os hexagonos res. 9 tem ~0.1 km2,
# entao mostrar o DF inteiro (que inclui muita area rural sem locais de
# votacao) faz cada hexagono aparecer como 1 pixel; focar na area urbana
# onde os pontos existem deixa a forma hexagonal visivel.
bbox_dados <- st_bbox(poligonos)
margem <- 0.03
xlim <- c(bbox_dados["xmin"] - margem, bbox_dados["xmax"] + margem)
ylim <- c(bbox_dados["ymin"] - margem, bbox_dados["ymax"] + margem)

# ---- 6. Mapa ----------------------------------------------------------------
cores <- c("Lula" = "#c0392b", "Bolsonaro" = "#2563eb", "Empate" = "#9ca3af")

mapa <- ggplot()
if (!is.null(contorno_df)) {
  mapa <- mapa + geom_sf(data = contorno_df, fill = "grey96", color = "grey70", linewidth = 0.4)
}
mapa <- mapa +
  geom_sf(data = poligonos, aes(fill = vencedor), color = "white", linewidth = 0.06) +
  scale_fill_manual(values = cores, name = "Mais votado\n(2º turno)") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Distrito Federal — Presidente 2022 (2º turno)",
    subtitle = sprintf("Vencedor por célula H3 (resolução 9) · %.0f%% dos votos Lula+Bolsonaro representados", pct_confiavel),
    caption = "Fonte: TSE (votação por seção) geocodificado com {geocodebr} + Censo Escolar/INEP.\nCélulas com geocodificação apenas em nível de município foram excluídas (falsa precisão)."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey30"),
    plot.caption = element_text(size = 7, color = "grey50"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- 7. Painel de zoom: na escala do DF inteiro, hexagonos res.9 (~0.1 km2)
# aparecem como pontos; um recorte da area mais densa (Plano Piloto, onde
# fica o aglomerado de celulas vencidas por Lula) mostra a forma hexagonal
# de fato. ------------------------------------------------------
# bbox de todos os hexagonos do Lula cobriria a cidade toda (os vencidos por
# ele estao espalhados em varios clusters distantes); aqui focamos no maior
# aglomerado contiguo (Asa Sul), identificado a partir dos centroides
xlim_zoom <- c(-47.905, -47.868)
ylim_zoom <- c(-15.825, -15.798)

mapa_zoom <- ggplot(poligonos) +
  geom_sf(aes(fill = vencedor), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = cores, guide = "none") +
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = FALSE) +
  labs(title = "Zoom: Plano Piloto") +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    panel.background = element_rect(fill = "grey96", color = NA),
    panel.border = element_rect(fill = NA, color = "grey30", linewidth = 0.6)
  )

retangulo_zoom <- data.frame(xmin = xlim_zoom[1], xmax = xlim_zoom[2],
                              ymin = ylim_zoom[1], ymax = ylim_zoom[2])
mapa_com_marcador <- mapa +
  geom_rect(data = retangulo_zoom, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "grey20", linewidth = 0.5, inherit.aes = FALSE)

mapa_final <- ggdraw(mapa_com_marcador) +
  draw_plot(mapa_zoom, x = 0.02, y = 0.34, width = 0.34, height = 0.34)

arquivo_saida <- file.path(diretorio_base, "mapa_df_presidencial_h3_res9.png")
ggsave(arquivo_saida, mapa_final, width = 10, height = 9, dpi = 300, bg = "white")
cat(sprintf("\nMapa salvo em: %s\n", arquivo_saida))
