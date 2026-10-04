# ==========================================================================
# Mapa coropletico do Plano Piloto (2o turno, Presidente 2022), resolucoes
# H3 8 e 9, com a mancha urbana (IBGE) como plano de fundo.
# Vermelho = Lula > Bolsonaro na celula | Azul = Bolsonaro > Lula
# ==========================================================================
# O DF nao tem um poligono administrativo pronto so do "Plano Piloto" (RA-I)
# nos pacotes de geo publica disponiveis (geobr traz malha urbana do IBGE,
# mas sem o recorte por Regiao Administrativa do DF). Por isso o recorte
# aqui e feito por uma caixa delimitadora (bounding box) que cobre o nucleo
# urbano de Brasilia (Asa Norte/Sul, Eixo Monumental, Lago Norte/Sul,
# Cruzeiro/Sudoeste/Octogonal) -- ou seja, uma aproximacao pragmatica do
# "Plano Piloto" no sentido coloquial (centro x cidades-satelite), nao a
# fronteira oficial exata da RA-I.
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
dt[, h3_08 := NULL]  # remove a coluna antiga (calculada em outro momento) p/ evitar duplicata no merge
dt <- merge(dt, locais_unicos[, .(chave_local, lat2 = lat, lon2 = lon, h3_08, h3_09)], by = "chave_local", all.x = TRUE)

# ---- 3. filtra: 2o turno, Lula x Bolsonaro, geocodificacao confiavel, dentro do Plano Piloto ----
votos <- dt[NR_TURNO == 2 & NR_VOTAVEL %in% c(13, 22) & h3_confiavel == TRUE &
              lon2 >= xlim_pp[1] & lon2 <= xlim_pp[2] & lat2 >= ylim_pp[1] & lat2 <= ylim_pp[2]]

n_locais_pp <- uniqueN(votos$chave_local)
cat(sprintf("Locais unicos no Plano Piloto (confiaveis): %d\n", n_locais_pp))

# ---- 4. mancha urbana (IBGE, via geobr) recortada ao Plano Piloto ----------
urbano_df <- read_urban_area(year = 2015, showProgress = FALSE)
urbano_pp <- suppressWarnings(st_crop(urbano_df, xmin = xlim_pp[1], xmax = xlim_pp[2],
                                       ymin = ylim_pp[1], ymax = ylim_pp[2]))

# ---- 5. funcao que agrega votos por celula H3 numa dada resolucao e monta o mapa ----
cores <- c("Lula" = "#c0392b", "Bolsonaro" = "#2563eb", "Empate" = "#9ca3af")

construir_mapa <- function(col_h3, res_label) {
  agg <- votos[, .(votos = sum(QT_VOTOS)), by = c("NR_VOTAVEL", col_h3)]
  setnames(agg, col_h3, "h3")
  agg_wide <- dcast(agg, h3 ~ NR_VOTAVEL, value.var = "votos", fill = 0)
  setnames(agg_wide, c("13", "22"), c("lula", "bolsonaro"))
  agg_wide[, vencedor := fifelse(lula > bolsonaro, "Lula",
                            fifelse(bolsonaro > lula, "Bolsonaro", "Empate"))]

  poligonos <- cell_to_polygon(agg_wide$h3, simple = FALSE)
  poligonos <- merge(poligonos, agg_wide, by.x = "h3_address", by.y = "h3")

  cat(sprintf("\nResolucao %s -- celulas por vencedor:\n", res_label))
  print(table(poligonos$vencedor))

  ggplot() +
    geom_sf(data = urbano_pp, fill = "grey85", color = NA) +
    geom_sf(data = poligonos, aes(fill = vencedor), color = "white", linewidth = 0.15, alpha = 0.92) +
    scale_fill_manual(values = cores, name = "Mais votado\n(2º turno)") +
    coord_sf(xlim = xlim_pp, ylim = ylim_pp, expand = FALSE) +
    labs(title = sprintf("Resolução H3 %s", res_label)) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      panel.background = element_rect(fill = "grey96", color = NA)
    )
}

mapa_8 <- construir_mapa("h3_08", "8")
mapa_9 <- construir_mapa("h3_09", "9")

# ---- 6. combina os dois paineis ---------------------------------------------
titulo <- ggdraw() +
  draw_label("Plano Piloto — Presidente 2022 (2º turno)", fontface = "bold", size = 16, x = 0.5)
subtitulo <- ggdraw() +
  draw_label("Vencedor por célula H3, sobre a mancha urbana (IBGE) · locais com geocodificação confiável",
             size = 10, color = "grey30", x = 0.5)

paineis <- plot_grid(mapa_8 + theme(legend.position = "none"),
                      mapa_9 + theme(legend.position = "none"),
                      nrow = 1)
legenda <- get_legend(mapa_8 + theme(legend.position = "bottom"))

rodape <- ggdraw() +
  draw_label("Fonte: TSE (votação por seção) geocodificado com {geocodebr} + Censo Escolar/INEP · mancha urbana: IBGE (geobr::read_urban_area)",
             size = 7, color = "grey50", x = 0.5)

mapa_final <- plot_grid(titulo, subtitulo, paineis, legenda, rodape,
                         ncol = 1, rel_heights = c(0.09, 0.05, 1, 0.08, 0.05))

arquivo_saida <- file.path(diretorio_base, "mapa_plano_piloto_h3_8_9.png")
ggsave(arquivo_saida, mapa_final, width = 13, height = 8, dpi = 300, bg = "white")
cat(sprintf("\nMapa salvo em: %s\n", arquivo_saida))
