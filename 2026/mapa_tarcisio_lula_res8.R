# ==========================================================================
# Mapa coropletico (H3 resolucao 8) das secoes de SP em que Tarcisio
# (Governador) e Lula (Presidente) foram os mais votados simultaneamente.
# ==========================================================================
# le dados/secoes_tarcisio_lula_geo.parquet (gerado por
# geolocalizar_secoes_tarcisio_lula.R) -- nao depende de objetos em memoria.

suppressMessages({
  library(dplyr)
  library(sf)
  library(h3jsr)
  library(arrow)
  library(ggplot2)
})

secoes <- read_parquet("dados/secoes_tarcisio_lula_geo.parquet")

# geobr usa lon/lat = c(-1, -1) como sentinela para locais fora do Brasil /
# sem coordenada -- sao so 16 das 12914 linhas, mas distorcem o bbox do mapa
# (ia parar no Golfo da Guine) se nao forem removidas antes de agregar.
n_sentinela <- sum(secoes$lon == -1 & secoes$lat == -1, na.rm = TRUE)
cat(sprintf("Removendo %d secoes com coordenada sentinela (-1,-1)\n", n_sentinela))
secoes <- secoes |> filter(!(lon == -1 & lat == -1))

# ---- 1. Agrega por hexagono H3 res. 8 --------------------------------------
# metrica do mapa: numero de secoes (vitorias conjuntas de Tarcisio e Lula)
# dentro de cada hexagono -- os votos de Governador e Presidente nao sao
# somaveis entre si (eleitorados/escalas diferentes), entao a unidade
# comparavel entre hexagonos e "quantas secoes" caem ali.
por_hex <- secoes |>
  summarise(
    n_secoes   = n(),
    votos_gov  = sum(QT_VOTOS_GOV),
    votos_pres = sum(QT_VOTOS_PRES),
    .by = h3_res8
  )

cat(sprintf("Hexagonos res. 8 com vitoria conjunta: %d\n", nrow(por_hex)))

# ---- 2. Poligono de cada hexagono ------------------------------------------
hex_geom <- cell_to_polygon(por_hex$h3_res8, simple = FALSE)
hex_geom <- st_sf(h3_res8 = por_hex$h3_res8, geometry = st_geometry(hex_geom))
mapa_dados <- left_join(hex_geom, por_hex, by = "h3_res8")

# ---- 3. Contornos de referencia (geobr) -------------------------------------
contorno_sp <- st_transform(
  geobr::read_state(code_state = "SP", year = 2022, showProgress = FALSE),
  st_crs(mapa_dados)
)
rmsp <- st_transform(
  geobr::read_metro_area(year = 2018, showProgress = FALSE) |>
    filter(name_metro == "Rm São Paulo"),
  st_crs(mapa_dados)
)

tema_mapa <- theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption = element_text(size = 7, color = "grey50"),
    legend.position = "right"
  )
escala_fill <- scale_fill_distiller(
  palette = "Blues", direction = 1, name = "Seções\n(Tarcísio + Lula)"
)

# ---- 4a. Panorama: estado inteiro -------------------------------------------
# resolucao 8 e muito fina para o estado inteiro (hexagonos de ~0,46 km2 num
# estado de ~248 mil km2) -- aqui o mapa mostra ONDE a vitoria conjunta se
# concentra, nao hexagonos individuais legiveis um a um.
mapa_estado <- ggplot() +
  geom_sf(data = contorno_sp, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = rmsp, fill = NA, color = "grey50", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = mapa_dados, aes(fill = n_secoes), color = NA) +
  escala_fill +
  coord_sf(xlim = st_bbox(contorno_sp)[c("xmin", "xmax")],
           ylim = st_bbox(contorno_sp)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio (Governador) e Lula (Presidente) venceram",
    subtitle = "Panorama do estado de São Paulo — tracejado: Região Metropolitana de SP (ver recorte)",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places(). Hexágonos H3 res. 8."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res8_estado.png", mapa_estado, width = 9, height = 8, dpi = 150)

# ---- 4b. Recorte: Regiao Metropolitana de Sao Paulo -------------------------
# onde a maior parte das secoes esta concentrada; aqui os hexagonos res. 8
# sao de fato distinguiveis uns dos outros.
mapa_rmsp <- ggplot() +
  geom_sf(data = rmsp, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = mapa_dados, aes(fill = n_secoes), color = "white", linewidth = 0.03) +
  escala_fill +
  coord_sf(xlim = st_bbox(rmsp)[c("xmin", "xmax")],
           ylim = st_bbox(rmsp)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio e Lula venceram — Região Metropolitana de SP",
    subtitle = "Agregado por hexágono H3, resolução 8",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places()."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res8_rmsp.png", mapa_rmsp, width = 9, height = 8, dpi = 150)

cat("Mapas salvos em dados/mapa_tarcisio_lula_res8_estado.png e _rmsp.png\n")
