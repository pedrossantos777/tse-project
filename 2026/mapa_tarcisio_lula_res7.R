# ==========================================================================
# Mapa coropletico (H3 resolucao 7) das secoes de SP em que Tarcisio
# (Governador) e Lula (Presidente) foram os mais votados simultaneamente.
# Classificacao por quebras naturais (Jenks) em vez de gradiente continuo.
# ==========================================================================
# le dados/secoes_tarcisio_lula_geo.parquet (gerado por
# geolocalizar_secoes_tarcisio_lula.R) -- nao depende de objetos em memoria.

suppressMessages({
  library(dplyr)
  library(sf)
  library(h3jsr)
  library(arrow)
  library(ggplot2)
  library(classInt)
})

secoes <- read_parquet("dados/secoes_tarcisio_lula_geo.parquet")

# geobr usa lon/lat = c(-1, -1) como sentinela para locais fora do Brasil /
# sem coordenada -- sao so 16 das 12914 linhas, mas distorcem o bbox do mapa
# (ia parar no Golfo da Guine) se nao forem removidas antes de agregar.
n_sentinela <- sum(secoes$lon == -1 & secoes$lat == -1, na.rm = TRUE)
cat(sprintf("Removendo %d secoes com coordenada sentinela (-1,-1)\n", n_sentinela))
secoes <- secoes |> filter(!(lon == -1 & lat == -1))

# ---- 1. Agrega por hexagono H3 res. 7 --------------------------------------
# metrica do mapa: numero de secoes (vitorias conjuntas de Tarcisio e Lula)
# dentro de cada hexagono -- os votos de Governador e Presidente nao sao
# somaveis entre si (eleitorados/escalas diferentes), entao a unidade
# comparavel entre hexagonos e "quantas secoes" caem ali.
por_hex <- secoes |>
  summarise(
    n_secoes   = n(),
    votos_gov  = sum(QT_VOTOS_GOV),
    votos_pres = sum(QT_VOTOS_PRES),
    .by = h3_res7
  )

cat(sprintf("Hexagonos res. 7 com vitoria conjunta: %d\n", nrow(por_hex)))

# ---- 2. Poligono de cada hexagono ------------------------------------------
hex_geom <- cell_to_polygon(por_hex$h3_res7, simple = FALSE)
hex_geom <- st_sf(h3_res7 = por_hex$h3_res7, geometry = st_geometry(hex_geom))
mapa_dados <- left_join(hex_geom, por_hex, by = "h3_res7")

# ---- 3. Contornos de referencia (geobr) -------------------------------------
contorno_sp <- st_transform(
  geobr::read_state(code_state = "SP", year = 2022, showProgress = FALSE),
  st_crs(mapa_dados)
)
metro_areas <- geobr::read_metro_area(year = 2018, showProgress = FALSE)
rmsp <- st_transform(metro_areas |> filter(name_metro == "Rm São Paulo"), st_crs(mapa_dados))
baixada_santista <- st_transform(metro_areas |> filter(name_metro == "Rm Baixada Santista"), st_crs(mapa_dados))

# hexagono pertence a RMSP / Baixada Santista se intersecta o poligono da regiao
mapa_dados$dentro_rmsp <- lengths(st_intersects(mapa_dados, rmsp)) > 0
mapa_dados$dentro_baixada <- lengths(st_intersects(mapa_dados, baixada_santista)) > 0
cat(sprintf("Hexagonos dentro da RMSP: %d | Baixada Santista: %d | fora de ambas: %d\n",
            sum(mapa_dados$dentro_rmsp), sum(mapa_dados$dentro_baixada),
            sum(!mapa_dados$dentro_rmsp & !mapa_dados$dentro_baixada)))

# ---- 4. Classificacao por quebras naturais (Jenks) --------------------------
# cada mapa usa sua PROPRIA quebra de Jenks (calculada so sobre os hexagonos
# que aparecem naquele mapa) -- do contrario os valores altos da RMSP puxariam
# as classes e o litoral/interior apareceria todo na mesma faixa baixa.
classificar_jenks <- function(x, n = 5) {
  n_classes <- min(n, length(unique(x)))
  brks <- unique(classInt::classIntervals(x, n = n_classes, style = "jenks")$brks)
  cut(x, breaks = brks, include.lowest = TRUE, dig.lab = 3)
}

tema_mapa <- theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption = element_text(size = 7, color = "grey50"),
    legend.position = "right"
  )
escala_fill <- function() {
  scale_fill_brewer(palette = "Blues", direction = 1, name = "Seções\n(Tarcísio + Lula)",
                     na.translate = FALSE)
}

# ---- 5a. Panorama: estado inteiro -------------------------------------------
mapa_estado_dados <- mapa_dados |> mutate(classe = classificar_jenks(n_secoes))

mapa_estado <- ggplot() +
  geom_sf(data = contorno_sp, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = rmsp, fill = NA, color = "grey50", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = mapa_estado_dados, aes(fill = classe), color = NA) +
  escala_fill() +
  coord_sf(xlim = st_bbox(contorno_sp)[c("xmin", "xmax")],
           ylim = st_bbox(contorno_sp)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio (Governador) e Lula (Presidente) venceram",
    subtitle = "Panorama do estado de São Paulo — classes por quebras naturais (Jenks)",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places(). Hexágonos H3 res. 7."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res7_estado.png", mapa_estado, width = 9, height = 8, dpi = 150)

# ---- 5b. Recorte: Regiao Metropolitana de Sao Paulo -------------------------
mapa_rmsp_dados <- mapa_dados |>
  filter(dentro_rmsp) |>
  mutate(classe = classificar_jenks(n_secoes))

mapa_rmsp <- ggplot() +
  geom_sf(data = rmsp, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = mapa_rmsp_dados, aes(fill = classe), color = "white", linewidth = 0.1) +
  escala_fill() +
  coord_sf(xlim = st_bbox(rmsp)[c("xmin", "xmax")],
           ylim = st_bbox(rmsp)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio e Lula venceram — Região Metropolitana de SP",
    subtitle = "Agregado por hexágono H3, resolução 7 — classes por quebras naturais (Jenks)",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places()."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res7_rmsp.png", mapa_rmsp, width = 9, height = 8, dpi = 150)

# ---- 5c. Litoral e interior (fora da RMSP e da Baixada Santista) ------------
# 16,5% das secoes de vitoria conjunta ficam fora da RMSP; dessas, a Baixada
# Santista (Santos, Sao Vicente, Cubatao, Guaruja...) e o nucleo mais forte e
# ganha mapa proprio (5d) -- aqui ficam so os polos do interior (Campinas,
# Araraquara/Sao Carlos, Ribeirao Preto). RMSP e Baixada ficam cinza (ja
# cobertas nos outros mapas).
mapa_litoral_dados <- mapa_dados |>
  filter(!dentro_rmsp, !dentro_baixada) |>
  mutate(classe = classificar_jenks(n_secoes))

mapa_litoral <- ggplot() +
  geom_sf(data = contorno_sp, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = rmsp, fill = "grey85", color = "grey50", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = baixada_santista, fill = "grey85", color = "grey50", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = mapa_litoral_dados, aes(fill = classe), color = NA) +
  escala_fill() +
  coord_sf(xlim = st_bbox(contorno_sp)[c("xmin", "xmax")],
           ylim = st_bbox(contorno_sp)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio e Lula venceram — interior",
    subtitle = "Fora da RMSP e da Baixada Santista (cinza, tracejado) — classes por Jenks",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places(). Hexágonos H3 res. 7."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res7_litoral_interior.png", mapa_litoral, width = 9, height = 8, dpi = 150)

# ---- 5d. Recorte: Baixada Santista -------------------------------------------
mapa_baixada_dados <- mapa_dados |>
  filter(dentro_baixada) |>
  mutate(classe = classificar_jenks(n_secoes))

mapa_baixada <- ggplot() +
  geom_sf(data = baixada_santista, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = mapa_baixada_dados, aes(fill = classe), color = "white", linewidth = 0.1) +
  escala_fill() +
  coord_sf(xlim = st_bbox(baixada_santista)[c("xmin", "xmax")],
           ylim = st_bbox(baixada_santista)[c("ymin", "ymax")]) +
  labs(
    title = "Seções em que Tarcísio e Lula venceram — Baixada Santista",
    subtitle = "Agregado por hexágono H3, resolução 7 — classes por quebras naturais (Jenks)",
    caption = "Fonte: TSE (votação por seção, 2026) + geobr::read_polling_places()."
  ) +
  tema_mapa

ggsave("dados/mapa_tarcisio_lula_res7_baixada_santista.png", mapa_baixada, width = 9, height = 8, dpi = 150)

cat("Mapas salvos: _estado.png, _rmsp.png, _litoral_interior.png, _baixada_santista.png\n")
