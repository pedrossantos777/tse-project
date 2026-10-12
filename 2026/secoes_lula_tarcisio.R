library(tidyverse)

# depende de dfsp_gov e df_pres_sp gerados em exploratory_export_to_parquet.R
# (todas as candidaturas de Governador-SP e Presidente-SP, uma linha por secao x candidato)

chave_secao <- c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO")

# vencedor de cada secao - Governador
vencedor_gov <- dfsp_gov |>
  summarise(QT_VOTOS = sum(QT_VOTOS), .by = c(all_of(chave_secao), NR_VOTAVEL, NM_VOTAVEL)) |>
  slice_max(QT_VOTOS, n = 1, with_ties = FALSE, by = all_of(chave_secao))

# vencedor de cada secao - Presidente
vencedor_pres <- df_pres_sp |>
  summarise(QT_VOTOS = sum(QT_VOTOS), .by = c(all_of(chave_secao), NR_VOTAVEL, NM_VOTAVEL)) |>
  slice_max(QT_VOTOS, n = 1, with_ties = FALSE, by = all_of(chave_secao))

# secoes em que Tarcisio (10) venceu Governador E Lula (13) venceu Presidente
secoes_tarcisio_lula <- vencedor_gov |>
  filter(NR_VOTAVEL == 10) |>
  rename(NR_VOTAVEL_GOV = NR_VOTAVEL, NM_VOTAVEL_GOV = NM_VOTAVEL, QT_VOTOS_GOV = QT_VOTOS) |>
  inner_join(
    vencedor_pres |>
      filter(NR_VOTAVEL == 13) |>
      rename(NR_VOTAVEL_PRES = NR_VOTAVEL, NM_VOTAVEL_PRES = NM_VOTAVEL, QT_VOTOS_PRES = QT_VOTOS),
    by = chave_secao
  )
