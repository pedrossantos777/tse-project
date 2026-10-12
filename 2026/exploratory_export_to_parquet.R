library(tidyverse)
library(arrow)
library(data.table)
#dfsp <- read.csv("dados/votacao_secao_2026_SP.csv")
dfsp <- fread("dados/votacao_secao_2026_SP.csv", encoding = "Latin-1")
#df <- fread("dados/detalhe_votacao_secao_2026_SP.csv", encoding = "Latin-1")


# dfsp_tarcisio <- dfsp |> 
#   filter(DS_CARGO == "Governador",
#          NR_VOTAVEL == 10)
#           

dfpres <- fread("dados/votacao_secao_2026_BR.csv", encoding = "Latin-1")

dfsp_gov <- dfsp |>
  filter(DS_CARGO == "Governador")

df_pres_sp <- dfpres |>
  filter(SG_UF == "SP")

# mesmo layout de colunas nos dois arquivos (confirmado no header dos CSVs),
# entao bind_rows empilha sem gerar NA: cada linha fica identificada por DS_CARGO
df_unificado <- bind_rows(dfsp_gov, df_pres_sp)

# Salve o arquivo Parquet
write_parquet(df_unificado, "dados/votacao_secao_2026_SP_unificado.parquet")

#leitura parquet
#dfsp <- read_parquet("dados/votacao_secao_2026_SP.parquet")

# dfsp_tarula <- dfsp |> 
#   filter(DS_CARGO == "Governador")
