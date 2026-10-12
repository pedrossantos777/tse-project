library(tidyverse)
library(arrow)

df <- read.csv("dados/votacao_secao_2022_BR.csv", sep = ';', fileEncoding = "latin1")

write_parquet(df, "dados/votacao_secao_2022_BR.parquet")
