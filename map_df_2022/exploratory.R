library(tidyverse)
library(electionsBR)
library(basedosdados)
library(bigrquery)
library(geocodebr)
library(geobr)
df <- read.csv("dados/bweb_2t_DF_311020221535.csv", sep = ';', fileEncoding = "latin1")

secoes_df <- read.csv("dados/votacao_secao_2022_BR.csv", sep = ';', fileEncoding = "latin1")
secoes_df <- secoes_df |> 
  filter(SG_UF == "DF",
         NR_TURNO == 2)
