library(geobr)
geo_pool  <- read_polling_places(
  year = 2022,
  code_muni = "DF",
  output = "sf",
  showProgress = TRUE,
  cache = TRUE,
  verbose = TRUE
)

geo_pool <- geo_pool |> 
  select(nr_local_votacao_original, nr_cep,lat_tse, lon_tse, lat_geocodebr, lon_geocodebr, precisao_geocodebr,
         tipo_resultado_geocodebr, desvio_metros_geocodebr, coords_source, geometry)


#left join 
#a ver como se faz um distinct join
secoes_df_geo <- secoes_df |> 
  left_join(geo_pool, by = c("NR_LOCAL_VOTACAO" = "nr_local_votacao_original"))
  
#secoes_df_geo <- secoes_df |> 
 # left_join(geo_pool, by = c("NR_LOCAL_VOTACAO" = "nr_local_votacao_original"))
