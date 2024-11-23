## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  out.width = "100%" 
)

## -----------------------------------------------------------------------------
library(geoidep)

## -----------------------------------------------------------------------------
providers <- get_data_sources()
providers

## -----------------------------------------------------------------------------
layers_available <- get_providers()
layers_available

## -----------------------------------------------------------------------------
# Region boundaries download 
loreto_prov <- get_provinces(show_progress = FALSE) |> 
  subset(NOMBDEP == 'LORETO')

## ----out.width='100%', out.height=250-----------------------------------------
library(leaflet)
library(sf)
loreto_prov |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons()

## -----------------------------------------------------------------------------
# Defined Ubigeo 
loreto_prov[["ubigeo"]] <- paste0(loreto_prov[["CCDD"]],loreto_prov[["CCPP"]])

## -----------------------------------------------------------------------------
# The first five rows
head(loreto_prov)

## -----------------------------------------------------------------------------
my_fun <- function(x){
  data <- get_forest_loss_data(
    layer = 'stock_bosque_perdida_provincia',
    ubigeo = loreto_prov[["ubigeo"]][x],
    show_progress = FALSE )
  return(data)
}
historico_list <- lapply(X = 1:nrow(loreto_prov),FUN = my_fun)
historico_df <- do.call(rbind.data.frame,historico_list)

## -----------------------------------------------------------------------------
# The first five rows
head(historico_df)

## ----fig.align='center'-------------------------------------------------------
library(ggplot2)
library(dplyr)
historico_df |> 
  inner_join(y = loreto_prov,by = "ubigeo") |> 
  ggplot(aes(x = anio,y = perdida)) + 
  geom_point(size = 1) + 
  geom_line() + 
  facet_wrap(NOMBPROV~.,ncol = 3) + 
  theme_minimal(base_size = 5) + 
  labs(
    title = "Pérdida de bosque histórico del 2001-2023 \npara las provincias de Loreto",
    caption = "Fuente: Geobosque",
    x = "",
    y = "") 

