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
providers <- get_data_sources(query = "Serfor")
providers

## -----------------------------------------------------------------------------
incendio.forestal <- get_forest_fire_data(show_progress = FALSE)
head(incendio.forestal)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(sf)
library(dplyr)
library(ggplot2)
stats <- incendio.forestal |> 
  st_drop_geometry() |>
  filter(FECHA >= '2024-09-11') |> 
  group_by(NOMDEP) |> 
  summarise( total = n()) 

## ----out.height=500,out.width=500,fig.align='center'--------------------------
# Simple visualisation
stats |> 
  ggplot(aes(x = NOMDEP, y = total)) + 
  geom_bar(stat = "identity") + 
  coord_polar() + 
  theme_minimal(base_size = 10)  + 
  labs(x="", y = "", caption = "Unidad de Monitoreo Forestales - SERFOR")

## -----------------------------------------------------------------------------
# Region boundaries download 
ucayali_dep <- get_departaments(show_progress = FALSE) |> 
  subset(NOMBDEP == 'UCAYALI')

## -----------------------------------------------------------------------------
# The first five rows
head(ucayali_dep)

## ----warning=FALSE,message=FALSE----------------------------------------------
# Data collection only within the regions of interest.
ucayali.if <- st_filter(incendio.forestal, ucayali_dep)
head(ucayali.if)

## ----fig.align='center',out.height=500----------------------------------------
library(leaflet)
library(leaflet.extras)
ucayali.if |> 
  leaflet() |> 
  addProviderTiles(provider = "CartoDB.Positron") |> 
  addHeatmap(radius = 10,minOpacity = 1)

