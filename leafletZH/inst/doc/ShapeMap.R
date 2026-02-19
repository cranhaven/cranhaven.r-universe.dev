## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
## devtools::install_github('Damonsoul/leafletZH')
## install.packages('leafletZH')

## -----------------------------------------------------------------------------
library(leaflet)
library(leafletZH)
leaflet() |>
  addTilesAmap() |>
  setView(lng = 120.33739, lat = 31.13533, zoom = 3)

## -----------------------------------------------------------------------------
library(leaflet)
library(leaflet.extras)
library(leafletZH)
library(sf)
data <- data.frame(adcode = seq(110101, 110110, 1), value = runif(5))
leaflet() |>
  leafletZH::addTilesAmap() |>
  addCityShape(
    data = data, adcode = "adcode", valueProperty = "value",
    popupProps = c("value")
  ) |>
  setView(lng = 116, lat = 40, zoom = 8)

## -----------------------------------------------------------------------------
library(leaflet)
library(leaflet.extras)
library(leafletZH)
library(sf)
data <- data.frame(adcode = seq(110000, 150000, 10000), value = runif(5))
leaflet() |>
  leafletZH::addTilesAmap() |>
  addProvinceShape(
    data = data, adcode = "adcode", valueProperty = "value",
    popupProps = c("value")
  ) |>
  setView(lng = 110, lat = 40, zoom = 3)

## -----------------------------------------------------------------------------
library(leaflet)
library(leaflet.extras)
library(leafletZH)
data <- data.frame(name = c("河北省", "山西", "陕西"), value = runif(3))
leaflet() |>
  leafletZH::addTilesAmap() |>
  addProvinceShape(
    data = data,
    provinceName = "name",
    valueProperty = "value",
    popupProps = c("value")
  ) |>
  setView(lng = 110, lat = 40, zoom = 4)

## -----------------------------------------------------------------------------
library(leaflet)
library(leaflet.extras)
library(leafletZH)
data <- data.frame(name = leafletZH::china_province$name, value = runif(34))
backg <- htmltools::tags$style(".leaflet-container { background: #000; }")
leaflet() |>
  addProvinceShape(
    data = data, provinceName = "name", valueProperty = "value",
    popupProps = c("value")
  ) |>
  setView(lng = 110, lat = 40, zoom = 2) |>
  htmlwidgets::prependContent(backg)

## -----------------------------------------------------------------------------
library(leaflet)
library(leafletZH)
leaflet() |>
  addTilesAmap() |>
  addAreaPolygons(
    longitude = c(121.0, 122.1, 121.2, 122.15, 121.5),
    latitude = c(31.1, 31.919, 31.917, 31.15, 31.5),
    coordinate = "WGS-84"
  ) |>
  addAwesomeMarkers(
    lng = c(121.0, 122.1, 121.2, 122.15, 121.5),
    lat = c(31.1, 31.919, 31.917, 31.15, 31.5)
  )

