## -----------------------------------------------------------------------------
#| label: packages
#| collapse: true
#| eval: false
# if (!requireNamespace("sf")) install.packages("sf")
# if (!requireNamespace("leaflet")) install.packages("leaflet")

## -----------------------------------------------------------------------------
#| label: loading_parzer
#| eval: true
library(parzer)


## -----------------------------------------------------------------------------
#| label: lats_lons
lats <- c(
  "46.4183",
  "46.4383° N",
  "46.5683° N",
  "46° 27´ 5.4\" N",
  "46° 25.56’",
  "N46°24’4.333"
)
lons <- c(
  "25.7391",
  "E25°34’6.4533",
  "25.3391° E",
  "25.8391° E",
  "25° 35.56’",
  "E25°34’4.333"
)


## -----------------------------------------------------------------------------
#| label: creating_dat
#| collapse: true
dat <- data.frame(
  longitude = parse_lon(lons),
  latitude = parse_lat(lats)
)
dat


## -----------------------------------------------------------------------------
#| label: adding_variables_to_dat
#| collapse: true
dat$shape <- c("round", "square", "triangle", "round", "square", "square")
dat$color <- c("blue", "yellow", "green", "red", "green", "yellow")
dat


## -----------------------------------------------------------------------------
#| label: converting_dat_to_sf
#| warning: false
#| collapse: true
datsf <- sf::st_as_sf(dat, coords = c("longitude", "latitude"))
datsf


## -----------------------------------------------------------------------------
#| label: map_center
center_lon <- mean(dat$longitude)
center_lat <- mean(dat$latitude)


## ----out.with="100%"----------------------------------------------------------
#| label: fig-leaflet_1
#| warning: false
#| collapse: true
library("leaflet")
leaflet() |>
  addTiles() |>
  addMarkers(data = datsf) |>
  setView(center_lon, center_lat, zoom = 10)


## -----------------------------------------------------------------------------
#| label: fig-leaflet_2
#| warning: false
bbox <- c(
  xmin = 25.42813, ymin = 46.39455,
  xmax = 25.68769, ymax = 46.60346
)
leaflet() |>
  addTiles() |>
  addRectangles(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) |>
  setView(center_lon, center_lat, zoom = 10)


## -----------------------------------------------------------------------------
#| label: cropping_dat
datsf_c <- sf::st_crop(datsf, bbox)


## ----out.with="100%"----------------------------------------------------------
#| label: fig-leaflet_3
#| warning: false
leaflet() |>
  addTiles() |>
  addMarkers(data = datsf_c) |>
  setView(center_lon, center_lat, zoom = 10)

