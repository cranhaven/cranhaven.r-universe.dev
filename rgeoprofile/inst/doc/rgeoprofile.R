## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(rgeoprofile)
library(leaflet)
require(leafsync)
load("vignette_results.RData")

## ----setup, eval = FALSE------------------------------------------------------
#  # Install from CRAN
#  install.packages("rgeoprofile")
#  # Install Development Version
#  devtools::install_github("JSSpaulding/rgeoprofile")
#  # After installation, load and attach the package
#  library(rgeoprofile)

## ----data---------------------------------------------------------------------
data("desalvo")
desalvo

## ---- eval=FALSE--------------------------------------------------------------
#  data(desalvo)
#  cgt <- cgt_profile(desalvo$lat, desalvo$lon)

## -----------------------------------------------------------------------------
# Code to map resultant geographic profile
g_map = sp::SpatialPixelsDataFrame(points = cgt[c("lons", "lats")], data = cgt)
g_map <- raster::raster(g_map)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
library(leaflet) #for mapping the geographic profile
pal <- colorNumeric(pals::parula(200), raster::values(g_map),
                    na.color = "transparent")

# Map 
cgt_map <- leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

## -----------------------------------------------------------------------------
cgt_map

## ---- eval=FALSE--------------------------------------------------------------
#  #CrimeStat Default Neg. Exp.
#  ne_1 <- neg_exp_profile(desalvo$lat, desalvo$lon, method = "CrimeStat")
#  #Dragnet Default Neg. Exp.
#  ne_2 <- neg_exp_profile(desalvo$lat, desalvo$lon, method = "Dragnet")
#  #Dragnet Neg. Exp. with Buffer Zone
#  ne_3 <- neg_exp_profile(desalvo$lat, desalvo$lon, method = "Dragnet", buffer = TRUE)

## -----------------------------------------------------------------------------
## Maps of Boston Strangler negative exponential geographic profiles
g_map1 = sp::SpatialPixelsDataFrame(points = ne_1[c("lons", "lats")], data = ne_1)
g_map1 <- raster::raster(g_map1)
g_map2 = sp::SpatialPixelsDataFrame(points = ne_2[c("lons", "lats")], data = ne_2)
g_map2 <- raster::raster(g_map2)
g_map3 = sp::SpatialPixelsDataFrame(points = ne_3[c("lons", "lats")], data = ne_3)
g_map3 <- raster::raster(g_map3)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map1) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
raster::crs(g_map2) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
raster::crs(g_map3) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
pal1 <- colorNumeric(pals::parula(200), raster::values(g_map1),
                    na.color = "transparent")
pal2 <- colorNumeric(pals::parula(200), raster::values(g_map2),
                    na.color = "transparent")
pal3 <- colorNumeric(pals::parula(200), raster::values(g_map3),
                    na.color = "transparent")

# Map 
ne_crimestat_map <- leaflet() %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map1, colors = pal1, opacity = 0.6) %>%
  addLegend(pal = pal1, values = raster::values(g_map1), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")
ne_dragnet_map <- leaflet() %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map2, colors = pal2, opacity = 0.6) %>%
  addLegend(pal = pal2, values = raster::values(g_map2), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")
ne_dbuffer_map <- leaflet() %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map3, colors = pal3, opacity = 0.6) %>%
  addLegend(pal = pal3, values = raster::values(g_map3), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

# Multi-plot of Negative Exponential Maps				   
library(leafsync)
leafsync::latticeView(ne_crimestat_map, ne_dragnet_map, ne_dbuffer_map,
                      sync = "all")

## ---- eval=FALSE--------------------------------------------------------------
#  linear <- linear_profile(desalvo$lat, desalvo$lon)
#  ## Follow the mapping method under the `cgt_profile()` function

## ---- include=FALSE-----------------------------------------------------------
# Code to map resultant geographic profile
g_map = sp::SpatialPixelsDataFrame(points = linear[c("lons", "lats")], data = linear)
g_map <- raster::raster(g_map)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
pal <- colorNumeric(pals::parula(200), raster::values(g_map),
                    na.color = "transparent")

# Map 
linear_map <- leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

## -----------------------------------------------------------------------------
## Map of Boston Strangler linear geographic profile
linear_map

## ---- eval=FALSE--------------------------------------------------------------
#  lognorm <- lognorm_profile(desalvo$lat, desalvo$lon)
#  ## Follow the mapping method under the `cgt_profile()` function

## ---- include=FALSE-----------------------------------------------------------
# Code to map resultant geographic profile
g_map = sp::SpatialPixelsDataFrame(points = lognorm[c("lons", "lats")], data = lognorm)
g_map <- raster::raster(g_map)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
pal <- colorNumeric(pals::parula(200), raster::values(g_map),
                    na.color = "transparent")

# Map 
lognorm_map <- leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

## -----------------------------------------------------------------------------
## Map of Boston Strangler lognormal geographic profile
lognorm_map

## ---- eval=FALSE--------------------------------------------------------------
#  normal <- norm_profile(desalvo$lat, desalvo$lon)
#  ## Follow the mapping method under the `cgt_profile()` function

## ---- include=FALSE-----------------------------------------------------------
# Code to map resultant geographic profile
g_map = sp::SpatialPixelsDataFrame(points = normal[c("lons", "lats")], data = normal)
g_map <- raster::raster(g_map)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
pal <- colorNumeric(pals::parula(200), raster::values(g_map),
                    na.color = "transparent")

# Map 
norm_map <- leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

## -----------------------------------------------------------------------------
## Map of Boston Strangler normal geographic profile
norm_map

## ---- eval=FALSE--------------------------------------------------------------
#  tne <- trun_neg_exp_profile(desalvo$lat, desalvo$lon)
#  ## Follow the mapping method under the `cgt_profile()` function

## ---- include=FALSE-----------------------------------------------------------
# Code to map resultant geographic profile
g_map = sp::SpatialPixelsDataFrame(points = tne[c("lons", "lats")], data = tne)
g_map <- raster::raster(g_map)

# Assign a Coordinate Reference System for the Raster
raster::crs(g_map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define a Parula Color Pallete for Resultant Jeopardy Surface
pal <- colorNumeric(pals::parula(200), raster::values(g_map),
                    na.color = "transparent")

# Map 
tne_map <- leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addRasterImage(g_map, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = raster::values(g_map), title = 'Score') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2,
                   fillColor = "black")

## -----------------------------------------------------------------------------
## Map of Boston Strangler truncated negative exponential geographic profile
tne_map

## -----------------------------------------------------------------------------
circle_center <- function(lat, lon){
  n <- length(lat)
  points <- data.frame(lat, lon)
  dat <- spatstat.geom::ppp(lon, lat, window = spatstat.geom::owin(xrange = c(min(lon), max(lon)),
                                     yrange = c(min(lat), max(lat))))
  nndists <- spatstat.geom::nndist.ppp(dat, k = n-1)
  fncases <- which(grepl(max(nndists), nndists))
  xi <- points[fncases[1],]
  xj <- points[fncases[2],]
  cc_lat <- mean(xi$lat, xj$lat)
  cc_lon <- mean(xi$lon, xj$lon)
  return(data.frame(lat = cc_lat, lon = cc_lon))
}

bs_cc <- circle_center(desalvo$lat, desalvo$lon)

## -----------------------------------------------------------------------------
bs_cmd <- cmd_pred(desalvo$lat, desalvo$lon)

## -----------------------------------------------------------------------------
bs_geom <- cmd_pred(desalvo$lat, desalvo$lon)

## -----------------------------------------------------------------------------
bs_harm <- cmd_pred(desalvo$lat, desalvo$lon)

## -----------------------------------------------------------------------------
bs_mean <- data.frame(lat = mean(desalvo$lat), 
                      lon = mean(desalvo$lon))
bs_med <- data.frame(lat = median(desalvo$lat), 
                     lon = median(desalvo$lon))

## -----------------------------------------------------------------------------
leaflet(width = "100%") %>%
  addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
  addAwesomeMarkers(lng = -71.07357, lat = 42.41322, icon = awesomeIcons(
    icon = 'home', markerColor = 'green'), popup = 'Residence') %>%
  addCircleMarkers(lng = desalvo$lon, lat = desalvo$lat, radius = 4, opacity = 1,
                   stroke = FALSE, fillOpacity = 0.75, weight = 2, fillColor = "black") %>%
  addCircleMarkers(lng = bs_cc$lon, lat = bs_cc$lat, radius = 4, color = "red", 
                   stroke = TRUE, fillOpacity = 1, label = "Center of Circle", 
                   labelOptions = labelOptions(noHide = T, direction ="top")) %>%
  addCircleMarkers(lng = bs_cmd$lon, lat = bs_cmd$lat, radius = 4, color = "orange",
                   stroke = TRUE, fillOpacity = 1, label = "CMD", 
                   labelOptions = labelOptions(noHide = T, direction ="right")) %>%
  addCircleMarkers(lng = bs_geom$lon, lat = bs_geom$lat, radius = 4, color = "yellow",
                   stroke = TRUE, fillOpacity = 1, label = "Geometric Mean", 
                   labelOptions = labelOptions(noHide = T, direction ="left")) %>%
  addCircleMarkers(lng = bs_harm$lon, lat = bs_harm$lat, radius = 4, color = "green", 
                   stroke = TRUE, fillOpacity = 1, label = "Harmonic Mean", 
                   labelOptions = labelOptions(noHide = T)) %>%
  addCircleMarkers(lng = bs_mean$lon, lat = bs_mean$lat, radius = 4, color = "blue", 
                   stroke = TRUE, fillOpacity = 1, label = "Mean", 
                   labelOptions = labelOptions(noHide = T)) %>%
  addCircleMarkers(lng = bs_med$lon, lat = bs_med$lat, radius = 4, color = "purple", 
                   stroke = TRUE, fillOpacity = 1, label = "Median", 
                   labelOptions = labelOptions(noHide = T))

