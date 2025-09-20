## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, include = FALSE-------------------------------------------
library(ElevDistr)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("ElevDistr")
#  library("ElevDistr")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("devtools", repos = "http://cran.us.r-project.org")
#  devtools::install_github("LivioBaetscher/ElevDistr")
#  library("ElevDistr")

## -----------------------------------------------------------------------------
gstURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
                 "climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif")
gslURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
                 "climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif")

gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

## -----------------------------------------------------------------------------
gmted2010URL <- paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/",
                      "Global_tiles_GMTED/300darcsec/med/E000/30N000E_20101117_gmted_med300.tif")

gmted2010Part <- terra::rast(gmted2010URL, vsi = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  distance_to_treeline(lon = 8.65, lat = 46.87,  gstRaster = gst,  gslRaster = gsl,
#                       elevationRaster = gmted2010Part, elevation = 504, pointDf = pointsAboveTreeline,
#                       plot = FALSE, plotHist = FALSE, gstMin = 6.4, gslMin = 94)

## ----eval=FALSE, fig.cap="Example of a sampled area", fig.dim=c(7, 7), message=FALSE, include=FALSE----
#  #ggmap::ggmap_show_api_key()
#  point <- as.list(c(8.728898, 46.9375))
#  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
#  temp$df <- classify_above_treeline(temp$df, gst, gsl)
#  treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)
#  plot_distr(point, temp$df, treeline, 12)

## ----eval=FALSE, fig.cap="Example of treelinie distribution", fig.dim=c(7, 6), message=FALSE, include=FALSE----
#  point <- as.list(c(8.728898, 46.9375))
#  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
#  temp$df <- classify_above_treeline(temp$df, gst, gsl)
#  treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)
#  x <- calculate_distance(treeline = treeline, elevationRaster = gmted2010Part, pointElevation = 512, treelineSampling = 10, plot = TRUE)

## -----------------------------------------------------------------------------
#install.packages("rgbif") #Remove hashtag if you have not installed this package
myspecies <- c("Ranunculus pygmaeus", "Ranunculus thora")
gbifData <- rgbif::occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 20000)

## -----------------------------------------------------------------------------
temp1 <- gbifData$`Ranunculus pygmaeus`$data[, c("scientificName", "decimalLatitude", "decimalLongitude",
                                                 "elevation", "taxonRank")]
temp2 <- gbifData$`Ranunculus thora`$data[, c("scientificName", "decimalLatitude", "decimalLongitude",
                                              "elevation", "taxonRank")]

ranunculus <- rbind(temp1, temp2)

## ----message = FALSE----------------------------------------------------------
#install.packages("tidyverse") #Remove hashtag if you have not installed this package
library("tidyverse")
ranunculusFiltered <- ranunculus %>% filter(!is.na(elevation) & 0 < elevation & elevation < 8850)

## -----------------------------------------------------------------------------
ranunculusFiltered <- ranunculusFiltered %>% filter(taxonRank == "SPECIES")

## -----------------------------------------------------------------------------
set.seed(42) #Set a seed for reproducible
ranunculusSampled <- ranunculusFiltered %>% group_by(scientificName) %>% slice_head(n = 100)

## ----print = FALSE------------------------------------------------------------
#Import climatic layers
gstURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
                 "climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif")
gslURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
                 "climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif")

gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

#Import the DEM
gmted2010URL2 <- paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/",
                      "Global_tiles_GMTED/300darcsec/med/E000/50N000E_20101117_gmted_med300.tif")

gmted2010Part2 <- terra::rast(gmted2010URL2, vsi = TRUE)

#Run classification for the first five elements
elev <- distance_to_treeline(lon = ranunculusSampled$decimalLongitude[1:5], 
                             lat = ranunculusSampled$decimalLatitude[1:5], gstRaster = gst,
                             gslRaster = gsl, elevationRaster = gmted2010Part2, pointDf = pointsAboveTreeline,
                             elevation = ranunculusSampled$elevation[1:5], plot = FALSE, 
                             plotHist = FALSE, gstMin = 6.4, gslMin = 94)

## -----------------------------------------------------------------------------
elev

## ----eval = FALSE-------------------------------------------------------------
#  temp <- terra::rast("your_raster_layer.tif")

## ----eval = FALSE-------------------------------------------------------------
#  #Import raster of your choice
#  gst <- terra::rast(file = "your_gst_layer.tif")
#  gsl <- terra::rast(file = "your_gsl_layer.tif")
#  
#  #Compute new data frame
#  newPointsAboveTreeline <- generate_point_df(gstRaster = gst, gslRaster = gsl, stepSize = 0.0416666,
#                                              gstTreshold = 6.4, gslTreshold = 94)
#  
#  #Save the output
#  save(newPointsAboveTreeline, file = "newPointsAboveTreeline.Rdata")

## ----eval = FALSE-------------------------------------------------------------
#  #Load the polygons of all the mountains (from the GMBA project)
#  mountain_polygons <- terra::vect("GMBA mountain inventory V1.2(entire world)/
#                                   GMBA Mountain Inventory_v1.2-World.shp")
#  
#  #Keep only the points that are in a alpine polygon
#  #"keep" will be a vector containing all row numbers that contain coordinate, which lie in the Alps
#  keep <- terra::is.related(terra::vect(newPointsAboveTreeline, geom = c("longitude", "latitude")),
#                            mountain_polygons, "intersects") |> which()
#  
#  newPointsAboveTreeline2 <- newPointsAboveTreeline [keep,] #Pick the lines of interest

## -----------------------------------------------------------------------------
pointAbove <- get_nearest_point(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline)
pointAbove

## -----------------------------------------------------------------------------
grid <- generate_grid(lon = pointAbove$lon, lat = pointAbove$lat, squareSize = 10, stepSize = 0.0025)
head(grid$df)

## -----------------------------------------------------------------------------
grid$df <- classify_above_treeline(coords = grid$df, gstRaster = gst, gslRaster = gsl,
                                   gstTreshold = 6.4, gslTreshold = 94)
head(grid$df)

## -----------------------------------------------------------------------------
treelineDf <- sample_treeline(df = grid$df, lonLength = grid$lonLength, latLength = grid$latLength)
head(treelineDf)

## ----eval = FALSE-------------------------------------------------------------
#  plot_distr(nearestCorner = pointAbove, grid = grid$df, treelineDf = treelineDf, size = 12)

## -----------------------------------------------------------------------------
calculate_distance(treeline = treelineDf, elevationRaster = gmted2010Part, pointElevation = 504,
                   treelineSampling = 10, plot = FALSE)

