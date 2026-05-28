## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE)

## ---- results='hide', message=FALSE, warning=FALSE----------------------------
library(waterquality)
library(terra)
Harsha <- terra::rast(system.file("raster/S2_Harsha.tif", package = "waterquality"))

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
Harsha_Am092Bsub <- wq_calc(terraRast = Harsha, 
                            alg = "MM12NDCI", 
                            sat = "sentinel2")

## ---- fig.height = 5, fig.width = 6-------------------------------------------
terra::plot(Harsha_Am092Bsub)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
Harsha_Multiple <- wq_calc(terraRast = Harsha,
                           alg = c("MM12NDCI", "Am092Bsub", "Da052BDA"), 
                           sat = "sentinel2")

## ---- fig.height = 5, fig.width = 6-------------------------------------------
terra::plot(Harsha_Multiple) 

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
Harsha_PC <- wq_calc(Harsha,
                     alg = "chlorophyll", 
                     sat = "sentinel2")

## ---- fig.height = 5, fig.width = 6-------------------------------------------
terra::plot(Harsha_PC) 

## ---- results='hide', message=FALSE, warning=FALSE----------------------------
Harsha_All <- wq_calc(Harsha, 
                      alg = "all",
                      sat = "sentinel2")

## ----fig.height = 5, fig.width = 6--------------------------------------------
terra::plot(Harsha_All) # Only displays first 16 of 28

## ---- results='hide', message=FALSE, warning=FALSE----------------------------
library(waterquality)
library(terra)
library(tmap)
library(sf)
s2 = terra::rast(system.file("raster/S2_Harsha.tif", package = "waterquality"))
MM12NDCI = wq_calc(s2, alg = "MM12NDCI", sat = "sentinel2")
samples = terra::vect(system.file("raster/Harsha_Simple_Points_CRS.gpkg", package = "waterquality"))
lake_extent = terra::vect(system.file("raster/Harsha_Lake_CRS.gpkg", package = "waterquality"))

## ----fig.height = 5, fig.width = 6--------------------------------------------
Map_WQ_raster(WQ_raster = MM12NDCI,
              sample_points = samples,
              map_title= "Water Quality Map",
              raster_style = "quantile",
              histogram = TRUE)


## ---- eval = FALSE------------------------------------------------------------
#  #Input raster image
#  wq_raster <- terra::rast("C:/temp/my_raster.tif")
#  
#  #Input shapefile
#  wq_samples <- terra::vect('C:/temp/my_samples.shp')
#  
#  #Extract values from raster and combine with shapefile
#  waterquality_data <- data.frame(wq_samples, terra::extract(wq_raster, wq_samples))
#  
#  #Export results as csv file
#  write.csv(waterquality_data, file = "C:/temp/waterquality_data.csv")

## ---- results='hide', message=FALSE, warning=FALSE----------------------------
library(waterquality)
library(caret)
df <- read.csv(system.file("raster/waterquality_data.csv", package = "waterquality"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
extract_lm(parameter = "Chl_ugL", algorithm = "MM12NDCI", df = df)

## ---- message=FALSE, warning=FALSE--------------------------------------------
extract_lm_cv(parameter = "Chl_ugL", algorithm = "MM12NDCI",
                                       df = df, train_method = "lm", control_method = "repeatedcv",
                                       folds = 3, nrepeats = 5)

## ---- message=FALSE, warning=FALSE--------------------------------------------
# Create series of strings to be used for parameters and algorithms arguments
algorithms <- c(names(df[6:10]))
parameters <- c(names(df[3:5]))
extract_lm_cv_multi_results <- extract_lm_cv_multi(parameters = parameters, algorithms = algorithms,
                                                   df = df, train_method = "lm", control_method = "repeatedcv",
                                                   folds = 3, nrepeats = 5)
head(extract_lm_cv_multi_results)

## ---- message=FALSE, warning=FALSE--------------------------------------------
extract_lm_cv_all_results <- extract_lm_cv_all(parameters = parameters, df = df,
                                                 train_method = "lm", control_method = "repeatedcv",
                                                 folds = 3, nrepeats = 5)
head(extract_lm_cv_all_results)

