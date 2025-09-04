## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----out.width = "850px", echo=FALSE------------------------------------------
knitr::include_graphics("flowchart.png")

## ----example1, eval = FALSE---------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# # read raster of wildland fuels hazardous for long-range ember transmission
# hazard_long <- rast("long_range_hazard.tif")
# 
# # read raster of wildland fuels hazardous for short-range ember transmission
# hazard_short <- rast("short_range_hazard.tif")
# 
# # compute long-range ember exposure using the long-range hazard layer
# exposure_long <- fire_exp(hazard_long, tdist = "l")
# 
# # compute short-range ember exposure using the short-range hazard layer
# exposure_short <- fire_exp(hazard_short, tdist = "s")
# 
# # Export the results as a .tif file using terra
# writeRaster(exposure_long, "exposure_long.tif")
# writeRaster(exposure_short, "exposure_short.tif")

## ----example2, eval = FALSE---------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# 
# # PLEASE NOTE: The following code is to demonstrate an example workflow. The
# # referenced input data does not exist.
# 
# # read raster of wildland fuels hazardous for long-range ember transmission
# hazard_long <- rast("long_range_hazard.tif")
# 
# # compute long-range ember exposure using the long-range hazard layer
# exposure_long <- fire_exp(hazard_long, tdist = "l")
# 
# # read point feature vector of a single value location
# point_value <- vect("point_value.shp")
# 
# # read point feature vector of value locations
# multiple_values <- vect("multiple_values.shp")
# 
# # assess directional exposure for a single point
# transects_point <- fire_exp_dir(exposure_long, point_value)
# 
# # export the transects for the point feature as a shapefile with terra
# writeVector(transects_point, "transects_point.shp")
# 
# # loop through multiple values and export the results as shapefiles
# for (i in seq_len(nrow(multiple_values))) {
#   dir_exp_point <- fire_exp_dir(exposure_long, multiple_values[i])
#   # set a file name based on the feature row number
#   file_name <- paste("transects_point_", i, ".shp", sep = "")
#   writeVector(dir_exp_point, file_name)
# }

## ----example3, eval = FALSE---------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# # PLEASE NOTE: The following code is to demonstrate an example workflow. The
# # referenced input data does not exist.
# 
# # read raster of wildland fuels hazardous for long-range ember transmission
# hazard_long <- rast("long_range_hazard.tif")
# 
# # read raster of wildland fuels hazardous for short-range ember transmission
# hazard_short <- rast("short_range_hazard.tif")
# 
# # compute long-range ember exposure using the long-range hazard layer
# exposure_long <- fire_exp(hazard_long, tdist = "l")
# 
# # compute short-range ember exposure using the short-range hazard layer
# exposure_short <- fire_exp(hazard_short, tdist = "s")
# 
# # read polygon feature vector of an area of interest boundary
# # (e.g., a built environment)
# area_of_interest <- vect("area_of_interest.shp")
# 
# # read point feature vector of value locations
# # (e.g., structures in a community)
# multiple_values <- vect("multiple_values.shp")
# 
# # extract the underlying long-range ember exposure to values attributes
# values_exposure_long <- fire_exp_extract(exposure_long, multiple_values)
# 
# # extract the underlying short-range ember exposure to values attributes
# values_exposure_short <- fire_exp_extract(exposure_short, multiple_values)
# 
# # Visualize short-range ember exposure classes in the aoi with a summary table
# fire_exp_summary(exposure_long, area_of_interest, classify = "local")
# 
# # Visualize long-range ember exposure classes in the aoi with a map
# fire_exp_map_class(exposure_long, area_of_interest, classify = "local")
# 
# # Visualize long-range ember exposure classes to values in summary table
# fire_exp_extract_vis(values_exposure_long, classify = "local")
# 
# # Visualize long-range ember exposure classes to values in map
# fire_exp_extract_vis(values_exposure_long, classify = "local", map = TRUE)
# 
# # Repeat the visualizations for short-range ember exposure
# fire_exp_summary(exposure_short, area_of_interest, classify = "local")
# fire_exp_map_class(exposure_short, area_of_interest, classify = "local")
# fire_exp_extract_vis(values_exposure_short, classify = "local")
# fire_exp_extract_vis(values_exposure_short, classify = "local", map = TRUE)

## ----example4a, eval = FALSE--------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# # PLEASE NOTE: The following code is to demonstrate an example workflow. The
# # referenced input data does not exist.
# 
# # read long-range ember hazard raster
# hazard <- rast("hazard.tif")
# 
# # read non-burnable landscape raster
# no_burn <- rast("no_burn.tif")
# 
# # compute long-range ember exposure
# exposure <- fire_exp(hazard, tdist = "l", no_burn = no_burn)
# 
# # read the fire perimeters with terra
# fires <- vect("fires.shp")
# 
# # generate validation table, and then plot the results
# output <- fire_exp_validate(exposure, fires)
# fire_exp_validate_plot(output)

## ----example4b, eval = FALSE--------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# # PLEASE NOTE: The following code is to demonstrate an example workflow. The
# # referenced input data does not exist.
# 
# # read long-range ember hazard raster
# hazard <- rast("hazard.tif")
# 
# # read non-burnable landscape raster
# no_burn <- rast("no_burn.tif")
# 
# # compute long-range ember exposure for default transmission distance
# exposure_a <- fire_exp(hazard, tdist = "l", no_burn = no_burn)
# 
# # compute long-range ember exposure for a custom transmission distance
# exposure_b <- fire_exp(hazard, tdist = 800, no_burn = no_burn)
# 
# # read the fire perimeters with terra
# fires <- vect("fires.shp")
# 
# # validation tables for both options
# output_a <- fire_exp_validate(exposure_a, fires)
# output_b <- fire_exp_validate(exposure_b, fires)

## ----example4c, eval = FALSE--------------------------------------------------
# # Load the fireexposuR library
# library(fireexposuR)
# 
# # Load the terra library for reading/writing spatial data
# library(terra)
# 
# # PLEASE NOTE: The following code is to demonstrate an example workflow. The
# # referenced input data does not exist.
# 
# # read long-range ember hazard raster: option A
# hazard_a <- rast("hazard_a.tif")
# 
# # read non-burnable landscape raster: option A
# no_burn_a <- rast("no_burn_a.tif")
# 
# # read long-range ember hazard raster: option B
# hazard_b <- rast("hazard_b.tif")
# 
# # read non-burnable landscape raster: option B
# no_burn_b <- rast("no_burn_b.tif")
# 
# # compute long-range ember exposure for option A
# exposure_a <- fire_exp(hazard_a, tdist = "l", no_burn = no_burn_a)
# 
# # compute long-range ember exposure for option A
# exposure_b <- fire_exp(hazard_b, tdist = "l", no_burn = no_burn_b)
# 
# # read the fire perimeters with terra
# fires <- vect("fires.shp")
# 
# # validation tables for both options
# output_a <- fire_exp_validate(exposure_a, fires)
# output_b <- fire_exp_validate(exposure_b, fires)

