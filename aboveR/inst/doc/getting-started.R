## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(aboveR)
library(terra)
library(sf)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("aboveR")

## -----------------------------------------------------------------------------
before <- rast(system.file("extdata/dem_before.tif", package = "aboveR"))
after  <- rast(system.file("extdata/dem_after.tif", package = "aboveR"))

## -----------------------------------------------------------------------------
change <- terrain_change(before, after)
plot(change[["change"]], main = "Elevation Change (m)")

## -----------------------------------------------------------------------------
boundary <- st_read(
  system.file("extdata/boundary.gpkg", package = "aboveR"),
  quiet = TRUE
)
vol <- estimate_volume(after, before, boundary)
cat("Cut volume: ", round(vol$cut_volume_m3), "m3\n")
cat("Fill volume:", round(vol$fill_volume_m3), "m3\n")
cat("Net change: ", round(vol$net_volume_m3), "m3\n")

## -----------------------------------------------------------------------------
line <- st_read(
  system.file("extdata/profile_line.gpkg", package = "aboveR"),
  quiet = TRUE
)
prof <- terrain_profile(before, line)
plot(prof$distance, prof$elevation, type = "l",
     xlab = "Distance (m)", ylab = "Elevation (m)",
     main = "Terrain Profile")

## -----------------------------------------------------------------------------
rough <- surface_roughness(before, window = 5)
plot(rough, main = "Surface Roughness")

## ----eval = FALSE-------------------------------------------------------------
# # Find DEM tiles for an area of interest
# tiles <- kfa_find_tiles(
#   aoi = c(-84.55, 37.95, -84.45, 38.05),
#   product = "dem",
#   phase = 2
# )
# 
# # Read and mosaic DEM tiles
# dem <- kfa_read_dem(
#   aoi = c(-84.55, 37.95, -84.45, 38.05),
#   phase = 2
# )

