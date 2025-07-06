## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval = F-----------------------------------------------------------------
# library(rLakeHabitat)
# 
# #load example depth data
# depths <- read.csv("data/example_depths.csv")
# #load example outline
# outline <- vect("data/example_outline.shp")
# 
# #rarify input data -- example data won't change
# clean_depth <- rarify(outline, depths, "x", "y", "z", res = 5)
# 
# #obtain xyz point data from depth contours
# contours <- read_sf("~/UW_Research/Project5_Habitat_Mapping/Repositories/rLakeHabitat/data/example_contour.shp")
# 
# contour_depths <- contourPoints(contours, depths = "Z", geometry = "geometry", density = 50)

## ----eval = F-----------------------------------------------------------------
# #load example depth data
# depths <- read.csv("data/example_depths.csv")
# #load example outline
# outline <- vect("data/example_outline.shp")
# 
# #interpolate using Inverse Distance Weighted method
# DEM <- interpBathy(outline, depths, "x", "y", "z", zeros = F, separation = 10,
#                    crsUnits = "dd", res = 10, method = "IDW", nmax = 6, idp = 2)
# plot(DEM)
# 
# #obtain RMSE for IDW interpolation
# crossValidate(outline, depths, "x", "y", "z", zeros = F, separation = 10, k = 5,
#               crsUnits = "dd", res = 10, method = "IDW", nmax = 6, idp = 2)
# 
# #Interpolate using Ordinary Kriging method
# DEM <- interpBathy(outline, depths, "x", "y", "z", zeros = F, separation = 10,
#                    crsUnits = "dd", res = 50, nmax = 6, method = "OK", model = "Sph")
# plot(DEM)
# 
# #obtain RMSE for OK interpolation
# crossValidate(outline, depths, "x", "y", "z", zeros = F, separation = 10, k = 5,
#               crsUnits = "dd", res = 50, nmax = 6, method = "OK", model = "Sph")

## ----eval = F-----------------------------------------------------------------
# #load example temperature profile data
# thermo_data <- read.csv("data/example_profile_data.csv") %>%
#   mutate(date = as.Date(date))
# 
# #estimate average thermocline depth across sites and dates
# estThermo(thermo_data, site = "site", date = "date", depth = "depth", temp = "temp", combine = "all")
# 
# #generate hypsography data, output = 'values' or 'plot'
# calcHyps(DEM, DEMunits = 'm', depthUnits = 'ft', by = 1, output = 'values')
# 
# #calculate littoral area
# calcLittoral(DEM, secchi = 2, DEMunits = 'm', depthUnits = 'ft', by = 1)
# 
# #calculate shoreline development index
# calcSDI(DEM, units = 'm', by = 1)
# 
# #calculate volume of pelagic habitats
# calcVolume(DEM, thermo_depth = 4, DEMunits = 'm', depthUnits = 'm', by = 1)
# 
# #calculate volume of pelagic vs. littoral habitat
# littoralVol(DEM, secchi = 2, DEMunits = 'm', depthUnits = 'm', by = 1)

## ----eval = F-----------------------------------------------------------------
# #generate bathymetry map
# bathyMap(DEM, contours = T, units = "m", by = 1)
# 
# #generate animated bathymetry map of littoral area or whole waterbody area across water levels
# animBathy(DEM, units = 'm', littoral = T, secchi = 2, by = 1)

## ----eval = F-----------------------------------------------------------------
# #generate raster stack from interpolated DEM
# raster_stack <- genStack(DEM, by = 1, save = F) #don't save
# plot(raster_stack)
# 
# genStack(DEM, by = 1, save = T, file_name = "Example_stack") #save

