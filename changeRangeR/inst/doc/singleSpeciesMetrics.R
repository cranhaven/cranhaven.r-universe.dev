## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)

## ----message = F, warning = F-------------------------------------------------
library(changeRangeR)
library(raster)
library(sf)
library(dplyr)

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Forest_suitable_projected_coarse.tif"))
# Check that your raster is projected in meters
crs(p)
# find the number of cells that are not NA
pCells <- ncell(p[!is.na(p)])
# Convert the raster resolution to km^s
Resolution <- (res(p)/1000)^2
# Multiply the two
area <- pCells * Resolution

paste0("area = ", area[1], " km^2")

## ----EOO occs-----------------------------------------------------------------
locs <- read.csv(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/locs/10KM_thin_2017.csv"))
# Look at the first 5 rows. Not that there are three columns: Species, Longitude, Latitude
head(locs)
# Create a minimum convex polygon around the occurrences
eoo <- mcp(locs[,1:2])
# Define the coordinate reference system as unprojected
crs(eoo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
area <- area(eoo)/1000000
## area is measured in meters^2
paste0(area, " km ^2")

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Climatically_suitable_projected_coarse.tif"))
# Threshold of the minimum training presence
thr <- min(values(p), na.rm=T)
p[p<thr] <- NA
p.pts <- rasterToPoints(p)
eooSDM <- mcp(p.pts[,1:2])
aeoosdm <- area(eooSDM)/1000000
paste0(aeoosdm, " meters ^2")

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Climatically_suitable_projected_coarse.tif"))
# Using unfiltered records
locs <- read.csv(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/locs/All_localities_30n.csv"))
locs <- locs[,1:2]
p[!is.na(p)] <- 1
AOOlocs <- AOOarea(r = p, locs = locs)

print(AOOlocs)

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Climatically_suitable_projected_coarse.tif"))
# Convert to binary
p[!is.na(p)] <- 1
AOO <- AOOarea(r = p)
print(AOO)

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Forest_suitable_projected_coarse.tif"))
# Convert to binary
p[!is.na(p)] <- 1
AOO <- AOOarea(r = p)
print(AOO)

## -----------------------------------------------------------------------------
p <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/olinguitoSDM_coarse.tif"))
xy <- read.csv(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/locs/10KM_thin_2017.csv"))
ch.orig <- mcp(xy[,1:2])
thr <- 0.3380209
sf_use_s2(FALSE)
SDMeoo <- mcpSDM(p = p, xy = xy[,1:2], ch.orig = ch.orig, thr = thr)
# Check the output
SDMeoo

## ----warning = F--------------------------------------------------------------
r <- raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Forest_suitable_projected_coarse.tif"))
shp <- readRDS(file.path(system.file(package="changeRangeR"), "extdata/DemoData/shapefiles", "WDPA_COL_olinguito_simp.rds"))
# set the projections to match
shp <- spTransform(shp, CRSobj = crs(r))
# View the fields
colnames(shp@data)
# Pick the field you are interested in
field <- "DESIG_ENG"
category <- unique(shp$DESIG_ENG)
ratio.Overlap <- ratioOverlap(r = r, shp = shp, field = field, category = category, subfield = F)
# Look at the range that is protected
plot(ratio.Overlap$maskedRange[[1]])
# The proportion of the range that is protected
ratio.Overlap$ratio

## -----------------------------------------------------------------------------
# Load shapefile
PA <- readRDS(file.path(system.file(package="changeRangeR"), "extdata/DemoData/shapefiles/vn", "VN_NRs_simp.rds"))
# load raster
r <- stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/franLang"), pattern = "\\.tif$", full.names = T))
# Assume PA's will not change, so make list of current protectes areas
futures <- list(PA, PA)
# create list of rasters for example
r <- raster::unstack(r)
# supply names for r and futures
r.names <- c("BCC.2040.ssp2", "BCC.2060.ssp2")
futures.names <- c("PA1", "PA2")
# Define shapefile field and category
field <- "DESIG_ENG"
category <- "All"
# Calculate the overlap for each time period
future.ratios <- futureOverlap(r = r, futures = futures, field = field, category = category, futures.names = futures.names, r.names = r.names)
## Plot
# Create list of years from which landcover comes
years <- c(2040, 2060)
# Plot
plot(x = years, y = future.ratios[,2], type = "b", main = "Percent of SDM predicted to be protected")

## -----------------------------------------------------------------------------
binaryRange <- raster::raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Climatically_suitable_projected_coarse.tif"))
rStack <- raster::stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/MODIS"), pattern = "\\.tif$", full.names = T))
rStack <- raster::projectRaster(rStack, binaryRange, method = "bilinear")
threshold <- 50.086735

SDM.time <- envChange(rStack = rStack, binaryRange = binaryRange, threshold = threshold, bound = "lower")

years <- c("2005", "2006", "2008", "2009")

SDM.time$Area

plot(y = SDM.time$Area, x = years, main = "SDM area change", ylab = "area (square m)")
lines(y = SDM.time$Area, x = years)

