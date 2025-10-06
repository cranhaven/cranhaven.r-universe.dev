## ----setup_reset, include=FALSE-----------------------------------------------
# Save original state
old_opts    <- options()
old_par     <- par(no.readonly = TRUE)
old_wd      <- getwd()
orig_search <- search()

# On exit, restore state and detach any new packages
on.exit({
  options(old_opts)
  par(old_par)
  setwd(old_wd)
  new_pkgs <- setdiff(search(), orig_search)
  for (pkg in new_pkgs) {
    if (grepl("^package:", pkg)) {
      detach(pkg, character.only = TRUE, unload = TRUE)
    }
  }
}, add = TRUE)

## ----libraries, echo=TRUE, message=FALSE, warning=FALSE-----------------------
# Load required libraries
library(tangles)
library(digest)
library(terra)
library(sf)

# Load point data
data("HV_subsoilpH")

# Load raster data from files
ext_path <- system.file("extdata", package = "tangles")
rast.files <- list.files(path = ext_path, full.names = TRUE)
rasters <- terra::rast(rast.files)

## ----tangles-point, echo=TRUE, message=FALSE, warning=FALSE-------------------
xyData <- as.matrix(HV_subsoilpH[, 1:2])
tangles.out <- tangles(
  data = xyData,
  depth = 3,
  rasterdata = FALSE,
  raster_object = FALSE,
  saveTangles = TRUE,
  exportShapefile = TRUE,
  path = tempdir()
)

# Using sf input
df <- HV_subsoilpH[, 1:2]
sf_pts <- st_as_sf(df, coords = c("X", "Y"))
tangles.sf.out <- tangles(
  data = sf_pts,
  depth = 3,
  saveTangles = TRUE,
  exportShapefile = TRUE,
  path = tempdir()
)

## ----tangles-raster, echo=TRUE, message=FALSE, warning=FALSE------------------
tangles.ras.out <- tangles(
  data = rasters,
  depth = 3,
  rasterdata = TRUE,
  raster_object = TRUE,
  saveTangles = TRUE,
  path = tempdir()
)

## ----tangling-together, echo=TRUE, message=FALSE, warning=FALSE---------------
# 1. Tangling the point data
xyData <- as.matrix(HV_subsoilpH[, 1:2])
tangles.out <- tangles(
  data = xyData,
  depth = 4,
  rasterdata = TRUE,
  raster_object = FALSE,
  saveTangles = FALSE
)

# 2. Tangling the raster data using the same detangler
tangler.out <- tangler(
  data = rasters,
  tanglerInfo = tangles.out[[2]],
  raster_object = TRUE,
  stub = "combined",
  saveTangles = FALSE
)

# 3. Convert points to sf objects
original_pts <- st_as_sf(HV_subsoilpH, coords = c("X", "Y"))
tangled_pts <- st_as_sf(as.data.frame(tangles.out[[1]]), coords = c("X", "Y"))

# 4. Plot both
par(mfrow = c(1, 2))
plot(rasters[[1]], main = "Original Raster + Points")
plot(original_pts, add = TRUE, pch = 16, col = "blue")

plot(tangler.out[[1]][[1]], main = "Tangled Raster + Points")
plot(tangled_pts, add = TRUE, pch = 16, col = "red")
par(mfrow = c(1, 1))

## ----detangles, echo=FALSE, eval=FALSE, message=FALSE, warning=FALSE----------
#  # Detangle points
#  detangled_points <- detangles(
#    data = tangles.out[[1]],
#    tanglerInfo = tangles.out[[2]],
#    raster_object = FALSE,
#    stub = "demo_points",
#    hash_key = tangles.out[[2]]$hash,
#    saveTangles = TRUE,
#    path = tempdir()
#  )
#  
#  # Detangle rasters
#  detangled_rasters <- detangles(
#    data = tangler.out[[1]],
#    tanglerInfo = tangles.out[[2]],
#    raster_object = TRUE,
#    stub = "demo_raster",
#    hash_key = tangles.out[[2]]$hash,
#    saveTangles = TRUE,
#    path = tempdir()
#  )

