## Function for untangling transformed spatial coordinates or raster data
##
## This function reverses the Anonymisation transformations applied by the `tangles()` function.
## It uses the accompanying detangler object, which stores the random sequence of transformations and parameters used.
## The function can restore either point coordinates or raster layers to their original form, depending on the input.
##
## Inputs:
##  - data: Either a 2-column matrix or data.frame of transformed XY coordinates,
##          an `sf` POINT object, or a `terra::SpatRaster` object
##  - tanglerInfo: A detangler object returned by `tangles()` containing transformation metadata and hash key
##  - raster_object: Logical; TRUE if input is a raster object
##  - stub: Character string used for naming output files (optional)
##  - hash_key: Character string used to confirm that the detangler matches the transformed data
##  - saveTangles: Logical; if TRUE, saves output to `.rds` and optionally `.tif` or shapefile
##  - exportShapefile: Logical; if TRUE, exports untangled point data to shapefile (no CRS assigned)
##  - path: Character string indicating directory to write outputs (default is current working directory)
##
## Output:
##  - Either a data.frame of untangled XY coordinates or a `SpatRaster` object
##  - Files saved to disk if `saveTangles` or `exportShapefile` is TRUE

detangles <- function(
    data = NULL,
    tanglerInfo = NULL,
    raster_object = FALSE,
    stub = NULL,
    hash_key = NULL,
    saveTangles = FALSE,
    exportShapefile = FALSE,
    path = NULL
) {
  # If the user wants to save outputs but didn't supply a path,
  # default into tempdir()
  if ((saveTangles || exportShapefile) && is.null(path)) {
    path <- tempdir()}
  
  ## Validate detangler identity
  if (as.character(tanglerInfo$hash) != hash_key) {
    stop("ERROR: detangler object does not match these de-identified data")
  }
  
  ## Prepare coordinate data from raster, sf or matrix
  if (raster_object) {
    tempD <- data.frame(cellNos = seq_len(terra::ncell(data)))
    vals <- as.data.frame(terra::values(data))
    tempD <- cbind(tempD, vals)
    cellNos <- tempD$cellNos
    gXY <- data.frame(terra::xyFromCell(data, cellNos))
    xyData <- as.matrix(gXY)
  } else if (inherits(data, "sf")) {
    xyData <- sf::st_coordinates(data)
  } else {
    xyData <- data
  }
  
  ## Internal function for reversing X shift
  leap_Xba <- function(xyData, r.num) {
    xyData[, 1] <- xyData[, 1] - r.num
    xyData
  }
  
  ## Internal function for reversing Y shift
  leap_Yba <- function(xyData, r.num) {
    xyData[, 2] <- xyData[, 2] - r.num
    xyData
  }
  
  ## Internal function for reversing rotation
  rotate_XYba <- function(xyData, deg, origin.point) {
    x <- t(xyData[, 1])
    y <- t(xyData[, 2])
    v <- rbind(x, y)
    
    x_center <- origin.point[1]
    y_center <- origin.point[2]
    
    center <- v
    center[1, ] <- x_center
    center[2, ] <- y_center
    
    bdeg <- 360 - deg
    theta <- (bdeg * pi) / 180
    R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
    
    s <- v - center
    so <- R %*% s
    vo <- so + center
    
    xyData <- cbind(vo[1, ], vo[2, ])
    xyData
  }
  
  ## Apply reverse transformation sequence in reverse order
  for (i in seq_len(nrow(tanglerInfo$unpicker))) {
    jp <- nrow(tanglerInfo$unpicker) - (i - 1)
    step <- tanglerInfo$unpicker$step[jp]
    
    if (step == 1) {
      xyData <- leap_Xba(xyData, tanglerInfo$unpicker$leap_dist[jp])
    } else if (step == 2) {
      xyData <- leap_Yba(xyData, tanglerInfo$unpicker$leap_dist[jp])
    } else if (step == 3) {
      xyData <- rotate_XYba(
        xyData,
        deg = tanglerInfo$unpicker$degree[jp],
        origin.point = c(
          tanglerInfo$unpicker$origin_X[jp],
          tanglerInfo$unpicker$origin_Y[jp]
        )
      )
    }
  }
  
  ## Finalize coordinates
  xyData <- as.data.frame(xyData)
  names(xyData) <- c("X", "Y")
  hash.out <- tanglerInfo$hash
  
  ## Handle raster reconstruction
  if (raster_object) {
    tDat <- cbind(xyData, tempD)
    
    if (ncol(tDat) > 4) {
      value_cols <- names(tDat)[-(1:3)]
      r_list <- lapply(value_cols, function(col) {
        xyz <- tDat[, c("X", "Y", col)]
        names(xyz) <- c("x", "y", "value")
        terra::rast(x = xyz, type = "xyz")
      })
      rasterOuts <- do.call(c, r_list)
      names(rasterOuts) <- value_cols
    } else {
      xyz <- tDat[, c(1, 2, 4)]
      names(xyz) <- c("x", "y", "value")
      rasterOuts <- terra::rast(x = xyz, type = "xyz")
    }
    
    ## Save raster outputs
    if (saveTangles) {
      saveRDS(rasterOuts, file = file.path(path, paste0("detangledXY_raster.rds")))
      for (i in seq_len(terra::nlyr(rasterOuts))) {
        out.name <- file.path(path, paste0("detangledXY_raster_", names(rasterOuts[[i]]), ".tif"))
        terra::writeRaster(rasterOuts[[i]], filename = out.name, overwrite = TRUE)
      }
    }
    
    return(list(rasterOuts))
    
  } else {
    ## Optionally save detangled points to .rds
    if (saveTangles) {
      nm <- file.path(path, paste0("detangledXY_", stub, "_", hash.out, ".rds"))
      saveRDS(xyData, file = nm)
    }
    
    ## Optionally export to shapefile (with NA CRS)
    if (exportShapefile) {
      pts_sf <- sf::st_as_sf(xyData, coords = c("X", "Y"))
      sf::st_crs(pts_sf) <- NA  # intentionally unspecified
      sf_out <- file.path(path, paste0("detangledXY_", stub, "_", hash.out, ".shp"))
      suppressWarnings(suppressMessages(
        sf::st_write(pts_sf, sf_out, delete_layer = TRUE, quiet = TRUE)
      ))
    }
    
    return(xyData)
  }
}
