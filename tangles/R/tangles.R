## Function for tangling original XY coordinates
##
## This function performs spatial Anonymisation ("tangling") by applying a randomized sequence of spatial transformations to XY coordinate data.
## It is designed for situations where users want to share spatial data without revealing the actual geographic locations,
## while still allowing downstream spatial analysis.
##
## There are 3 types of transformations:
##  - Step 1: Shifting X (left/right)
##  - Step 2: Shifting Y (up/down)
##  - Step 3: Rotating about a randomly selected origin point
##
## The function works on:
##  - A matrix or data.frame of XY coordinates
##  - An sf object (POINT geometry)
##  - A terra::SpatRaster object (values and cell locations preserved and transformed)
##
## Inputs:
##  - data: A 2-column matrix, data.frame, sf POINT object, or a terra::SpatRaster
##  - depth: Number of transformations to apply (default = 3)
##  - rasterdata: If TRUE, rotations are limited to 90°, 180°, or 270° to preserve raster alignment
##  - raster_object: Set TRUE to treat input `data` as a SpatRaster
##  - saveTangles: If TRUE, saves the transformed data and detangler object to disk
##  - exportShapefile: If TRUE (and input is XY or sf), saves the tangled coordinates to a shapefile (with no CRS)
##  - path: Output directory (default is current working directory)
##
## Outputs:
##  A list with two objects:
##   1. The transformed coordinates (or raster)
##   2. A detangler list containing the transformation log and unique hash

tangles <- function(
    data = NULL,
    depth = 3,
    rasterdata = FALSE,
    raster_object = FALSE,
    saveTangles = FALSE,
    exportShapefile = FALSE,
    path = NULL
) {
  # If the user wants to save outputs but didn't supply a path,
  # default into tempdir()
  if ((saveTangles || exportShapefile) && is.null(path)) {
    path <- tempdir()}
  
  # Handle raster input
  if (raster_object) {
    tempD <- data.frame(cellNos = seq_len(terra::ncell(data)))
    vals <- as.data.frame(terra::values(data))
    tempD <- cbind(tempD, vals)
    cellNos <- tempD$cellNos
    gXY <- data.frame(terra::xyFromCell(data, cellNos))
    xyData <- as.matrix(gXY)
  } else if (inherits(data, "sf")) {
    # Handle sf input
    xyData <- sf::st_coordinates(data)
  } else {
    # Assume matrix or data.frame
    xyData <- data
  }
  
  ###### Internalised Step Functions
  
  leap_X <- function(xyData = NULL) {
    r.num <- sample(-999999:999999, 1)
    xyData[, 1] <- xyData[, 1] + r.num
    list(xyData, r.num)
  }
  
  leap_Y <- function(xyData = NULL) {
    r.num <- sample(-999999:999999, 1)
    xyData[, 2] <- xyData[, 2] + r.num
    list(xyData, r.num)
  }
  
  rotate_XY <- function(xyData = NULL) {
    row.sample <- sample(1:nrow(xyData), 1)
    origin.point <- xyData[row.sample, ]
    
    x <- t(xyData[, 1])
    y <- t(xyData[, 2])
    v <- rbind(x, y)
    
    x_center <- origin.point[1]
    y_center <- origin.point[2]
    
    center <- v
    center[1, ] <- x_center
    center[2, ] <- y_center
    
    deg <- if (rasterdata) sample(c(90, 180, 270), 1) else sample(1:359, 1)
    theta <- (deg * pi) / 180
    R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
    
    s <- v - center
    so <- R %*% s
    vo <- so + center
    
    xyData <- cbind(vo[1, ], vo[2, ])
    list(xyData, origin.point, deg)
  }
  
  ##########################################
  
  step.random <- sample(1:3, depth, replace = TRUE)
  s3.cnt <- 1
  seq.mat <- matrix(NA, nrow = depth, ncol = 6)
  
  for (i in 1:depth) {
    seq.step <- step.random[i]
    
    if (seq.step == 1) {
      step1.out <- leap_X(xyData = xyData)
      xyData <- step1.out[[1]]
      seq.mat[i, 1] <- seq.step
      seq.mat[i, 2] <- step1.out[[2]]
    }
    
    if (seq.step == 2) {
      step2.out <- leap_Y(xyData = xyData)
      xyData <- step2.out[[1]]
      seq.mat[i, 1] <- seq.step
      seq.mat[i, 2] <- step2.out[[2]]
    }
    
    if (seq.step == 3) {
      step3.out <- rotate_XY(xyData = xyData)
      xyData <- step3.out[[1]]
      seq.mat[i, 1] <- seq.step
      seq.mat[i, 3:4] <- step3.out[[2]]
      seq.mat[i, 5] <- step3.out[[3]]
      seq.mat[i, 6] <- s3.cnt
      s3.cnt <- s3.cnt + 1
    }
  }
  
  seq.dat <- as.data.frame(seq.mat)
  names(seq.dat) <- c("step", "leap_dist", "origin_X", "origin_Y", "degree", "s3_count")
  
  xyData <- as.data.frame(xyData)
  names(xyData) <- c("X", "Y")
  
  if (raster_object && exists("tempD") && nrow(xyData) != nrow(tempD)) {
    warning("Coordinate/value row count mismatch. Check raster completeness.")
  }
  
  hash.out <- digest::digest(seq.dat, "sha256")
  deTangler <- list(hash = hash.out, step_sequence = step.random, unpicker = seq.dat)
  
  if (saveTangles) {
    saveRDS(deTangler, file = file.path(path, paste0("detangler_", hash.out, ".rds")))
  }
  
  # Raster output
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
    
    if (saveTangles) {
      saveRDS(rasterOuts, file = file.path(path, paste0("tangledXY_raster_", hash.out, ".rds")))
      for (i in 1:terra::nlyr(rasterOuts)) {
        rz <- rasterOuts[[i]]
        out.name <- file.path(path, paste0("tangledXY_raster_", names(rasterOuts[[i]]), "_", hash.out, ".tif"))
        terra::writeRaster(rz, filename = out.name, overwrite = TRUE)
      }
    }
    
    return(list(rasterOuts, deTangler))
    
  } else {
    if (saveTangles) {
      saveRDS(xyData, file = file.path(path, paste0("tangledXY_", hash.out, ".rds")))
    }
    
    if (exportShapefile) {
      pts_sf <- sf::st_as_sf(xyData, coords = c("X", "Y"))
      sf::st_crs(pts_sf) <- NA  # still good practice to set this explicitly
      sf_out <- file.path(path, paste0("tangledXY_", hash.out, ".shp"))
      suppressWarnings(
        suppressMessages(
          sf::st_write(pts_sf, sf_out, delete_layer = TRUE, quiet = TRUE)
        )
      )
    }
    
    return(list(xyData, deTangler))
  }
}
