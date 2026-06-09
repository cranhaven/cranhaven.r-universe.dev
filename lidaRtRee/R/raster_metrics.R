# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' Computes metrics by aggregating a raster at lower resolution or summarizing
#' attributes based on XY locations
#'
#' Computes statistics by aggregating a raster at lower resolution. Aggregation
#' groups are larger cells, new values are computed by applying a user-specified
#' function to original cells contained in the larger cells. Results are provided
#' as a data.frame with the XY coordinates of the larger cells, or as SpatRaster.
#'
#' @param r SpatRaster object, data.frame with xy coordinates in two first columns, or POINT \code{\link[sf]{sf}} spatial object
#' @param res numeric. Resolution of the aggregation raster, should be a multiple
#' of r resolution if a raster is provided
#' @param fun function. Function to compute metrics in each aggregated cell from
#' the values contained in the initial raster (use x$layer to access raster
#' values) / data.frame (use x$colum_name to access values)
#' @param start	vector of x and y coordinates for the reference raster. Default is (0,0) meaning that the grid aligns on (0,0). 
#' @param output string. indicates the class of output object "raster" for a SpatRaster or "data.frame"
#' @return a data.frame with the XY center coordinates of the aggregated cells,
#' and the values computed with the user-specified function, or a SpatRaster object
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#' # replace NA with zeros
#' chm_chablais3[is.na(chm_chablais3)] <- 0
#'
#' # raster metrics from raster
#' metrics1 <- raster_metrics(chm_chablais3, res = 10)
#' metrics1
#'
#' # raster metrics from data.frame
#' n <- 1000
#' df <- data.frame(
#'   x = runif(n, 0, 100), y = runif(n, 0, 100), z1 = runif(n, 0, 1),
#'   z2 = runif(n, 10, 20)
#' )
#' # compute raster metrics
#' metrics2 <- raster_metrics(df,
#'   res = 10,
#'   fun = function(x) {
#'     data.frame(max.z = max(x$z1), max.sum = max(x$z1 + x$z2))
#'   },
#'   output = "data.frame"
#' )
#' summary(metrics2)
#'
#' # display raster metrics
#' terra::plot(metrics1)
#' # display data.frame metrics
#' terra::plot(terra::rast(metrics2, type = "xyz"))
#' @export
raster_metrics <-
  function(r,
           res = 20,
           fun = function(x) {
             data.frame(mean = mean(x[, 3]), sd = stats::sd(x[, 3]))
           },
           start = c(0, 0),
           output = "raster") {
    if (inherits(r, "SpatRaster")) {
      # convert to data.frame
      st <- terra::as.points(r, na.rm = FALSE, na.all = FALSE)
      st <- cbind(terra::geom(st)[, c("x", "y")], as.data.frame(st))
      # backup crs
      projinfo <- terra::crs(r)
    } else {
      if (inherits(r, "sf")) {
        # convert to data.frame
        st <- cbind(data.frame(sf::st_coordinates(r)), sf::st_drop_geometry(r))
        # backup crs
        projinfo <- sf::st_crs(r)$wkt
      } else {
        st <- r
        projinfo <- NA
      }
    }
    # return NULL if empty
    if (nrow(st) == 0) return(NULL)
    # compute coordinates of new cell center at metrics resolution
    dummy <- data.frame(X = round((st[, 1] - start[1] - res / 2) / res) * res + start[1] + res / 2, 
                        Y = round((st[, 2] - start[2]- res / 2) / res) * res + start[2] + res / 2)
    # compute metrics by grouping factor
    dummy <- lapply(split(st, list(dummy$X, dummy$Y), sep = "_"), FUN = fun)
    # convert to data.frame
    dummy <- as.data.frame(do.call(rbind, dummy))
    #
    dummy <- cbind(matrix(as.numeric(unlist(strsplit(row.names(dummy), "_"))), ncol = 2, byrow = TRUE), dummy)
    names(dummy)[1:2] <- c("X", "Y")
    # add id column or coordinates
    if (output == "raster") {
      # create additional row and column
      # - to avoid raster with only one row or column
      # (an error is returned by rast with "xyz" when only one row or column)
      # - to force lowest distance between two existing cell = resolution
      # (a raster with yres = X * xres might be created)
      # A TEST COULD BE ADDED TO PERFORMED THIS OPERATION ONLY IF REQUIRED ?
      # 
      # duplicate last line
      dummy <- rbind(dummy[1,], dummy)
      # move additional line to one cell low left
      dummy[1, c("X", "Y")] <- c(min(dummy$X) - res, min(dummy$Y) - res)
      # rasterize
      dummy <- terra::rast(dummy, type = "xyz", crs = projinfo)
      extent <- terra::ext(dummy)
      # remove bottom row and left column
      dummy <- terra::crop(dummy, terra::ext(extent$xmin + res, extent$xmax, extent$ymin + res, extent$ymax))
    }
    dummy
  }
