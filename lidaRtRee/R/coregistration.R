# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' Raster corresponding to circle extent
#'
#' Creates an empty raster which extents corresponds to the circle specified by 
#' center coordinates, radius and optional buffer size.
#'
#' @param X numeric. easting coordinate of plot center in meters
#' @param Y numeric. northing coordinate of plot center in meters
#' @param radius numeric. plot radius in meters
#' @param resolution numeric. raster resolution in meters
#' @param buffer numeric. buffer to be added to plot radius in meters
#' @param ... other parameters to pass to \code{\link[terra]{rast}} (e.g. crs)
#' @return A SpatRaster object
#' @examples
#' circle2Raster(100, 100, 20, 1, 5)
#' @export
circle2Raster <- function(X, Y, radius, resolution = 0.5, buffer = 0.5, ...) {
  terra::rast(xmin = floor((X - radius - buffer) / resolution) * resolution,
              ymin = floor((Y - radius - buffer) / resolution) * resolution,
              xmax = ceiling((X + radius + buffer) / resolution) * resolution,
              ymax = ceiling((Y + radius + buffer) / resolution) * resolution,
              resolution = resolution,
              ...)
}

#-------------------------------------------------------------------------------
#' Correlation between two rasters
#'
#' computes correlation between two rasters, based on the extent of the smallest 
#' one.
#'
#' @param raster_b SpatRaster. raster to correlate with largest extent
#' @param raster_s SpatRaster. raster to correlate with smallest extent
#' @param mask SpatRaster. mask of area to correlate
#' @param small.SC boolean. is the small raster already standardized and 
#' centered ?
#' @seealso \code{\link{rasters_moving_cor}} to compute correlation between rasters
#' for different translations
#' @return A numeric
#' @examples
#' # create raster
#' r_b <- terra::rast(xmin = 0, xmax = 40, ymin =0 , ymax = 40,
#' resolution = 1, crs = NA)
#' xy <- terra::xyFromCell(r_b, 1:(nrow(r_b) * ncol(r_b)))
#'
#' # add Gaussian surface and noise
#' z <- 3 * exp(-((xy[, 1] - 20)^2 + (xy[, 2] - 20)^2 / 2) / 6)
#' r_b <- terra::rast(cbind(xy, z), type = "xyz")
#'
#' # create circular mask of radius 5
#' z_mask <- (xy[, 1] - 20)^2 + (xy[, 2] - 20)^2 < 5^2
#' r_mask <- terra::rast(cbind(xy, z_mask), type = "xyz")
#'
#' # create small raster of size 20
#' r_s <- terra::crop(r_b, terra::ext(c(10, 30, 10, 30)))
#'
#' # add noise to small raster
#' terra::values(r_s) <- terra::values(r_s) + rnorm(ncol(r_s) * nrow(r_s), 0, 0.5)
#' r_mask <- terra::crop(r_mask, terra::ext(c(10, 30, 10, 30)))
#'
#' # compute correlation on masked area where signal to noise ratio is lower
#' rasters2Cor(r_b, r_s, r_mask, small.SC = FALSE)
#'
#' # compute correlation for whole small raster
#' rasters2Cor(r_b, r_s, small.SC = FALSE)
#'
#' # display large raster
#' terra::plot(r_b, main = "Large raster")
#' # display small raster
#' terra::plot(r_s, main = "Small raster")
#' # display mask
#' terra::plot(r_mask, main = "Computation mask")
#' @export
rasters2Cor <- function(raster_b, raster_s, mask = NULL, small.SC = TRUE) {
  # crop large raster to smaller size
  raster_b <- terra::crop(raster_b, raster_s)
  # apply raster mask if existing
  if (!is.null(mask)) {
    # apply raster mask to small raster if not already centered / standardized
    if (small.SC == F) {
      raster_s <- raster_s * mask
    }
    raster_b <- raster_b * mask
  }
  # center small raster if not already done
  if (small.SC == F) {
    # compute standard deviation
    sd.raster_s <- stats::sd(terra::values(raster_s), na.rm = TRUE)
    # compute mean
    m.raster_s <- mean(terra::values(raster_s), na.rm = TRUE)
    # center values of small raster
    raster_s <- raster_s - m.raster_s
  } else {
    sd.raster_s <- 1
  }
  # compute standard deviation of large raster
  sd.raster_b <- stats::sd(terra::values(raster_b), na.rm = TRUE)
  # compute mean of large raster
  m.raster_b <- mean(terra::values(raster_b), na.rm = TRUE)
  # center large raster
  raster_b <- raster_b - m.raster_b
  #
  # compute correlation value
  mean(terra::values(raster_b * raster_s), na.rm = TRUE) / (sd.raster_s * sd.raster_b)
}
#
#-------------------------------------------------------------------------------
#' Correlation between rasters for different XY translations
#'
#' computes correlation between two rasters for different XY translations. The 
#' correlation values are computed on the extent of the smallest raster using 
#' \code{\link{rasters2Cor}}, after applying an optional mask, and for each 
#' translation within a buffer area.
#'
#' @param raster_b SpatRaster. raster to correlate with largest extent
#' @param raster_s SpatRaster. raster to correlate with smallest extent
#' @param mask SpatRaster. mask of area to correlate, applied to small raster
#' @param buffer numeric. radius of the circular buffer area for possible 
#' translations
#' @param step numeric. increment step of translations within buffer area to 
#' compute correlation values, should be a multiple of raster resolution
#' @seealso \code{\link{raster_local_max}} to extract local maximum of resulting 
#' correlation raster, \code{\link{rasters2Cor}}
#' @return A SpatRaster. Raster value at coordinates x,y correspond to the correlation 
#' between the large raster and the small raster when small raster center has 
#' been translated of (x,y)
#' @examples
#' # create raster
#' r_b <- terra::rast(xmin = 0, xmax = 40, ymin =0 , ymax = 40,
#' resolution = 1, crs = NA)
#' xy <- terra::xyFromCell(r_b, 1:(nrow(r_b) * ncol(r_b)))
#'
#' # add Gaussian surfaces
#' z1 <- 1.5 * exp(-((xy[, 1] - 22)^2 + (xy[, 2] - 22)^2 / 2) / 5)
#' z2 <- exp(-((xy[, 1] - 20)^2 + (xy[, 2] - 22)^2 / 2) / 3)
#' z3 <- 1.5 * exp(-((xy[, 1] - 17)^2 + (xy[, 2] - 17)^2 / 2) / 5)
#' r_b <- terra::rast(cbind(xy, z1 + z2 + z3), type = "xyz")
#'
#' # create small raster
#' r_s <- terra::crop(r_b, terra::ext(c(15, 25, 15, 25)))
#' # offset raster by (-2, -2)
#' terra::ext(r_s) <- c(13, 23, 13, 23)
#'
#' # compute correlations for translations inside buffer
#' rr <- rasters_moving_cor(r_b, r_s, buffer = 6, step = 1)
#' rr
#'
#' # display large raster
#' terra::plot(r_b, main = "Large raster")
#' # display small raster
#' terra::plot(r_s, main = "Small raster")
#' # display correlation
#' terra::plot(rr,
#'   xlab = "X translation", ylab = "Y translation",
#'   main = "Correlation between rasters"
#' )
#' @export
rasters_moving_cor <- function(raster_b, raster_s, mask = NULL, buffer = 19, step = 0.5) {
  # create data.frame for results
  resultat <- data.frame(xoffset = numeric(), yoffset = numeric(), correlation = numeric())
  # initialize output line
  n <- 0
  # perform computation on small raster
  # apply mask
  if (!is.null(mask)) {
    raster_s <- raster_s * mask
  }
  # center and standardize small raster
  raster_s <- raster_s - mean(terra::values(raster_s), na.rm = TRUE)
  raster_s <- raster_s / stats::sd(terra::values(raster_s), na.rm = TRUE)
  #
  # compute squared radius
  buff2 <- buffer^2
  # copy small raster before offsetting
  # use terra::rast to make a deep copy
  matrice2 <- terra::deepcopy(raster_s)
  # copy mask before offsetting
  if (!is.null(mask)) {
    mask2 <- terra::deepcopy(mask)
  } else {
    mask2 <- NULL
  }
  # loop on X offset
  for (xoffset in seq(from = -buffer, to = buffer, by = step))
  {
    # modify small raster X extent
    terra::ext(matrice2)$xmin <- terra::ext(raster_s)$xmin + xoffset
    terra::ext(matrice2)$xmax <- terra::ext(raster_s)$xmax + xoffset
    # modify mask X extent
    if (!is.null(mask)) {
      terra::ext(mask2)$xmin <- terra::ext(mask)$xmin + xoffset
      terra::ext(mask2)$xmax <- terra::ext(mask)$xmax + xoffset
    }
    # loop on Y offset
    for (yoffset in seq(from = -buffer, to = buffer, by = step))
    {
      # check if new position is inside the search buffer
      if (yoffset^2 + xoffset^2 <= buff2) {
        # modify small raster Y extent
        terra::ext(matrice2)$ymin <- terra::ext(raster_s)$ymin + yoffset
        terra::ext(matrice2)$ymax <- terra::ext(raster_s)$ymax + yoffset
        # modify mask Y extent
        if (!is.null(mask)) {
          terra::ext(mask2)$ymin <- terra::ext(mask)$ymin + yoffset
          terra::ext(mask2)$ymax <- terra::ext(mask)$ymax + yoffset
        }
        # increment line
        n <- n + 1
        # compute correlation and record results
        resultat[n, ] <- c(xoffset, yoffset, rasters2Cor(raster_b, matrice2, mask2, T))
      }
    }
  }
  # convert results to raster
  terra::rast(resultat, type = "xyz")
}

#-------------------------------------------------------------------------------
#' Statistics of raster local maximum
#'
#' identifies global maximum and second global maximum from raster (e.g. output 
#' from \code{\link{rasters_moving_cor}}), and computes related statistics. Local 
#' maxima can be excluded based on a minimum distance \code{dm} to nearest local 
#' maximum.
#'
#' @param r SpatRaster. typically output of \code{\link{rasters_moving_cor}}
#' @param dm numeric. minimum distance between two local maxima in meters
#' @param med1 numeric. window radius to compute median value around the maximum 
#' position (default: 1m)
#' @param med2 numeric. window radius #2 to compute median value around the 
#' maximum position (default: 2m)
#' @param quanta numeric. quantile value to compute for raster values (default: 
#' 3rd quartile)
#' @param quantb numeric. quantile #2 value to compute for raster values 
#' (default: median)
#' @seealso \code{\link{rasters_moving_cor}}, \code{\link{coregistration}} for 
#' application to the coregistration of tree inventory data with canopy height 
#' models
#' @return A data.frame with value of maximum, position of maximum, position of 
#' second maximum, ratio of max value to 2nd max, ratio of max value to median 
#' of neighborhood (size1 and size 2), ratio of max value to raster quantiles 1 
#' and 2
#' @examples
#' # create raster
#' r_b <- terra::rast(xmin = 0, xmax = 40, ymin =0 , ymax = 40,
#' resolution = 1, crs = NA)
#' xy <- terra::xyFromCell(r_b, 1:(nrow(r_b) * ncol(r_b)))
#'
#' # add Gaussian surfaces
#' z1 <- 1.5 * exp(-((xy[, 1] - 22)^2 + (xy[, 2] - 22)^2 / 2) / 5)
#' z2 <- exp(-((xy[, 1] - 20)^2 + (xy[, 2] - 22)^2 / 2) / 3)
#' z3 <- 1.5 * exp(-((xy[, 1] - 17)^2 + (xy[, 2] - 17)^2 / 2) / 5)
#' r_b <- terra::rast(cbind(xy, z1 + z2 + z3), type = "xyz")
#'
#' # create small raster
#' r_s <- terra::crop(r_b, terra::ext(c(15, 25, 15, 25)))
#' # offset raster by (-2, -2)
#' terra::ext(r_s) <- c(13, 23, 13, 23)
#'
#' rr <- rasters_moving_cor(r_b, r_s, buffer = 6, step = 1)
#' loc_max <- raster_local_max(rr)
#' loc_max
#'
#' # plot raster
#' terra::plot(rr)
#' # add location of two local maxima
#' graphics::points(loc_max[1, c("dx1", "dx2")], loc_max[1, c("dy1", "dy2")],
#'   cex = c(1, 0.5), pch = 3
#' )
#' @export
raster_local_max <- function(r, dm = 2, med1 = 1, med2 = 2, quanta = 0.75, quantb = 0.5) {
  # check if raster is valid
  if (nrow(r) * ncol(r) <= 1 | max(terra::values(r), na.rm = TRUE) == -Inf) return(NA)
  # replace NA values with -Inf
  dummy <- terra::deepcopy(r)
  dummy[is.na(dummy)] <- -Inf
  # maxima detection
  maxi <- maxima_detection(dummy, jitter = FALSE)
  dummy[] <- 1
  # maxima selection, without value criteria
  maxi <- maxima_selection(maxi, dummy, hmin = 0, dmin = dm, dprop = 0)
  # set back values in maxi to correlation values
  maxi <- (maxi > 0) * r
  # indice of global max
  pixel1 <- terra::which.max(terra::values(maxi))
  # max value
  max1 <- r[pixel1[1]]
  # coordinates of global max
  coord1 <- terra::xyFromCell(maxi, pixel1[1])
  # erase first max
  maxi[pixel1] <- NA
  # second maximum
  pixel2 <- terra::which.max(terra::values(maxi))
  max2 <- r[pixel2[1]]
  coord2 <- terra::xyFromCell(maxi, pixel2[1])
  # median around global max
  # half resolution
  hres <- terra::res(r)[1] / 2
  # median around maximum, neighborhood 1
  medloc1 <- stats::median(terra::values(terra::crop(r, terra::ext(c(
    coord1[1] - med1 - hres,
    coord1[1] + med1 + hres,
    coord1[2] - med1 - hres,
    coord1[2] + med1 + hres
  )))), na.rm = TRUE)
  # median around maximum, neighborhood 2
  medloc2 <- stats::median(terra::values(terra::crop(r, terra::ext(c(
    coord1[1] - med2 - hres,
    coord1[1] + med2 + hres,
    coord1[2] - med2 - hres,
    coord1[2] + med2 + hres
  )))), na.rm = TRUE)
  # stats of correlation raster
  quant_a <- stats::quantile(r, quanta, na.rm = TRUE)
  quant_b <- stats::quantile(r, quantb, na.rm = TRUE)
  # store results into data.frame
  data.frame(max1 = max1, 
             dx1 = coord1[1], 
             dy1 = coord1[2], 
             dx2 = coord2[1], 
             dy2 = coord2[2], 
             ratiomax1max2 = as.numeric(max1 / max2), 
             rmedloc1 = as.numeric(max1 / medloc1), 
             rmedloc2 = as.numeric(max1 / medloc2), 
             rquanta = as.numeric(max1 / quant_a), 
             rquantb = as.numeric(max1 / quant_b))
}

#-------------------------------------------------------------------------------
#' Tree inventory and canopy height model coregistration
#'
#' Computes the correlation between the canopy height model and a virtual canopy 
#' height model simulated from tree locations, for different translations of tree 
#' inventory positions, and outputs the translation corresponding to best 
#' estimated co-registration.
#'
#' @param chm raster. canopy height model
#' @param trees data.frame. the first two columns contain xy coordinates, and 
#' the third is the value to correlate to the chm (e.g. tree heights or diameters)
#' @param mask raster. raster mask of tree inventory area
#' @param buffer numeric. radius of the circular buffer area of possible translations
#' @param step numeric. increment step of translations within buffer area to 
#' compute correlation values, should be a multiple of raster resolution
#' @param dm numeric. minimum distance between two local maxima in meters
#' @param plot boolean. whether to display the results or not
#' @seealso \code{\link{rasters_moving_cor}}, \code{\link{raster_local_max}}
#' @return A list with two elements : first the correlation SpatRaster returned by 
#' \code{\link{rasters_moving_cor}}, second a data.frame returned by 
#' \code{\link{raster_local_max}}
#' @references Monnet, J.-M. and Mermin, E. 2014. Cross-Correlation of Diameter 
#' Measures for the Co-Registration of Forest Inventory Plots with Airborne Laser 
#' Scanning Data. Forests 2014, 5(9), 2307-2326, \doi{10.3390/f5092307}
#' @examples
#' # tree inventory
#' trees <- data.frame(x = c(22.2, 18.3, 18.1), y = c(22.1, 22.7, 18.4), 
#' z = c(15, 10, 15))
#'
#' # mask of inventory area
#' # empty raster with extent
#' tree_mask <- circle2Raster(20, 20, 9, resolution = 1)
#' # fill binary mask
#' tree_mask <- raster_xy_mask(rbind(c(20, 20), c(20, 20)), c(9, 9), tree_mask, 
#' binary = TRUE)
#'
#' # simulate chm raster
#' chm <- terra::rast(extent = c(0, 40, 0, 40), resolution = 1, crs = NA)
#' xy <- terra::xyFromCell(chm, 1:(ncol(chm) * nrow(chm)))
#'
#' # add Gaussian surfaces to simulate tree crowns
#' z1 <- trees$z[1] * exp(-((xy[, 1] - trees$x[1])^2 + (xy[, 2] - trees$y[1])^2 / 2) * trees$z[1] / 50)
#' z2 <- trees$z[2] * exp(-((xy[, 1] - trees$x[2])^2 + (xy[, 2] - trees$y[2])^2 / 2) * trees$z[2] / 50)
#' z3 <- trees$z[3] * exp(-((xy[, 1] - trees$x[3])^2 + (xy[, 2] - trees$y[3])^2 / 2) * trees$z[3] / 50)
#' chm <- terra::rast(cbind(xy, pmax(z1, z2, z3)), type = "xyz") #+rnorm(length(z1),0,1)))
#'
#' # translate trees
#' trees$x <- trees$x + 1
#' trees$y <- trees$y + 2
#'
#' coreg <- coregistration(chm, trees, mask = tree_mask, buffer = 5, step = 1, dm = 1, plot = FALSE)
#' coreg$local_max[, c("dx1", "dy1")]
#'
#' # plot raster
#' terra::plot(coreg$correlation_raster)
#' abline(h = 0, lty = 2)
#' abline(v = 0, lty = 2)
#' # add location of two local maxima
#' graphics::points(coreg$local_max[1, c("dx1", "dx2")],
#'   coreg$local_max[1, c("dy1", "dy2")],
#'   cex = c(1, 0.5), pch = 3, col = "red"
#' )
#' @export
coregistration <- function(chm, trees, mask, buffer = 19, step = 0.5, 
                           dm = 2, plot = TRUE) {
  # convert to terra
  if(!inherits(chm, "SpatRaster"))
  {
    chm <- convert_raster(chm, "terra")
    mask <- convert_raster(mask, "terra")
  }
  # compute virtual chm from tree inventory
  virtualchm <- terra::rasterize(as.matrix(trees[, 1:2]), mask, trees[, 3], fun = max)
  # set coordinate reference
  terra::crs(virtualchm) <- terra::crs(chm)
  terra::crs(mask) <- terra::crs(chm)
  # fill NA values with 0
  virtualchm[is.na(virtualchm)] <- 0
  #
  # compute correlation raster
  r_correlation <- rasters_moving_cor(chm, virtualchm, mask = mask, 
                                      buffer = buffer, step = step)
  # analyse correlation image
  resul <- raster_local_max(r_correlation)
  # display results
  if (plot) {
    # CHM
    terra::plot(chm, asp = 1)
    # display initial tree positions
    graphics::points(trees[, 1], trees[, 2], cex = trees[, 3] / 40)
    # display coregistered tree positions
    graphics::points(trees[, 1] + resul$dx1, trees[, 2] + resul$dy1, 
                     cex = trees[, 3] / 40, col = "red")
    graphics::legend("topleft", c("Initial", "Coregistered"), pch = 1, 
                     col = c("black", "red"))
  }
  list(correlation_raster = r_correlation, local_max = resul)
}
#
