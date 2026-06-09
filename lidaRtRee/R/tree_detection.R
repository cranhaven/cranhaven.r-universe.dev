# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
########################### FUNCTIONS FOR TREE DETECTION #######################
#
#-------------------------------------------------------------------------------
#' Tree detection
#' 
#' Performs tree detection by applying the functions \code{\link{tree_segmentation}}
#' and \code{\link{tree_extraction}} to objects of class \code{\link[terra]{SpatRaster-class}},
#' \code{\link[lidR]{LAS-class}} or \code{\link[lidR]{LAScatalog-class}}
#' 
#' @param las An object of class \code{\link[terra]{SpatRaster-class}},
#' \code{\link[lidR]{LAS-class}} or \code{\link[lidR]{LAScatalog-class}}
#' @param res numeric. The size of a grid cell in point cloud coordinates units,
#' used to rasterize the point cloud. In case the \code{las} argument is a \code{SpatRaster}
#' \code{res} is not used.
#' @param ROI spatial polygons in sf/sfc format, in the same CRS as argument \code{las}. geometric object that defines the
#' region where tree detection has to be performed. In case the input is of class
#' \code{\link[lidR]{LAScatalog-class}}, the chunk buffer set with 
#' \code{\link[lidR]{engine_options}} is applied to the point cloud to prevent 
#' border effects, but only treetops lying within the \code{ROI} are returned.
#' @param normalize boolean. Should the point cloud be normalized before detection
#' (not applicable if \code{las} argument is a \code{SpatRaster}) ?
#' @param crown Parameter passed to \code{\link{tree_extraction}}
#' @param ... Parameters passed to \code{\link{tree_segmentation}}
#' @examples
#' # load canopy height model
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#' # create polygon of region of interest
#' ROI <- sf::st_polygon(list(cbind(
#'  c(974360, 974360, 974380, 974380, 974360),
#'  c(6581640, 6581680, 6581680, 6581640, 6581640)
#' )))
#' # convert to sfc and set projection
#' ROI = sf::st_sfc(ROI)
#' sf::st_crs(ROI) <- terra::crs(chm_chablais3)
#' #
#' # tree detection
#' trees <- tree_detection(chm_chablais3)
#' # plot results
#' # canopy height model background
#' terra::plot(chm_chablais3)
#' # detected trees
#' plot(trees["h"], add = TRUE, cex = trees$h/20, col = "black")
#' #
#' # tree detection in ROI and minimum tree height set to 10
#' trees_ROI <- tree_detection(chm_chablais3, ROI = ROI, hmin = 10, crown = TRUE)
#' # create polygons from WKT field
#' trees_ROI_crowns <- sf::st_as_sf(sf::st_drop_geometry(trees_ROI), wkt = "crown")
#' # plot results
#' # canopy height model background 
#' terra::plot(chm_chablais3)
#' # detected trees
#' plot(trees_ROI["h"], add = TRUE, cex = trees_ROI$h/20, col = "black")
#' # corresponding crowns
#' plot(sf::st_geometry(trees_ROI_crowns), add = TRUE, border = "black", col = NA)
#' # add ROI
#' plot(ROI, add = TRUE, border = "red", col = NA)
#' @seealso \code{\link{tree_segmentation}}, \code{\link{tree_extraction}}
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain 
#' forests mapping: Support vector regression for stand parameters estimation 
#' and unsupervised training for treetop detection. Ph.D. thesis. University of 
#' Grenoble, France. Section 6.2 
#' \url{https://theses.hal.science/tel-00652698/document}
#'
#' Monnet, J.-M., Mermin, E., Chanussot, J., Berger, F. 2010. Tree top detection 
#' using local maxima filtering: a parameter sensitivity analysis. Silvilaser 2010, 
#' the 10th International Conference on LiDAR Applications for Assessing Forest 
#' Ecosystems, September 14-17, Freiburg, Germany, 9 p. 
#' \url{https://hal.science/hal-00523245/document}
#' @return A sf collection of POINTs with 7 fields: tree id, local maximum stats
#'  (height, dominance radius), segment stats (surface and volume), coordinates 
#'  (x and y). In case argument \code{crown} is \code{TRUE}, a \code{crown} field 
#'  containing the WKT geometry of the 2D crown is also present.
#' @export
tree_detection <-
  function(las,
           res = 1,
           ROI = NULL,
           normalize = FALSE,
           crown = FALSE,
           ...
           )
  {
    if (!is.null(ROI))
    {
      # dissolve ROI, keep only geometry
      ROI <- sf::st_union(sf::st_combine(sf::st_geometry(ROI)))
    }
    # if catalog
    if (lidR::is(las, "LAScatalog")) {
      # automerge
      options <- list(automerge = TRUE)
      # if ROI not NULL
      if (!is.null(ROI))
      {
        if (sf::st_crs(las) != sf::st_crs(ROI))
          warning("CRS of las and ROI should be the same")
        # flag as processed only tiles which intersect ROI
        las$processed <- FALSE
        dummy <- sf::st_filter(las@data, ROI)
        las$processed[is.element(las$filename, dummy$filename)] <-
          TRUE
        if (sum(las$processed) == 0)
        {
          warning("No file intersects the ROI")
          return(NULL)
        }
      }
      # check buffer size
      if (lidR::opt_chunk_buffer(las) < 10)
        warning("For tree segmentation a buffer larger than 10 m is recommended to avoid border effects")
      # apply function to catalog
      output <- lidR::catalog_apply(
        las,
        tree_detection,
        res = res,
        ROI = ROI,
        normalize = normalize,
        crown = crown,
        ...,
        .options = options
      )
      return(output)
      #
      # if LAScluster
    } else if (lidR::is(las, "LAScluster")) {
      # IS IT POSSIBLE TO TEST INTERSECTION OF ROI WITH CHUNK WITHOUT BUFFER HERE ?
      # AND OUTPUT NULL IF NO INTERSECTION ?
      x <- lidR::readLAS(las)
      if (lidR::is.empty(x))
        return(NULL)
      # retrieve buffer
      buffer <- las@buffer
      # if ROI present
      if (!is.null(ROI))
      {
        # crop ROI to chunk
        ROI <- sf::st_crop(ROI, las)
        # crop point cloud to ROI plus buffer
        x <- lidR::clip_roi(x, sf::st_buffer(ROI, buffer))
      }
      if (lidR::is.empty(x))
        return(NULL)
      #
      output <- tree_detection(
        x,
        res = res,
        ROI = ROI,
        normalize = normalize,
        crown = crown,
        ...
      )
      if (is.null(output)) return(NULL)
      if (nrow(output) == 0) return(NULL)
      # if no ROI
      if (is.null(ROI))
      {
        # crop results to bounding box of chunk
        sf::st_agr(output) = "constant"
        output <- sf::st_crop(output, las)
      }
      if (nrow(output) == 0) {
        return(NULL)
      } else {
        return(output)
      }
      #
      # if LAS
    } else {
      if (lidR::is(las, "LAS")) {
        #
        if (normalize)
        {
          las <- lidR::normalize_height(las, algorithm = lidR::tin())
        }
        # compute chm from points
        las <-
          lidR::rasterize_canopy(las, res = res, algorithm = lidR::p2r())
      } # if raster
      if (inherits(las, "SpatRaster"))
      {
        segms <- lidaRtRee::tree_segmentation(
          las,
          ...
        )
        # extract apices
        output <-
          lidaRtRee::tree_extraction(segms, crown = crown)
        if (is.null(output)) return(NULL)
        # if ROI
        if (!is.null(ROI))
        {
          sf::st_agr(output) = "constant"
          output <- sf::st_intersection(output, ROI)
        }
        if (nrow(output) == 0) {
          return(NULL)
        }
        return(output)
      }
      stop("Not supported input")
    }
  }

#-------------------------------------------------------------------------------
#' Disk-shaped matrix mask
#'
#' Creates a matrix with TRUE values shaping a centered disk
#'
#' @param width numeric. disk width in pixels, should be an uneven number
#' @return A matrix with 1 for pixels inside the disk, 0 outside
#' @examples
#' create_disk(7)
#' @export
create_disk <- function(width = 5) {
  if (width %% 2 != 1) {
    stop("Mask width should be uneven")
  }
  # matrix of row indices
  row.mat <- matrix(rep(1:width, width), nrow = width, ncol = width)
  # matrix of column indices
  col.mat <- matrix(rep(1:width, width), nrow = width, ncol = width, byrow = TRUE)
  radius <- width %/% 2
  row.mat <- row.mat - radius - 1
  col.mat <- col.mat - radius - 1
  mask <- (col.mat^2 + row.mat^2) <= radius^2
  mask
}

#-------------------------------------------------------------------------------
#' Image pre-processing (non-linear filtering and Gaussian smoothing)
#'
#' applies two filters to an image:
#' \enumerate{
#' \item A non-linear filter: closing (\code{\link[imager]{mclosing}}) with disk 
#' kernel, or median (\code{\link[imager]{medianblur}}) with square kernel
#' \item A 2D Gaussian smoother (The \code{\link[imager]{deriche}} filter is 
#' applied on both dimensions). Value-dependent smoothing is possible
#' }
#'
#' @param dem cimg object (e.g. obtained with \code{\link[imager]{as.cimg}}) or 
#' SpatRaster object (e.g. obtained with \code{\link[terra]{rast}})
#' @param nl_filter string. type of non-linear filter to apply: "None", "Closing" 
#' or "Median"
#' @param nl_size numeric. kernel width in pixels for non-linear filtering
#' @param sigma numeric or matrix. If a single number is provided, \code{sigma} is 
#' the standard deviation of the Gaussian filter, 0 corresponds to no 
#' smoothing. Unit is pixel in case \code{dem} is a cimg object, SpatRaster units 
#' otherwise. In case of a matrix, the first column corresponds to the standard 
#' deviation of the filter, and the second to thresholds for image values (e.g. 
#' a filter of standard deviation specified in line \code{i} is applied to pixels 
#' in image which values are between thresholds indicated in lines \code{i} and 
#' \code{i+1}). Threshold values should be ordered in increasing order.
#' @param padding boolean. Whether image should be padded by duplicating edge 
#' values before filtering to avoid border effects
#' @param sigmap deprecated (numeric or matrix). (old name for \code{sigma} parameter, 
#' retained for backward compatibility, overwrites \code{sigma} if provided, unit is 
#' pixel whatever the class of \code{dem}) 
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # filtering with median and Gaussian smoothing
#' im <- dem_filtering(chm_chablais3, nl_filter = "Median", nl_size = 3, sigma = 0.8)
#'
#' # filtering with median filter and value-dependent Gaussian smoothing
#' # (less smoothing for values between 0 and 15)
#' im2 <- dem_filtering(chm_chablais3,
#'   nl_filter = "Median", nl_size = 3,
#'   sigma = cbind(c(0.2, 0.8), c(0, 15))
#' )
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Initial image")
#'
#' # plot image after median filter
#' terra::plot(im$non_linear_image, main = "Median filter")
#'
#' # plot image after median and Gaussian filters
#' terra::plot(im$smoothed_image, main = "Smoothed image")
#'
#' # plot image after median and value-dependent Gaussian filters
#' terra::plot(im2$smoothed_image, main = "Value-dependent smoothing")
#' @seealso \code{\link{maxima_detection}}, filters of imager package: 
#' \code{\link[imager]{mclosing}}, \code{\link[imager]{medianblur}}, 
#' \code{\link[imager]{deriche}}
#' @return A list of two cimg objects or a SpatRaster object with image after non-linear 
#' filter and image after both filters
#' @export
dem_filtering <- function(dem, nl_filter = "Closing", nl_size = 5, sigma = 0.3, 
                          padding = TRUE, sigmap = NULL) {
  # convert raster to cimg object if necessary
  if (inherits(dem, "SpatRaster")) {
    dem.c <- raster2Cimg(dem)
    # conversion of Gaussian filter standard deviation from meters to pixels
    if (length(sigma == 1)) {
      sigma <- sigma / terra::res(dem)[1]
    } else {
      sigma[, 2] <- sigma[, 2] / terra::res(dem)[1]
    }
  } else {
    dem.c <- dem
  }
  # if sigmap argument present, its value (in pixels) overrides sigma
  if (!is.null(sigmap))
  {
    sigma <- sigmap
    warning("sigmap argument in function dem_filtering is deprecated, please use its new name sigma")
  }
  #
  if (padding) {
    # padding number of cells is maximum of half width of non linear filter or 
    # ceiling value of three times sigma
    if (!is.null(dim(sigma))) {
      dummy <- max(sigma[, 1])
    } else {
      dummy <- sigma
    }
    border.size <- max((nl_size - 1) / 2 + 1, ceiling(dummy * 3))
    # convert to matrix
    dem.c <- as.matrix(dem.c)
    # duplicate columns
    dem.c <- dem.c[, c(rep.int(1, border.size), 1:ncol(dem.c), 
                       rep.int(ncol(dem.c), border.size))]
    # duplicate rows
    dem.c <- dem.c[c(rep.int(1, border.size), 1:nrow(dem.c), 
                     rep.int(nrow(dem.c), border.size)), ]
    dem.c <- imager::as.cimg(dem.c)
  }
  #
  # non-linear filtering
  dem_nl <- dem.c
  if (nl_filter == "Closing") {
    # closing with structuring element (disk)
    strel <- imager::as.cimg(create_disk(nl_size))
    dem_nl <- imager::mclosing(dem_nl, strel)
  }
  if (nl_filter == "Median") {
    # median on square window
    # strel <- imager::as.cimg(create_disk(nl_size))
    dem_nl <- imager::medianblur(dem_nl, nl_size)
  }
  #
  # linear filtering
  # gaussian smoothing
  # if several values of sigma are provided, value-dependent smoothin is performed
  if (length(sigma) > 1) {
    dem_gs <- dem_nl
    # for each value of sigma
    for (i in 1:nrow(sigma))
    {
      # perform 2D smoothing
      dummy <- imager::deriche(dem_nl, sigma[i, 1], axis = "x")
      dummy <- imager::deriche(dummy, sigma[i, 1], axis = "y")
      # identify pixels which values are superior to the threshold
      temp <- dem_gs >= sigma[i, 2]
      # update only those values in the output
      dem_gs[temp] <- dummy[temp]
    }
  } else { # if only one value of sigma is provided
    if (sigma > 0) {
      dem_gs <- imager::deriche(dem_nl, sigma, axis = "x")
      dem_gs <- imager::deriche(dem_gs, sigma, axis = "y")
    } else { # if 0, no smoothing is performed
      dem_gs <- dem_nl
    }
  }
  # remove padding
  if (padding) {
    dem_nl <- as.matrix(dem_nl)
    dem_nl <- imager::as.cimg(dem_nl[(border.size + 1):(nrow(dem_nl) - border.size), 
                                     (border.size + 1):(ncol(dem_nl) - border.size)])
    dem_gs <- as.matrix(dem_gs)
    dem_gs <- imager::as.cimg(dem_gs[(border.size + 1):(nrow(dem_gs) - border.size), 
                                     (border.size + 1):(ncol(dem_gs) - border.size)])
  }
  # convert cimg objects to SpatRaster if necessary
  if (inherits(dem, "SpatRaster")) {
    output <- c(cimg2Raster(dem_nl, dem), cimg2Raster(dem_gs, dem))
    names(output) <- c("non_linear_image", "smoothed_image")
  } else {
    output <- list(non_linear_image = dem_nl, smoothed_image = dem_gs)
  }
  output
}

#-------------------------------------------------------------------------------
#' Local maxima extraction on image
#'
#' Variable window size maxima detection is performed on the image to extract 
#' local maxima position and calculate the window size where they are global 
#' maxima. Gaussian white noise is added to the image to avoid adjacent maxima 
#' due to neighbor pixels with identical value.
#'
#' @param dem cimg object (e.g. as created by \code{\link[imager]{cimg}}) or 
#' SpatRaster object (e.g. obtained with \code{\link[terra]{rast}})
#' @param dem.res numeric. image resolution, in case \code{dem} is a SpatRaster 
#' object, \code{dem.res} is extracted from the object by \code{\link[terra]{res}}
#' @param max.width numeric. maximum kernel width to check for local maximum, in 
#' pixels if \code{dem} is a cimg, in SpatRaster units otherwise
#' @param jitter boolean. indicates if noise should be added to image values to 
#' avoid adjacent maxima due to the adjacent pixels with equal values
#' @return A cimg object / SpatRaster object which values correspond to the radius 
#' (n) in pixels / meters of the square window (width 2n+1) where the center pixel is global 
#' maximum (tested up to the \code{max.width} parameter)
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # maxima detection
#' maxi <- maxima_detection(chm_chablais3)
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Initial image")
#'
#' # plot maxima image
#' terra::plot(maxi, main = "Local maxima")
#' @seealso \code{\link{dem_filtering}}, \code{\link{maxima_selection}}, 
#' \code{\link{tree_segmentation}}
#' @export
maxima_detection <- function(dem, dem.res = 1, max.width = 11, jitter = TRUE) {
  # convert raster to cimg object if necessary
  if (inherits(dem, "SpatRaster")) {
    dem_gs <- raster2Cimg(dem)
    dem.res <- terra::res(dem)[1]
    max.width <- max.width / dem.res
  } else {
    dem_gs <- dem
  }
  #
  # add absolute of gaussian white noise, mean=0, sd=sd(dem_gs)/100000 to non 0 pixels
  # avoids adjacent maxima but output maybe not reproducible
  if (jitter) {
    dem_gs <- dem_gs + abs(imager::imnoise(
      dim = dim(dem_gs), 
      sd = stats::sd(dem_gs) / 100000)) * (dem_gs != 0)
  }
  #
  max.radius <- max.width %/% 2
  # extraction of maxima on variable window size (from 0 to max.radius)
  # METHOD 1 using lapply
  # maxi <- lapply(1:max.radius, function(i)
  # {
  #   # create square structuring element for dilation
  #   strel <- imager::imfill(2 * i + 1, 2 * i + 1, val = 1)
  #   # check where pixel values are equal in original image and 
  #   # in image dilated by structuring element
  #   imager::as.cimg(dem_gs == imager::dilate(dem_gs, strel)) * i
  # }
  # )
  # # retain only max value from each image
  # maxi <- imager::parmax(maxi2, na.rm = TRUE)
  #
  # METHOD 2 using a for loop (avoid storing multiple images at the same time)
  # used in lidaRtRee <= 4.0.4
  # maxi <- NULL
  # for (i in 1:max.radius)
  # {
  #   # create square structuring element for dilation
  #   strel <- imager::imfill(2 * i + 1, 2 * i + 1, val = 1)
  #   # if initialization check where pixel values are equal in original image and
  #   # in image dilated by structuring element
  #   if (is.null(maxi)) {
  #     maxi <- imager::as.cimg(dem_gs == imager::dilate(dem_gs, strel)) * i
  #     # otherwise perform check and update previous result
  #   } else {
  #     maxi <- imager::parmax(list(maxi,
  #                                 imager::as.cimg(dem_gs == imager::dilate(dem_gs, strel)) * i), na.rm = TRUE)
  #   }
  # }
  # METHOD 3 : using a for loop with iterative dilation -> faster
  # create 3x3 square structuring element for dilation
  strel <- imager::imfill(3, 3, val = 1)
  # first dilation (radius = 1)
  dem_dilate <- imager::dilate(dem_gs, strel)
  # first result
  maxi <- imager::as.cimg(dem_gs == dem_dilate)
  # loop on increasing radius
  for (i in 2:max.radius)
  {
    # dilate more
    dem_dilate <- imager::dilate(dem_dilate, strel)
    # perform check and update previous result
    maxi <- imager::parmax(list(maxi, 
                                imager::as.cimg(dem_gs == dem_dilate) * i), na.rm = TRUE)
  }
  #
  # convert window size from pixels to meters
  maxi <- ( maxi > 0) * (maxi + 1) * dem.res
  # convert cimg objects to raster if necessary
  if (inherits(dem, "SpatRaster")) {
    maxi <- cimg2Raster(maxi, dem)
  }
  maxi
}
#-------------------------------------------------------------------------------
#' Image maxima selection based on values and neighborhood of local maxima
#'
#' In a maxima image (output of \code{\link{maxima_detection}}), sets values to 
#' zero for pixels which:
#' \enumerate{
#' \item values in the initial image (from which maxima were detected) are below 
#' a threshold
#' \item values in the maxima image (corresponding to the radius of the 
#' neighborhood where they are global maxima) are below a threshold depending on 
#' the initial image value.
#' }
#' Make sure that the \code{max.width} parameter in \code{\link{maxima_detection}}
#' is consistent with the selection parameters (e.g. do not select with 
#' \code{dmin = 7} if values were only tested up to \code{max.width} the default 
#' value which is approx. 5.5 m).
#'
#' @param maxi cimg object or SpatRaster object. image with local maxima 
#' (typically output from \code{\link{maxima_detection}}, image values correspond 
#' to neighborhood radius on which pixels are global maxima in the initial image)
#' @param dem_nl cimg object or SpatRaster object. initial image from which maxima were detected
#' @param hmin numeric. minimum value in initial image for a maximum to be selected
#' @param dmin numeric. intercept term for selection of maxima depending on 
#' neighborhood radius: \code{maxi >= dmin + dem_nl * dprop}
#' @param dprop numeric. proportional term for selection of maxima depending on 
#' neighborhood radius: \code{maxi >= dmin + dem_nl * dprop}
#' @return A cimg object or SpatRaster object which values are the radius (n) 
#' in meter of the square window (width 2n+1) where the center pixel is global 
#' maximum and which fulfill the selection criteria
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # maxima detection
#' maxi <- maxima_detection(chm_chablais3)
#'
#' # several maxima selection settings
#' selected_maxi_hmin <- maxima_selection(maxi, chm_chablais3, hmin = 15)
#' selected_maxi_dm <- maxima_selection(maxi, chm_chablais3, dmin = 2.5)
#' selected_maxi <- maxima_selection(maxi, chm_chablais3, dmin = 1, dprop = 0.1)
#'
#' # corresponding count number of remaining maxima
#' table(terra::values(maxi))
#' table(terra::values(selected_maxi_hmin))
#' table(terra::values(selected_maxi_dm))
#' table(terra::values(selected_maxi))
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Initial image")
#'
#' # plot maxima images, original and first case
#' terra::plot(maxi, main = "Local maxima")
#' terra::plot(selected_maxi, main = "Selected maxima")
#' @seealso \code{\link{maxima_detection}}, \code{\link{tree_segmentation}}
#' @export
maxima_selection <- function(maxi, dem_nl, hmin = 5, dmin = 0, dprop = 0.05) {
  # convert SpatRaster to cimg object if necessary
  if (inherits(maxi, "SpatRaster")) {
    israster <- TRUE
    maxi_c <- raster2Cimg(maxi)
    dem_nl <- raster2Cimg(dem_nl)
  } else {
    israster <- FALSE
    maxi_c <- maxi
  }
  # height filter
  maxi_c[dem_nl < hmin] <- 0
  # distance filter
  maxi_c[maxi_c < (dmin + dem_nl * dprop)] <- 0
  # convert to raster if necessary
  if (israster) {
    cimg2Raster(maxi_c, maxi)
  } else {
    maxi_c
  }
}

#-------------------------------------------------------------------------------
#' Image segmentation by seed-based watershed algorithm
#'
#' performs a seed-based watershed segmentation (wrapper for \code{\link[imager]{watershed}})
#'
#' @param maxi cimg or SpatRaster object. image with seed points (e.g. from 
#' \code{\link{maxima_detection}} or \code{\link{maxima_selection}})
#' @param dem_nl cimg or SpatRaster object. image for seed propagation 
#' (typically initial image used for maxima detection).
#' @return A cimg object or SpatRaster object with segments id
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # median filter
#' chm_chablais3 <- dem_filtering(chm_chablais3,
#'   nl_filter = "Median", nl_size = 3,
#'   sigma = 0
#' )$non_linear_image
#'
#' # maxima detection
#' maxi <- maxima_detection(chm_chablais3)
#'
#' # maxima selection
#' selected_maxi <- maxima_selection(maxi, chm_chablais3, dm = 1, dprop = 0.1)
#'
#' # segmentation
#' seg_maxi <- segmentation(maxi, chm_chablais3)
#' seg_selected_maxi <- segmentation(selected_maxi, chm_chablais3)
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Median filter")
#'
#' # plot segmented image
#' # replace segment with id 0 (not a tree) with NA
#' seg_maxi[seg_maxi == 0] <- NA
#' terra::plot(seg_maxi %% 8, main = "Segments, no maxima selection", 
#' col = rainbow(8))
#' seg_selected_maxi [seg_selected_maxi == 0] <- NA
#' terra::plot(seg_selected_maxi %% 8, main = "Segments, maxima selection", 
#' col = rainbow(8))
#' @seealso \code{\link{maxima_detection}}, \code{\link{maxima_selection}}, 
#' \code{\link{seg_adjust}}
#' @export
segmentation <- function(maxi, dem_nl) {
  # convert SpatRaster to cimg object if necessary
  if (inherits(maxi, "SpatRaster")) {
    israster <- TRUE
    maxi_c <- raster2Cimg(maxi)
    dem_nl <- raster2Cimg(dem_nl)
  } else {
    israster <- FALSE
  }
  # maxima locations
  a <- which(maxi_c > 0)
  # tree number
  na <- length(a)
  # create seed image
  dummy <- maxi_c
  # initialise seeds with id
  # WHY USE SAMPLE HERE ?
  # tree id different in each run but segments more visible when plotted
  dummy[a] <- sample(1:na, na)
  # watershed segmentation
  dem_w <- imager::watershed(dummy, dem_nl)
  #
  # convert to raster if necessary
  if (israster) {
    cimg2Raster(dem_w, maxi)
  } else {
    dem_w
  }
}

#-------------------------------------------------------------------------------
#' Image statistic in segment
#'
#' compute zonal statistic of an image
#'
#' @param segms cimg or SpatRaster object. image with segments id (e.g. from 
#' \code{\link{segmentation}})
#' @param dem_nl cimg or SpatRaster object. image to compute statistic from
#' @param fun function to compute statistics from values in each segment
#' @return A cimg object or raster object with values of the statistic
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # median filter
#' chm_chablais3 <- dem_filtering(chm_chablais3,
#'   nl_filter = "Median", nl_size = 3,
#'   sigma = 0
#' )$non_linear_image
#'
#' # maxima detection
#' maxi <- maxima_detection(chm_chablais3)
#'
#' # segmentation
#' seg_maxi <- segmentation(maxi, chm_chablais3)
#'
#' # compute image of maximum value in each segment
#' max_in_segment <- raster_zonal_stats(seg_maxi, chm_chablais3)
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Median filter")
#'
#' # plot segments and image of max value inside segments
#' seg_maxi[seg_maxi == 0] <- NA
#' terra::plot(seg_maxi %% 8, main = "Segments", col = rainbow(8))
#' terra::plot(max_in_segment, main = "Max value in segment")
#' @seealso \code{\link{segmentation}}
#' @export
raster_zonal_stats <- function(segms, dem_nl, fun = max) {
  # convert SpatRaster to cimg object if necessary
  if (inherits(segms, "SpatRaster")) {
    israster <- TRUE
    segms_c <- raster2Cimg(segms)
    dem_nl <- raster2Cimg(dem_nl)
  } else {
    israster <- FALSE
  }
  dummy <- stats::aggregate(as.numeric(dem_nl), by = list(as.numeric(segms_c)), 
                            FUN = fun)
  dem_wh <- dummy$x[match(as.numeric(segms_c), dummy$Group.1)]
  #
  dem_wh <- imager::as.cimg(matrix(dem_wh, nrow(segms_c)))
  #
  # convert to raster if necessary
  if (israster) {
    cimg2Raster(dem_wh, segms)
  } else {
    dem_wh
  }
}

#-------------------------------------------------------------------------------
#' Modification of segments based on values
#'
#' in a segmented image, removes from segments the pixels which values in a 
#' reference image is below a certain percentage of the highest value inside the 
#' segment. Removed pixels are attributed 0 value.
#'
#' @param dem_w cimg or SpatRaster object. image with segments id, without 0 
#' values
#' @param dem_wh cimg or SpatRaster object. image with max value inside segment
#' @param dem_nl cimg or SpatRaster object. image with initial values
#' @param prop numeric. proportional threshold for removal of pixels which initial 
#' values are lower than the max height of the segment (\code{dem_nl < prop x dem_wh})
#' @param min.value numeric. threshold for removel of pixels which initial values 
#' are lower (\code{dem_nl < min.value})
#' @param min.maxvalue numeric. threshold for complete removal of segments which 
#' maximum value height is smaller to the threshold (\code{dem_wh < min.maxvalue})
#' @return A cimg or SpatRaster object: image with modified segments.
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # median filter
#' chm_chablais3 <- dem_filtering(chm_chablais3,
#'   nl_filter = "Median", nl_size = 3,
#'   sigma = 0
#' )$non_linear_image
#'
#' # maxima detection and selection
#' maxi <- maxima_detection(chm_chablais3)
#' selected_maxi <- maxima_selection(maxi, chm_chablais3, dm = 1, dprop = 0.1)
#'
#' # segmentation
#' seg_selected_maxi <- segmentation(selected_maxi, chm_chablais3)
#'
#' # max value in segments
#' max_in_segment <- raster_zonal_stats(seg_selected_maxi , chm_chablais3)
#'
#' # segmentation modification
#' seg_modif1 <- seg_adjust(seg_selected_maxi , max_in_segment,
#'   chm_chablais3,
#'   prop = 0.5
#' )
#' seg_modif2 <- seg_adjust(seg_selected_maxi , max_in_segment,
#'   chm_chablais3,
#'   prop = 0, min.value = 5, min.maxvalue = 10
#' )
#'
#' # plot initial segmented image
#' # seg_selected_maxi[seg_selected_maxi == 0] <- NA
#' terra::plot(seg_selected_maxi %% 8, main = "Initial segments", col = rainbow(8))
#' # seg_modif1[seg_modif1 == 0] <- NA
#' terra::plot(seg_modif1 %% 8, main = "Modified segments 1", col = rainbow(8))
#' seg_modif2[seg_modif2 == 0] <- NA
#' terra::plot(seg_modif2 %% 8, main = "Modified segments 2", col = rainbow(8))
#' @seealso \code{\link{maxima_detection}}, \code{\link{maxima_selection}}
#' @export
seg_adjust <- function(dem_w, dem_wh, dem_nl, prop = 0.3, min.value = 2, min.maxvalue = 5) {
  # convert SpatRaster to cimg object if necessary
  if (inherits(dem_w, "SpatRaster")) {
    israster <- TRUE
    dem_wc <- raster2Cimg(dem_w)
    dem_wh <- raster2Cimg(dem_wh)
    dem_nl <- raster2Cimg(dem_nl)
  } else {
    israster <- FALSE
  }
  # removal of segments with maximum value lower than min.maxvalue
  dem_wc[dem_wh < min.maxvalue] <- 0
  # removal of pixels with values lower than prop * max value in segment
  dem_wc[dem_nl < prop * dem_wh] <- 0
  # removal of pixels with values lower than min.value
  dem_wc[dem_nl < min.value] <- 0
  # convert to raster if necessary
  if (israster) {
    dem_w <- cimg2Raster(dem_wc, dem_w)
  }
  dem_w
}

#-------------------------------------------------------------------------------
#' Preprocessing and segmentation of raster image for tree identification
#'
#' global function for preprocessing (filtering), maxima detection and selection, 
#' segmentation and segmentation adjustment of a raster image.
#'
#' @param dem raster object or string indicating location of raster file 
#' (typically a canopy height model or a digital surface model; in the latter 
#' case the dtm parameter should be provided)
#' @param dtm raster object or string indicating location of raster file with 
#' the terrain model. If provided, the maxima extraction and watershed segmentation 
#' are performed on the dem (this avoids the deformation of crown because of the 
#' normalisation with terrain), but maxima selection and segment adjustment are 
#' performed on 'dem-dtm' because the selection criteria are based on the height to terrain.
#' @param ... arguments passed to functions \code{\link{dem_filtering}}
#' (e.g. \code{nl_filter}, \code{nl_size}, \code{sigma}), 
#' \code{\link{maxima_detection}}, \code{\link{maxima_selection}}, 
#' \code{\link{maxima_selection}} (\code{dmin}: treetop minimum distance to next
#' higher pixel in meters, \code{dprop}: number defining the treetop minimum 
#' distance as proportion of its height to next higher pixel, \code{hmin}: 
#' minimum treetop height), \code{\link{seg_adjust}} (\code{prop}: minimum 
#' height of tree crown base as proportion of treetop height, \code{min.value}: 
#' minimum crown base height)
#' @param crown_prop (deprecated) numeric. (overrides \code{prop} parameter 
#' passed to \code{\link{seg_adjust}}, for backward compatibility)
#' @param crown_hmin (deprecated) numeric. (overrides \code{min.value} parameter 
#' passed to \code{\link{seg_adjust}}, for backward compatibility)
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain 
#' forests mapping: Support vector regression for stand parameters estimation 
#' and unsupervised training for treetop detection. Ph.D. thesis. University of 
#' Grenoble, France. Section 6.2 
#' \url{https://theses.hal.science/tel-00652698/document}
#'
#' Monnet, J.-M., Mermin, E., Chanussot, J., Berger, F. 2010. Tree top detection 
#' using local maxima filtering: a parameter sensitivity analysis. Silvilaser 2010, 
#' the 10th International Conference on LiDAR Applications for Assessing Forest 
#' Ecosystems, September 14-17, Freiburg, Germany, 9 p. 
#' \url{https://hal.science/hal-00523245/document}
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # tree segmentation
#' segments <- tree_segmentation(chm_chablais3)
#' segments2 <- tree_segmentation(chm_chablais3,
#'   nl_filter = "Median", nl_size = 3,
#'   sigma = cbind(c(0.2, 0.8), c(0, 15)), dmin = 0, dprop = 0, hmin = 10, 
#'   crown_prop = 0.5, crown_hmin = 5
#' )
#'
#' # plot initial image segments
#' terra::plot(chm_chablais3, main = "Initial image")
#' terra::plot(segments$smoothed_dem, main = "Filtered image")
#' terra::plot(segments$local_maxima, main = "Local maxima")
#' #
#' # replace segment with id 0 (not a tree) with NA
#' segments$segments_id[segments$segments_id == 0] <- NA
#' terra::plot(segments$segments_id %% 8, main = "Segments", col = rainbow(8))
#' #
#' # plot segmentation with other parameters
#' segments2$segments_id[segments2$segments_id == 0] <- NA
#' terra::plot(segments2$segments_id %% 8, main = "Segments2", col = rainbow(8))
#' @seealso \code{\link{dem_filtering}}, \code{\link{maxima_detection}}, 
#' \code{\link{maxima_selection}}, \code{\link{segmentation}}, 
#' \code{\link{seg_adjust}}, \code{\link{tree_extraction}},
#' \code{\link{tree_detection}}
#' @return A SpatRaster with 4 layers: selected local maxima (values = 
#' distance to higher pixel), segments, non-linear preprocessed dem, smoothed 
#' preprocessed dem
#' @export
tree_segmentation <- function(dem, dtm = NULL, crown_prop = NULL, crown_hmin = NULL, ...) {
  #
  # sort dots arguments
  # expected arguments for subsequent functions
  dem_filtering.args <- names(formals(dem_filtering)) #  nl_filter = "Closing", nl_size = 5, sigma = 0.3
  maxima_detection.args <- names(formals(maxima_detection))
  maxima_selection.args <- names(formals(maxima_selection)) # dmin = 0, dprop = 0.05, hmin = 5
  seg_adjust.args <- names(formals(seg_adjust)) # crown_prop = 0.3, crown_hmin = 2
  # dots arguments
  dots <- list(...)
  # set default values
  # handle deprecated values
  if (!is.null(crown_prop))
  {
    dots$prop <- crown_prop
    warning("Argument crown_prop is deprecated, please use its new name: prop which is passed to function seg_adjust")
  }
  if (!is.null(crown_hmin))
  {
    dots$min.value <- crown_hmin
    warning("Argument crown_hmin is deprecated, please use its new name: min.value which is passed to function seg_adjust")
  }
  #
  if (all(is.element(c("min.value", "hmin"), names(dots))))
  {
    if (dots$min.value > dots$hmin) {
      stop("minimum tree height lower than minimum crown base height")
    }
  }
  dem <- convert_raster(dem, "terra")
  # convert NAs to terrain altitude
  if (!is.null(dtm)) {
    dtm <- convert_raster(dtm, "terra")
    dem[is.na(dem)] <- dtm[is.na(dem)]
  } else {
    dtm <- 0
    dem[is.na(dem)] <- dtm
  }
  #
  # dem filtering
  # temp <- dem_filtering(dem, nl_filter = nl_filter, nl_size = nl_size, sigma = sigma)
  temp <- do.call('dem_filtering', c(list(dem), dots[names(dots) %in% dem_filtering.args]))
  dem_nl <- temp$non_linear_image
  dem_gs <- temp$smoothed_image
  #
  # maxima detection
  # maxi <- maxima_detection(dem_gs, terra::res(dem)[1])
  maxi <- do.call('maxima_detection', c(list(dem_gs), dots[names(dots) %in% maxima_detection.args]))
  #
  # chm
  chm <- dem_nl - dtm
  # maxima selection
  # check detection parameters (window size) with selection parameters
  temp_dmin <- ifelse(methods::hasArg("dmin"), dots$dmin, formals(maxima_selection)$dmin)
  temp_dprop <- ifelse(methods::hasArg("dprop"), dots$dprop, formals(maxima_selection)$dprop)
  temp_max.width <- ifelse(methods::hasArg("max.width"), dots$max.width, formals(maxima_detection)$max.width)
  if (temp_dmin + temp_dprop * terra::global(chm, fun="max",  na.rm = TRUE) >= (temp_max.width / terra::res(dem)[1]) %/% 2)
    warning("Window size for detecting local maxima might be too small to ensure that function maxima_selection returns a correct output. Make sure that parameter max.width in function maxima_detection is appropriate, or consider reducing dmin or dprop parameters in maxima_selection")
  #
  # no selection based on top height at this stage, otherwise some artefacts occur in the watershed step
  # maxi <- maxima_selection(maxi, chm, 0, dmin, dprop) 
  maxi <- do.call('maxima_selection', c(list(maxi, chm, hmin = 0), dots[names(dots) %in% setdiff(maxima_selection.args, "hmin")]))
  #
  # segmentation
  dem_w <- segmentation(maxi, dem_nl)
  dem_wh <- raster_zonal_stats(dem_w, chm, fun = max)
  #
  # segmentation adjustment
  # pass hmin parameter for selection to min.maxvalue parameter for seg_adjust
  temp_hmin <- ifelse(methods::hasArg("hmin"), dots$hmin, formals(maxima_selection)$hmin)
  dots$min.maxvalue <- temp_hmin
  if (methods::hasArg("min.maxvalue"))
  {
    warning(paste0("min.maxvalue parameter value in seg_adjust in set to the same value as hmin parameter (", temp_hmin, ") in maxima_selection for constitency. To manually specify the minimum tree height please use only the hmin parameter"))
  }

  # dem_w <- seg_adjust(dem_w, dem_wh, chm, crown_prop, crown_hmin, hmin) # removal of trees with top heigth < hmin
  dem_w <- do.call('seg_adjust', c(list(dem_w, dem_wh, chm), dots[names(dots) %in% seg_adjust.args]))# prop = crown_prop, min.value = hmin)))
  # remove local maxima from r_maxi where segments may have been removed because of 
  # height criterion
  maxi[dem_w == 0] <- 0
  #
  output <- c(maxi, dem_w, dem_nl, dem_gs)
  names(output) <- c("local_maxima", "segments_id", "filled_dem", "smoothed_dem")
  output
}

#-------------------------------------------------------------------------------
#' Tree extraction
#'
#' creates a data.frame with segment id, height and coordinates of maxima, surface and volume, computed from three images: 
#' initial, local maxima and segmented, obtained with \code{\link{tree_segmentation}}. The 2D polygon associated to each crown 
#' can be added as a WKT field
#'
#' @param r_dem_nl SpatRaster object. Output raster of \code{\link{tree_segmentation}}. Otherwise a raster of canopy height model,
#' preferably filtered to avoid effect of holes on volume and surface computation can be provided. In this case arguments `r_maxi`, `r_dem_w` 
#' have to be provided
#' @param r_maxi SpatRaster object. raster with positive values at local maxima (in case `r_dem_nl` does not contain it)
#' @param r_dem_w SpatRaster object. segmented raster  (in case `r_dem_nl` does not contain it)
#' @param r_mask SpatRaster object. only segments which maxima are inside the mask are extracted. Values should be NA outside the mask, 1 inside.
#' @param crown boolean. Should the 2D crown geometry be added in wkt format 
#' to the output data.frame ?
#' @return A \code{sf} collection of POINTs with 7 fields: tree id, local maximum stats
#'  (height, dominance radius), segment stats (surface and volume), coordinates 
#'  (x and y). In case argument `crown` is `TRUE`, a `crown` field 
#'  containing the WKT geometry of the 2D crown is also present. Coordinates are 
#'  written with one decimal to the right of the order of magnitude of 
#'  the SpatRaster resolution (e.g. if resolution is 1/3 then 2 decimals are written).
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # tree segmentation
#' segments <- tree_segmentation(chm_chablais3)
#'
#' # tree extraction
#' trees <- tree_extraction(segments, crown = TRUE)
#' # create crown polygons from WKT field
#' trees_crowns <- sf::st_as_sf(sf::st_drop_geometry(trees), wkt = "crown")
#' 
#' # summary of trees without wkt field
#' summary(trees[, -which(names(trees) == "crown")])
#'
#' # plot initial image
#' terra::plot(chm_chablais3, main = "CHM and extracted trees")
#'
#' # add treetop positions
#' plot(trees["h"], add = TRUE, cex = trees$h/20, col = "black")
#' # add crowns
#' plot(sf::st_geometry(trees_crowns), add = TRUE, border = "black", col = NA)
#' 
#' # plot segments
#' terra::plot(segments$segments_id, main = "Segments")
#' # add crowns
#' plot(sf::st_geometry(trees_crowns), add = TRUE, border = "black", col = NA)
#'
#' @seealso \code{\link{tree_segmentation}}, \code{\link{tree_detection}}
#' @export
tree_extraction <- function(r_dem_nl, r_maxi = NULL, r_dem_w = NULL, r_mask = NULL, crown = FALSE) {
  # if first argument is an output of tree_segmentation
  if(length(names(r_dem_nl)) == 4)
  {
    if (all(names(r_dem_nl) == c("local_maxima", "segments_id", "filled_dem", "smoothed_dem")))
    {
      r_dem_w <- r_dem_nl$segments_id
      r_maxi <- r_dem_nl$local_maxima
      r_dem_nl <- r_dem_nl$filled_dem
    } else { # check that other rasters are provided if first one is not multi-layer
      if(is.null(r_maxi) & is.null(r_dem_w))
      {
        warning("Arguments r_maxi and r_dem_w should be provided if r_dem_nl is not multi-layered")
        return(NULL)
      }
    }
  }
  # segments surface
  s <- data.frame(t(table(terra::values(r_dem_w))))[, -1]
  names(s) <- c("id", "s")
  s$s <- s$s * terra::xres(r_dem_nl) * terra::yres(r_dem_nl)
  # segments volume
  v <- stats::aggregate(terra::values(r_dem_nl), by = list(terra::values(r_dem_w)), FUN = sum)
  names(v) <- c("id", "v")
  v$v <- v$v * terra::xres(r_dem_nl) * terra::yres(r_dem_nl)
  #
  # if mask is not null
  if (!is.null(r_mask)) {
    # apply mask to remove maxima outside of region of interest
    r_maxi <- r_maxi * r_mask
    # compute surface and volume inside mask
    # segments surface in mask
    sp <- data.frame(t(table(terra::values(r_dem_w * r_mask))))[, -1]
    names(sp) <- c("id", "sp")
    sp$sp <- sp$sp * terra::xres(r_dem_nl) * terra::yres(r_dem_nl)
    # segments volume in mask
    vp <- stats::aggregate(terra::values(r_dem_nl * r_mask),
                           by = list(terra::values(r_dem_w)), FUN = sum)
    names(vp) <- c("id", "vp")
    vp$vp <- vp$vp * terra::xres(r_dem_nl) * terra::yres(r_dem_nl)
  }
  #
  # extract segment id, height and dom_radius
  cells <- which(terra::values(r_maxi) > 0)
  # check if positive values are present
  if (length(cells) == 0) return(NULL)
  #
  # extract coordinates
  coord <- terra::xyFromCell(r_maxi, cells)
  # create data.frame
  trees <- data.frame(coord, id = terra::values(r_dem_w)[cells], 
                      h = terra::values(r_dem_nl)[cells], 
                      dom_radius = terra::values(r_maxi)[cells])
  # add surface and volume
  trees <- merge(trees, s, all.x = TRUE)
  trees <- merge(trees, v, all.x = TRUE)
  # add surface and volume in plot if mask present
  if (!is.null(r_mask)) {
    trees <- merge(trees, sp, all.x = TRUE)
    trees <- merge(trees, vp, all.x = TRUE)
  }
  # duplicate coordinates 
  trees$X <- trees$x
  trees$Y <- trees$y
  # convert to sf
  trees <- sf::st_as_sf(trees, coords = c("X", "Y"),
                        crs = terra::crs(r_dem_nl))
  # if output crowns
  if(crown)
  {
    vecteur <- terra::as.polygons(r_dem_w)
    # convert to sf
    vecteur <- sf::st_as_sf(vecteur)
    # compute number of digits to write (left of .)
    n1 <- ceiling(log(max(abs(sf::st_bbox(vecteur))))/log(10))
    # compute number of digits to write (right of .)
    n2 <- abs(min(floor(log((min(terra::res(r_dem_w))/10))/log(10)), 0))
    # extract geom to attribute
    vecteur$crown <- sf::st_as_text(sf::st_geometry(vecteur), digits = n1 + n2)
    # rename first attribute for merging
    names(vecteur)[1] <- "id"
    # add polygon geometry attribute to output
    trees <-
      merge(trees, sf::st_drop_geometry(vecteur), all.x = TRUE)
  }
  trees
}

#-------------------------------------------------------------------------------
#' Cimg to SpatRaster conversion
#'
#' converts a cimg object to a SpatRaster object
#'
#' @param cimg raster object. raster of canopy height model, preferably filtered to avoid effect of holes on volume and surface computation
#' @param r SpatRaster object. defines the extent and projection of conversion result
#' @return A SpatRaster object
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # convert raster to cimg object
#' chm_cim <- raster2Cimg(chm_chablais3)
#'
#' # apply filtering
#' chm_cim_filt <- dem_filtering(chm_cim,
#'   nl_filter = "Closing",
#'   nl_size = 3,
#'   sigma = 0
#' )$non_linear_image
#'
#' # convert to SpatRaster
#' chm_filt <- cimg2Raster(chm_cim_filt, chm_chablais3)
#'
#' # plot SpatRaster
#' terra::plot(chm_chablais3)
#'
#' # plot cimg object
#' plot(chm_cim)
#'
#' # plot filtered cimg object
#' plot(chm_cim_filt)
#'
#' # plot filtered SpatRaster
#' terra::plot(chm_filt)
#' @seealso \code{\link{raster2Cimg}}
#' @export
cimg2Raster <- function(cimg, r = NULL) {
  # convert to SpatRaster
  dem <- terra::rast(t(as.matrix(cimg)))
  # if reference is provided
  if (!is.null(r)) {
    # specifiy extent
    terra::ext(dem) <- terra::ext(r)
    # specify crs
    terra::crs(dem) <- terra::crs(r)
  }
  dem
}

#-------------------------------------------------------------------------------
#' SpatRaster to Cimg conversion
#'
#' converts a SpatRaster object to cimg object. NA values in raster are replaced.
#'
#' @param r SpatRaster object. raster of canopy height model, preferably 
#' filtered to avoid effect of holes on volume and surface computation
#' @param NA_replace numeric. value to replace NA values with.
#' @param maxpixels numeric. maximum number of pixels to be converted to cimg 
#' (argument passed to \code{\link[imager]{as.cimg}}).
#' @return A cimg object
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' chm_cim <- raster2Cimg(chm_chablais3)
#' chm_cim
#' summary(chm_cim)
#'
#' # plot SpatRaster
#' terra::plot(chm_chablais3)
#'
#' # plot cimg object
#' plot(chm_cim)
#' @seealso \code{\link{cimg2Raster}}
#' @export
raster2Cimg <- function(r, NA_replace = 0, maxpixels = 1e+10) {
  # replace NA values
  r[is.na(r)] <- NA_replace
  # convert to cimg object
  if (ncol(r) * nrow(r) > maxpixels) {
    warning("Too many raster pixels: conversion to cimg partial; try with higher maxpixels arguments")
  }
  imager::as.cimg(matrix(r, ncol = nrow(r)), maxpixels = maxpixels)
}
