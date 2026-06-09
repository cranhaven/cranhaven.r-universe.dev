# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' Gap detection in a Canopy Height Model
#'
#' Performs gaps detection on a canopy height model provided as object  of class 
#' \code{\link[terra]{SpatRaster-class}}, or computed from the point cloud of 
#' objects of class \code{\link[lidR]{LAS-class}} or 
#' \code{\link[lidR]{LAScatalog-class}}. Function \code{\link{dem_filtering}} 
#' is first applied to the canopy height model to remove artefacts. 
#' Gaps are then extracted based on several criteria:
#' \enumerate{
#' \item Vegetation height must be smaller than a threshold
#' \item Gap width must be large enough, depending on surrounding canopy height;
#' distance to surrounding vegetation is tested with morphological closings
#' \item Gap must have a minimum surface
#' }
#'
#' @param las An object of class \code{\link[terra]{SpatRaster-class}},
#' \code{\link[lidR]{LAS-class}} or \code{\link[lidR]{LAScatalog-class}}
#' @param res numeric. The size of a grid cell in point cloud coordinates units,
#' used to rasterize the point cloud. In case the \code{las} argument is a 
#' \code{SpatRaster} \code{res} is not used.
#' @param ratio numeric. maximum ratio between surrounding canopy height and gap
#' distance (a pixel belongs to the gap only if for any vegetation pixel around
#' it, the distance to the vegetation pixel is larger than pixel height/ratio).
#' If ratio is set to NULL, this criterion is not taken into account
#' @param gap_max_height numeric. maximum canopy height to be considered as gap
#' @param min_gap_surface numeric. minimum gap surface
#' @param max_gap_surface numeric. maximum gap surface
#' @param closing_height_bin numeric. height bin width for morphological closing
#' of gaps to test ratio between canopy height and gap distance
#' @param nl_filter string. type of non-linear filter to apply to canopy height
#' model to remove artefacts, should be an option of \code{\link{dem_filtering}}
#' @param nl_size numeric. kernel width in pixel for non-linear filtering
#' @param gap_reconstruct boolean. default behaviour is that areas that do not
#' fulfill the ratio criterion are removed from gaps. If set to TRUE, in case
#' some pixels of a gap fulfill the distance criterion, the connected pixels that
#' fulfill the height criterion are also integrated to it.
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # fill NA values in canopy height model
#' chm_chablais3[is.na(chm_chablais3)] <- 0
#'
#' # gap detection with distance larger than canopy height / 2
#' gaps <- gap_detection(chm_chablais3, ratio = 2, gap_max_height = 1, 
#' min_gap_surface = 0)
#'
#' # gap detection with distance larger than canopy height / 2
#' # and reconstruction of border areas
#' gaps1 <- gap_detection(chm_chablais3,
#'   ratio = 2, gap_max_height = 1, min_gap_surface = 0,
#'   gap_reconstruct = TRUE
#' )
#'
#' # gap detection without distance criterion
#' gaps2 <- gap_detection(chm_chablais3, ratio = NULL, gap_max_height = 1, 
#' min_gap_surface = 0)
#'
#' # gap id and corresponding surface for third detection parameters
#' table(terra::values(gaps2$gap_id)) * terra::res(gaps2$gap_id)[1]^2
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Initial image")
#'
#' # plot binary image of gaps
#' terra::plot(gaps$gap_id > 0, main = "Gaps", col = "green", legend = FALSE)
#' terra::plot(gaps1$gap_id > 0, main = "Gaps, with reconstruction", col = "green", legend = FALSE)
#' terra::plot(gaps2$gap_id > 0, main = "Gaps, no width criterion", col = "green", legend = FALSE)
#'
#' # plot filtered CHM
#' terra::plot(gaps2$filled_chm, main = "Filtered CHM")
#' @seealso \code{\link{dem_filtering}}, \code{\link{edge_detection}}
#' @return A \code{SpatRaster} object with three layers: gap labels, gap surface 
#' and canopy height model after filter.
#' @export
#' 
gap_detection <-
  function(las,
           res = 1, 
           ratio = 2,
           gap_max_height = 1,
           min_gap_surface = 25,
           max_gap_surface = +Inf,
           closing_height_bin = 1,
           nl_filter = "Median",
           nl_size = 3,
           gap_reconstruct = FALSE)
  {
    # if catalog
    if (lidR::is(las, "LAScatalog")) {
      # automerge
      options <- list(automerge = TRUE)
      #
      # check buffer size
      if (lidR::opt_chunk_buffer(las) < 20)
        warning("For gap detection a buffer larger than 20 m is recommended to avoid border effects")
      # apply function to catalog
      output <- lidR::catalog_apply(
        las,
        gap_detection,
        res = res, 
        ratio = ratio,
        gap_max_height = gap_max_height,
        min_gap_surface = min_gap_surface,
        max_gap_surface = max_gap_surface,
        closing_height_bin = closing_height_bin,
        nl_filter = nl_filter,
        nl_size = nl_size,
        gap_reconstruct = gap_reconstruct,
        .options = options
      )
      return(output)
      #
    } else if (lidR::is(las, "LAScluster")) {
      #
      x <- lidR::readLAS(las)
      if (lidR::is.empty(x))
        return(NULL)
      #
      output <- gap_detection(
        x,
        res = res, 
        ratio = ratio,
        gap_max_height = gap_max_height,
        min_gap_surface = min_gap_surface,
        max_gap_surface = max_gap_surface,
        closing_height_bin = closing_height_bin,
        nl_filter = nl_filter,
        nl_size = nl_size,
        gap_reconstruct = gap_reconstruct
      )
      if (is.null(output)) return(NULL)
      # crop to chunk bounding box
      output <- terra::crop(output, terra::ext(las))
      return(output)
      #
    } else if (lidR::is(las, "LAS")) {
      # compute chm from points
      las <-
        lidR::rasterize_canopy(las, res = res, algorithm = lidR::p2r())
    }
    # convert to SpatRaster
    if (!inherits(las, "SpatRaster"))
    {
      chm <- convert_raster(las)
    } else {
      chm <- las
    }
    # convert to cimg object
    c_chm <- raster2Cimg(chm)
    # apply non linear filter to chm
    c_chm <- dem_filtering(c_chm, nl_filter = nl_filter, nl_size = nl_size)[[1]]
    # convert to raster for export
    r.nl <- cimg2Raster(c_chm, chm)
    #
    # check gap width
    if (is.null(ratio)) {
      l_im <- list(imager::as.cimg(c_chm > gap_max_height))
    } else {
      l_im <- list()
      # loop on dilate size -> canopy height (threshold at 60 m in case of outliers)
      for (i in seq(from = gap_max_height, 
                    to = max(gap_max_height, min(max(c_chm), 60)), 
                    by = closing_height_bin))
      {
        # create binary image to close (areas where chm> i)
        dummy <- imager::as.cimg(c_chm > i)
        # create stucturing element (uneven disk of radius i/2)
        strel <- imager::as.cimg(create_disk(floor(i / ratio / terra::xres(chm) / 2) * 2 + 1))
        # perform morphological closing and store in list
        l_im[[as.character(i)]] <- imager::mclosing(dummy, strel)
      }
    }
    #
    # union of closed images -> non gap areas
    final <- imager::parmax(l_im)
    # compute gap areas not closed
    gaps <- abs(final - 1)
    if (gap_reconstruct) {
      # extend non closed gaps into connected pixels where h < gap_max_height
      # map of pixels which comply with the height criterion (gap candidates)
      gaps_candidate <- c_chm < gap_max_height
      # label all gap candidates
      labels <- (imager::label(gaps_candidate) + 1) * gaps_candidate
      # list of labels of gaps not closed when applying the distance ratio
      not_closed_labels <- setdiff(unique(labels * gaps), 0)
      # remove closed labels
      gaps <- cimg2Raster(labels, chm)
      terra::values(gaps)[!is.element(terra::values(gaps), not_closed_labels)] <- 0
      gaps <- raster2Cimg(gaps > 0)
    }
    #
    # label unconnected gaps
    labels <- (imager::label(gaps) + 1) * gaps
    # extract gap surface
    gap_surface <- table(as.vector(labels)) * (terra::xres(chm))^2
    gap_surface <- as.data.frame(gap_surface)
    # remove non-gap category (0), except if only one gap
    if (nrow(gap_surface) > 1) {
      gap_surface <- gap_surface[-1, ]
    }
    #
    # convert gap label to raster object
    r_labels <- cimg2Raster(labels, chm)
    # set label of non gaps to NA
    r_labels[r_labels == 0] <- NA
    # create map where pixel value is gap size
    r_surface <- r_labels
    terra::values(r_surface) <- NA
    # PROBABLY A BETTER WAY TO DO IT THAN IN A LOOP
    for (i in 1:nrow(gap_surface))
    {
      r_surface[r_labels == as.numeric(as.character(gap_surface$Var1[i]))] <- gap_surface$Freq[i]
    }
    # set surface of non gaps to NA
    r_surface[is.na(r_labels)] <- NA
    # removal of labels with small or very large surface
    dummy <- (r_surface < min_gap_surface) | (r_surface > max_gap_surface)
    r_labels[dummy] <- r_surface[dummy] <- NA
    output <- c(r_labels, r_surface, r.nl)
    names(output) <- c("gap_id", "gap_surface", "filled_chm")
    return(output)
  }

#-------------------------------------------------------------------------------
#' Edge detection in gap image
#'
#' Performs edge detection on a gap image (e.g. output from function 
#' \code{\link{gap_detection}}). The gap image is compared to a gap image which 
#' has undergone a dilation or erosion to identify edges of gaps.
#'
#' @param gaps SpatRaster object. gaps image where 1 represents gaps and 0 non-gaps 
#' areas
#' @param inside boolean. defines where the edge is extracted: either inside the 
#' gaps (an erosion is applied to the gaps image) or outside (a dilation is applied)
#' @examples
#' data(chm_chablais3)
#' chm_chablais3 <- terra::rast(chm_chablais3)
#'
#' # fill NA values in canopy height model
#' chm_chablais3[is.na(chm_chablais3)] <- 0
#'
#' # gap detection with distance larger than canopy height / 2
#' gaps <- gap_detection(chm_chablais3,
#'   ratio = 2, gap_max_height = 1, min_gap_surface = 10,
#'   gap_reconstruct = TRUE
#' )
#'
#' # edge detection
#' edges_inside <- edge_detection(!is.na(gaps$gap_id))
#' edges_outside <- edge_detection(!is.na(gaps$gap_id), inside = FALSE)
#'
#' # edge proportion
#' sum(terra::values(edges_inside)) / (nrow(edges_inside) * ncol(edges_inside))
#' sum(terra::values(edges_outside)) / (nrow(edges_outside) * ncol(edges_outside))
#'
#' # plot original image
#' terra::plot(chm_chablais3, main = "Initial image")
#'
#' # plot binary image of gaps
#' terra::plot(gaps$gap_id > 0, main = "Gaps", col = "green", legend = FALSE)
#'
#' # plot edges
#' terra::plot(edges_inside, main = "Edges (inside)", legend = FALSE)
#' terra::plot(edges_outside, main = "Edges (outside)", legend = FALSE)
#' @seealso \code{\link{gap_detection}}
#' @return A SpatRaster object where edges are labelled as 1.
#' @export
edge_detection <- function(gaps, inside = TRUE) {
  # convert to cimg object
  c_gap <- raster2Cimg(gaps)
  # create structuring element before morphological operation
  mask <- create_disk(width = 3)
  # convert to cimg object
  c_mask <- imager::as.cimg(mask)
  # apply morphological operation
  if (inside) {
    c_morpho <- imager::erode(c_gap, c_mask)
  } else {
    c_morpho <- imager::dilate(c_gap, c_mask)
  }
  # output edges
  c_edges <- c_morpho != c_gap
  # convert to raster
  cimg2Raster(c_edges, gaps)
}
