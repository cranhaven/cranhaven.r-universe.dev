# ===============================================================================
#
# Developers:
#
# Tiago de Conto - tdc.florestal@gmail.com -  https://github.com/tiagodc/
#
# COPYRIGHT: Tiago de Conto, 2020
#
# This piece of software is open and free to use, redistribution and modifications
# should be done in accordance to the GNU General Public License >= 3
# Use this software as you wish, but no warranty is provided whatsoever. For any
# comments or questions on TreeLS, please contact the developer (prefereably through my github account)
#
# If publishing any work/study/research that used the tools in TreeLS,
# please don't forget to cite the proper sources!
#
# Enjoy!
#
# ===============================================================================
#' Stem points classification
#' @description Classify stem points of all trees in a \strong{normalized}
#' point cloud. Stem denoising methods are prefixed by \code{stm}.
#' This file includes code derived from the TreeLS package by Tiago de Conto
#' Original source: https://github.com/tiagodc/TreeLS
#' License: GPL-3
#' The code below is copied and adapted from TreeLS::stemPoints for the purpose
#' of maintaining CRAN compatibility. All modifications are clearly documented.
#' @param las \code{\link[lidR:LAS]{LAS}} object.
#' @param method Function to classify stems. Default: \code{\link{stm.hough}}.
#' @return \code{\link[lidR:LAS]{LAS}} object.
#' @references
#' Carvalho, T. (2017). TreeLS: Tools for Terrestrial LiDAR in R.
#'   GitHub: https://github.com/tiagodc/TreeLS
#' @export
#' @note This function includes code derived from TreeLS::stemPoints
#'   (GPL-3 license). See source for details.
#'   #' @examples
#' library(lidR)
#' library(CrownScorchTLS)
#'
#' #download external data from github repo
#' url <- paste0(
#'   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/",
#'   "M-04-15549_post.laz")
#'  las_file = tempfile(fileext = paste0(".", tools::file_ext(url)))
#'  download.file(url, las_file, mode = "wb", quiet = TRUE)
#'  las <- readLAS(las_file)
#'
#'  # or load your own data
#'  #las <- readLAS('C:/path/to/your/file.laz')
#'
#'  las$Z = las$Z - min(las$Z)  # Normalize las
#'  las <- stemPoints(las)  # Classify stem points
#'  #plot(las, color='Stem')
stemPoints = function(las, method = stm.hough()){

  if(!'TreeID' %in% colnames(las@data)){
    rg = as.double(apply(las@data[,1:2], 2, function(x) max(x) - min(x)))
    if(any(rg > 15))
      message("point cloud unlikely a single tree (XY extents too large)")
  }
  if(max(las$Z) < 0)
    stop('input Z coordinates are all negative')
  if(abs(min(las$Z)) > 0.5)
    warning('point cloud apparently not normalized')
  las = method(las)
  las@data[is.na(las$Stem)]$Stem = FALSE
  las@data$Stem <- as.logical(las@data$Stem)
  return(las)
}


#' Stem denoising algorithm: Hough Transform
#' @description This function is meant to be used inside \code{\link{stemPoints}}. It applies an adapted version of the Hough Transform for circle search. Mode details are given in the sections below.
#' This file includes code derived from the TreeLS package by Tiago de Conto
#' Original source: https://github.com/tiagodc/TreeLS
#' License: GPL-3
#' The code below is copied and adapted from TreeLS::stemPoints for the purpose
#' of maintaining CRAN compatibility. All modifications are clearly documented.
#' @return \code{\link[lidR:LAS]{LAS}} object.
#' @param h_step \code{numeric} - height interval to perform point filtering/assignment/classification.
#' @param max_d \code{numeric} - largest tree diameter expected in the point cloud.
#' @param h_base \code{numeric} vector of length 2 - tree base height interval to initiate circle search.
#' @param pixel_size \code{numeric} - pixel side length to discretize the point cloud layers while performing the Hough Transform circle search.
#' @param min_density \code{numeric} - between 0 and 1 - minimum point density within a pixel evaluated on the Hough Transform - i.e. only \emph{dense} point clousters will undergo circle search.
#' @param min_votes \code{integer} - Hough Transform parameter - minimum number of circle intersections over a pixel to assign it as a circle center candidate.
#' @references
#' Carvalho, T. (2017). TreeLS: Tools for Terrestrial LiDAR in R.
#'   GitHub: https://github.com/tiagodc/TreeLS
#' @note This function includes code derived from TreeLS::stemPoints
#'   (GPL-3 license). See source for details.
#' @section \code{LAS@data} Special Fields:
#'
#' Meaninful new fields in the output:
#'
#' \itemize{
#' \item \code{Stem}: \code{TRUE} for stem points
#' \item \code{Segment}: stem segment number (from bottom to top and nested with TreeID)
#' \item \code{Radius}: approximate radius of the point's stem segment estimated by the Hough Transform - always a multiple of the \code{pixel_size}
#' \item \code{Votes}: votes received by the stem segment's center through the Hough Transform
#' }#'
#' @section Adapted Hough Transform:
#'
#' The Hough Transform circle search algorithm used in
#' TreeLS applies a constrained circle search on discretized
#' point cloud layers. Tree-wise, the circle search is
#' recursive, in which the search for circle parameters
#' of a stem section is constrained to the
#' \emph{feature space} of the stem section underneath it.
#' Initial estimates of the stem's \emph{feature space}
#' are performed on a \emph{baselise} stem segment - i.e.
#' a low height interval where a tree's bole is expected
#' to be clearly visible in the point cloud.
#' The algorithm is described in detail by Conto et al. (2017).
#'
#' This adapted version of the algorithm is very robust against outliers,
#' but not against forked or leaning stems.
#' @references
#' Olofsson, K., Holmgren, J. & Olsson, H., 2014. Tree stem and height measurements using terrestrial laser scanning and the RANSAC algorithm. Remote Sensing, 6(5), pp.4323–4344.
#' @references
#' Conto, T. et al., 2017. Performance of stem denoising and stem modelling algorithms on single tree point clouds from terrestrial laser scanning. Computers and Electronics in Agriculture, v. 143, p. 165-176.
stm.hough = function(h_step=0.5, max_d=0.5, h_base = c(1,2.5), pixel_size=0.025, min_density=0.1, min_votes=3){

  if(length(h_base) != 2)
    stop('h_base must be a numeric vector of length 2')

  if(diff(h_base) <= 0)
    stop('h_base[2] must be larger than h_base[1]')

  params = list(
    h_step = h_step,
    max_d = max_d,
    pixel_size = pixel_size,
    min_density = min_density,
    min_votes = min_votes
  )

  for(i in names(params)){
    val = params[[i]]

    if(length(val) != 1)
      stop( paste(i, 'must be of length 1') )

    if(!is.numeric(val))
      stop( paste(i, 'must be Numeric') )

    if(val <= 0)
      stop( paste(i, 'must be positive') )
  }

  if(min_density > 1)
    stop('min_density must be between 0 and 1')

  func = function(las){

    if(min(las$Z) < 0)
      message("points with Z below 0 will be ignored")

    if(min(las$Z) > 5)
      message("point cloud doesn't look normalized (Z values too high) - check ?tlsNormalize")

    survey_points = if(!'Classification' %in% colnames(las@data)){
      las$Classification != 2
    } else{
      rep(TRUE, nrow(las@data))
    }

    if(!'TreeID' %in% colnames(las@data)){
      message('no TreeID field found with tree_points signature: performing single stem point classification')
      results = houghStemPoints(las2xyz(las)[survey_points,], h_base[1], h_base[2], h_step, max_d/2, pixel_size, min_density, min_votes)
    }else{
      message('performing point classification on multiple stems')
      survey_points = survey_points & las$TreeID > 0
      results = houghStemPlot(las2xyz(las)[survey_points,], las@data$TreeID[survey_points], h_base[1], h_base[2], h_step, max_d/2, pixel_size, min_density, min_votes)
    }

    las@data$Stem = FALSE
    las@data$Stem[survey_points] = results$Stem

    las@data$Segment = 0
    las@data$Segment[survey_points] = results$Segment

    las@data$Radius = 0
    las@data$Radius[survey_points] = results$Radius

    las@data$Votes = 0
    las@data$Votes[survey_points] = results$Votes

    las = cleanFields(las, c('Radius', 'Votes'))

    return(las)

  }

  attr(func, "stem_pts_mtd") <- TRUE

  return(func)

}

las2xyz = function(las){

  if(class(las)[1] != "LAS")
    stop("las must be a LAS object")

  las = as.matrix(las@data[,c('X','Y','Z')])
  return(las)
}

hasField = function(las, field_name){
  if(class(las)[1] == 'LAS') las = las@data
  return(any(colnames(las) == field_name))
}

cleanFields = function(las, field_names){
  is_las = class(las)[1] == 'LAS'
  for(i in field_names){
    temp = if(is_las) las@data[,i,with=F] else las[,i,with=F]
    temp = unlist(temp)
    temp[is.na(temp) | is.nan(temp) | is.infinite(temp) | is.null(temp)] = ifelse(is.logical(temp), F, 0)
    if(is_las) las@data[,i] = temp else las[,i] = temp
  }
  return(las)
}

