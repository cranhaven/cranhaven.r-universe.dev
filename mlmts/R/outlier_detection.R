

#' Constructs the outlier detection procedure of López-Oriona and Vilar (2021)
#'
#' \code{outlier_detection} computes the outlier detection method for MTS proposed
#' by \insertCite{lopez2021outlier;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param levels The set of probability levels to compute the QCD-estimates.
#' @param alpha The desired rate of outliers to detect (a real number
#' between 0 and 1).
#' @return A list with two elements:
#' \itemize{
#' \item \code{Depths}. The functional depths associated with elements in \code{X}, sorted
#' in increasing order.
#' \item \code{Indexes}. The corresponding indexes associated with the
#' vector \code{Depths}.
#' }
#' @examples
#' outliers <- outlier_detection(SyntheticData2$data[c(1 : 3, 65)])
#' outliers$Indexes[1] # The first outlying MTS in dataset SyntheticData2
#' outliers$Depths[1] # The corresponding value for the depths
#' @details
#' This function performs outlier detection according to the procedure proposed
#' by \insertCite{lopez2021outlier;textual}{mlmts}. Specifically, each MTS in the original set is described by means of
#' a multivariate functional datum by using an estimate of its quantile cross- spectral
#'  density. Given the corresponding set of multivariate
#' functional data, the functional depth of each object is computed. Based on
#' depth computations, the outlying elements are the objects with low values
#' for the depths.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lopez2021outlier}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_qcd}}
#' @export


outlier_detection <- function(X, levels = c(0.1, 0.5, 0.9), alpha = NULL) {

  check_mts(X)
  l <- length(X)
  series_length <- nrow(X[[1]])
  c <- ncol(X[[1]])
  l_levels <- length(levels)


  # Extracting the QCD-based features

  qcd_features <- dis_qcd(X, levels = levels, features = TRUE)


  # Preparing the data for each curve

  n_points <- series_length/2 + 1
  n_curves <- 2 * c^2 * l_levels^2
  datasets <- list()

  for (i in 1 : n_curves) {

    datasets[[i]] <- matrix(nrow = l, ncol = n_points)

    for (j in 1 : l) {



      datasets[[i]][j,] <- qcd_features[j,][((i-1) * n_points+1) : (i * n_points)]


    }


  }


  # Converting the list of datasets to a list of fdata objects

  l_datasets <- length(datasets)
  datasets_fdata <- list()

  for (i in 1 : l_datasets) {

    datasets_fdata[[i]] <- fda.usc::fdata(datasets[[i]], rangeval = c(1, n_points), argvals = 1 : n_points)

  }


  # Performing outlier detection

  depth_fm <- fda.usc::depth.FMp(datasets_fdata, dfunc = fda.usc::mdepth.TD)$dep
  indexes <- order(depth_fm, decreasing = FALSE)

  if (is.null(alpha)) {

    sort_depth_fm <- sort(depth_fm, decreasing = FALSE)
    return_list <- list(Depths = sort_depth_fm, Indexes = indexes)
    return(return_list)

  } else {

    n_outliers <- ceiling(l * alpha)
    outliers <- indexes[1 : n_outliers]
    return(outliers)

  }


}
