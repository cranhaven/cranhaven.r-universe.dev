

#' Constructs a pairwise distance matrix based on multivariate
#' dynamic time warping
#'
#' \code{dis_dtw_1} returns a pairwise distance matrix based on one of the multivariate
#' extensions of the well-known dynamic time warping distance \insertCite{shokoohi2017generalizing}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param normalization Logical. If \code{normalization = TRUE} (default), the normalized
#' distance is computed. Otherwise (default), no normalization is taken into account
#' @param ... Additional parameters for the function. See  \code{\link[dtw]{dtw}}.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 5] # Selecting the first 5 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_dtw_1(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_dtw_1 without normalization
#' distance_matrix_normalized <- dis_dtw_1(toy_dataset, normalization = TRUE)
#' # Computing the pairwise distance matrix based
#' # on the distance dis_dtw_1 with normalization
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS is defined as the sum of the standard
#' dynamic time warping distances between each corresponding pair of dimensions (univariate
#' time series)
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{shokoohi2017generalizing}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_dtw_2}}, \code{\link{dis_mahalanobis_dtw}}
#' @export

dis_dtw_1 <- function(X, normalization = FALSE,...) {

  check_mts(X)

  c <- ncol(X[[1]])
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      distance <- numeric(c)

      for (k in 1 : c) {

        if (normalization == TRUE) {

        distance[k] <- dtw::dtw(X[[i]][,k], X[[j]][,k], distance.only = TRUE,...)$normalizedDistance

        } else {

          distance[k] <- dtw::dtw(X[[i]][,k], X[[j]][,k], distance.only = TRUE,...)$distance

        }

      }

      distance_matrix[i, j] <- sum(distance)

    }

    }

  }

  return(stats::as.dist(distance_matrix))

}
