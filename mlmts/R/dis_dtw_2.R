

#' Constructs a pairwise distance matrix based on multivariate
#' dynamic time warping
#'
#' \code{dis_dtw_2} returns a pairwise distance matrix based on one of the multivariate
#' extensions of the well-known dynamic time warping distance \insertCite{shokoohi2017generalizing}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param normalization Logical. If \code{normalization = TRUE} (default), the normalized
#' distance is computed. Otherwise (default), no normalization is taken into account
#' @param ... Additional parameters for the function. See  \code{\link[dtw]{dtw}}.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_dtw_2(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_dtw1 without normalization
#' distance_matrix_normalized <- dis_dtw_2(toy_dataset, normalization = TRUE)
#' # Computing the pairwise distance matrix based
#' # distance matrix based on the distance dis_dtw1 with normalization
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS is defined as the multivariate extension of the
#' dynamic time warping distance which forces all dimensions to warp identically,
#' in a single warping matrix.
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

dis_dtw_2 <- function(X, normalization = FALSE,...) {

  check_mts(X)

  c <- ncol(X[[1]])
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {



        if (normalization == TRUE) {

          distance_matrix[i, j] <- dtw::dtw(X[[i]], X[[j]], distance.only = TRUE,...)$normalizedDistance

        } else {

          distance_matrix[i, j] <- dtw::dtw(X[[i]], X[[j]], distance.only = TRUE,...)$distance

        }


    }

    }

  }

  return(stats::as.dist(distance_matrix))

}

