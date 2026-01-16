

#' Constructs a pairwise distance matrix based on a dissimilarity combining
#' both the dynamic time warping and the Mahalanobis distance.
#'
#' \code{dis_mahalanobis_dtw} returns a pairwise distance matrix based on a
#' dynamic time warping distance in which the local cost matrix is computed
#' by using the Mahalanobis distance \insertCite{mei2015learning}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param M The matrix with respect to compute the Mahalanobis distance
#' (default is the covariance matrix of concatenation of all MTS objects
#' by rows).
#' @param ... Additional parameters for the function. See  \code{\link[dtw]{dtw}}.
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- Libras$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset Libras
#' distance_matrix <- dis_mahalanobis_dtw(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_mahalanobis_dtw
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' a dynamic time warping-type distance in which the local cost matrix is
#' constructed by using the Mahalanobis distance.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{mei2015learning}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_dtw_1}}, \code{\link{dis_dtw_2}}, \code{\link{dis_mahalanobis_dtw}}
#' @export

dis_mahalanobis_dtw <- function(X, M = NULL,...) {

  check_mts(X)
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  if (is.null(M)) {

    M <- stats::cov(do.call(rbind, X))

  }

  distance_matrix <- matrix(0, l, l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      cost_matrix <- auxiliary_dtw_mahalanobis_function_extra(X[[i]], X[[j]])
      distance_matrix[i, j] <- dtw::dtw(cost_matrix,...)$distance

    }

    }

  }

  return(stats::as.dist(distance_matrix))

}
