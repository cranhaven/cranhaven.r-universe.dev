

#' Constructs a pairwise distance matrix based on auto and cross-correlations
#'
#' \code{dis_cor} returns a pairwise distance matrix based on a generalization of the
#' dissimilarity introduced by \insertCite{d2009autocorrelation;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param lag_max The maximum lag considered to compute the auto and cross-correlations.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{COR}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{COR}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_cor(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_cor
#' distance_matrix <- dis_cor(toy_dataset, lag_max = 5) # Considering
#' # auto and cross-correlations up to lag 5 in the computation of the distance
#' feature_dataset <- dis_cor(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as
#' \deqn{d_{COR}(\boldsymbol X_T, \boldsymbol Y_T)=\Big|||\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{AC}-
#' \widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{AC}||^2+||\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{CC}-
#' \widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{CC}||^2\Big|^{1/2},}
#' where \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{AC}} and \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{AC}} are vectors
#' containing the estimated autocorrelations within \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively, and
#' \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{CC}} and \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{CC}} are vectors
#' containing the estimated cross-correlations within \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{d2009autocorrelation}{mlmts}
#'
#' }
#' @export


dis_cor <- function(X, lag_max = 1, features = FALSE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])

  n_cross_lag_0 <- c * (c - 1)/2


  # Feature extraction stage


  matrix_cor <-  matrix(nrow = l, ncol = lag_max * c + n_cross_lag_0 + 2 * lag_max * n_cross_lag_0)

  for (i in 1 : l) {

    matrix_cor[i,] <- auxiliary_cor_function(X[[i]], lag_max = lag_max)

  }



  if (features == TRUE) {

    matrix_cor

  } else {

    stats::dist(matrix_cor) # Computation of distance matrix

  }

}
