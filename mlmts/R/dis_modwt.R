

#' Constructs a pairwise distance matrix based on the maximum overlap discrete wavelet transform
#'
#' \code{dis_modwt} returns a pairwise distance matrix based on the dissimilarity
#' introduced by \insertCite{d2012wavelets;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param wf The wavelet filter (default is 'd4').
#' @param J The maximum allowable number of scales.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{MODWT}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{MODWT}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_modwt(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_cor
#' feature_dataset <- dis_modwt(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @seealso \code{\link[waveslim]{modwt}}
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as
#' \deqn{d_{MODWT}(\boldsymbol X_T, \boldsymbol Y_T)=\Big|||\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{WV}-
#' \widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{WV}||^2+||\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{WC}-
#' \widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{WC}||^2\Big|^{1/2},}
#' where \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{WV}} and \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{WV}} are vectors
#' containing the estimated wavelet variances within \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively, and
#' \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol X_T}_{WC}} and \eqn{\widehat{\boldsymbol \theta}^{\boldsymbol Y_T}_{WC}} are vectors
#' containing the estimated wavelet correlations within \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{d2012wavelets}{mlmts}
#'
#' }
#' @export

dis_modwt <- function(X, wf = 'd4', J = floor(log(nrow(X[[1]])))-1, features = FALSE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])

  # Feature extraction stage


  matrix_modwt <-  matrix(nrow = l, ncol = length(c(auxiliary_modwt_function(X[[1]], wf = wf, J = J))))

  for (i in 1 : l) {

    matrix_modwt[i,] <- auxiliary_modwt_function(X[[i]], wf = wf, J = J)

  }



  if (features == TRUE) {

    matrix_modwt

  } else {

    stats::dist(matrix_modwt) # Computation of distance matrix

  }



}
