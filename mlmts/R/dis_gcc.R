


#' Constructs a pairwise distance matrix based on the generalized cross-correlation
#'
#' \code{dis_gcc} returns a pairwise distance matrix based on the generalized
#' cross-correlation measure introduced by \insertCite{alonso2019clustering;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param lag_max The maximum lag considered to compute the generalized cross-correlation.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{GCC}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{GCC}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_gcc(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_cor
#' feature_dataset <- dis_gcc(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined
#' as
#' \deqn{d_{GCC}(\boldsymbol X_T, \boldsymbol Y_T)=\Bigg[\sum_{j_1,j_2=1, j_1 \ne j_2}^{d}
#' \bigg(\widehat{GCC}(\boldsymbol X_{T,j_1}, \boldsymbol X_{T,j_2} )-\widehat{GCC}(\boldsymbol Y_{T,j_1},\boldsymbol Y_{T,j_2})\bigg)^2\Bigg]^{1/2},}
#' where \eqn{\boldsymbol X_{T,j}} and \eqn{\boldsymbol Y_{T,j}} are the \eqn{j}th dimensions (univariate time series) of
#' \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively, and \eqn{\widehat{GCC}(\cdot, \cdot)} is the estimated genelarized cross-correlation
#' measure between univariate series proposed by \insertCite{alonso2019clustering;textual}{mlmts}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{alonso2019clustering}{mlmts}
#'
#' }
#' @export


dis_gcc <- function(X, lag_max = 1, features = FALSE) {

  # Usual checkings

  check_mts(X)
  l <- length(X)
  c <- ncol(X[[1]])


  # Feature extraction stage


  matrix_gcc <-  matrix(nrow = l, ncol = (c*(c-1)/2) * lag_max)

  for (i in 1 : l) {

    matrix_gcc[i,] <- auxiliary_gcc_function_2(X[[i]], lag_max = lag_max)

  }



  if (features == TRUE) {

    matrix_gcc

  } else {

    stats::dist(matrix_gcc) # Computation of distance matrix

  }

}
