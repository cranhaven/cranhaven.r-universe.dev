

#' Constructs a pairwise distance matrix based on the quantile cross-covariance
#' function
#'
#' \code{dis_qcf} returns a pairwise distance matrix based on a generalization of the
#' dissimilarity introduced by \insertCite{lafuente2016clustering;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param levels The set of probability levels.
#' @param max_lag The maximum lag considered to compute the cross-covariances.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{QCF}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{QCF}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_qcf(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_qcf
#' feature_dataset <- dis_qcf(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' \deqn{d_{QCF}(\boldsymbol X_T, \boldsymbol Y_T)=\Bigg(\sum_{l=1}^{L}\sum_{i=1}^{r}\sum_{i'=1}^{r}\sum_{j_1=1}^{d}
#' \sum_{j_2=1}^{d}\bigg(\widehat \gamma_{j_1,j_2}^{\boldsymbol X_T}(l,\tau_i,\tau_{i^\prime})-\widehat \gamma_{j_1,j_2}^{\boldsymbol Y_T}
#' (l,\tau_i,\tau_{i^\prime})\bigg)^2+}
#' \deqn{\sum_{i=1}^{r}\sum_{i'=1}^{r}\sum_{{j_1,j_2=1: j_1 > j_2}}^{d}
#' \bigg(\widehat \gamma_{j_1,j_2}^{\boldsymbol X_T}(0,\tau_i,\tau_{i^\prime})-
#' \widehat \gamma_{j_1,j_2}^{\boldsymbol Y_T}(0,\tau_i,\tau_{i^\prime})\bigg)^2\Bigg]^{1/2},}
#' where \eqn{\widehat \gamma_{j_1,j_2}^{\boldsymbol X_T}(l,\tau_i,\tau_{i^\prime})} and
#' \eqn{\widehat \gamma_{j_1,j_2}^{\boldsymbol Y_T}(l,\tau_i,\tau_{i^\prime})} are estimates of the quantile cross-covariances
#' with respect to the variables \eqn{j_1} and \eqn{j_2} and probability levels \eqn{\tau_i} and \eqn{\tau_{i^\prime}} for
#' series \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lafuente2016clustering}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_qcd}}
#' @export



dis_qcf <- function(X, levels = c(0.1, 0.5, 0.9), max_lag = 1, features = FALSE) {

  check_mts(X)
  check_mts_rows(X)
  l <- length(X)
  l_levels <- length(levels)
  c <- ncol(X[[1]])

  features_list <- list()



  # Feature extraction stage

  for (i in 1 : l) {

    features_list[[i]] <- auxiliary_qaf_function_2(X[[i]], levels = levels, max_lag = max_lag)

  }



  features_matrix <- listTomatrix(features_list)
  if (features == TRUE) {

    features_matrix

  } else {

    stats::dist(features_matrix) # Computation of distance matrix

  }



}
