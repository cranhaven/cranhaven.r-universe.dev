

#' Constructs a pairwise distance matrix based on the quantile cross-spectral
#' density (QCD)
#'
#' \code{dis_qcd} returns a pairwise distance matrix based on the
#' dissimilarity introduced by \insertCite{lopez2021quantile;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @param levels The set of probability levels.
#' @param freq Vector of frequencies in which the smoothed CCR-periodograms
#' must be computed. If \code{freq=NULL} (default), the set of Fourier
#' frequencies is considered.
#' @param ... Additional parameters for the function. See  \code{\link[quantspec]{smoothedPG}}.
#' @param features Logical. If \code{features = FALSE} (default), a distance matrix is returned. Otherwise, the function
#' returns a dataset of feature vectors.
#' @return If \code{features = FALSE} (default), returns a distance matrix based on the distance \eqn{d_{QCD}}. Otherwise, the function
#' returns a dataset of feature vectors, i.e., each row in the dataset contains the features employed to compute the
#' distance \eqn{d_{QCF}}.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 4] # Selecting the first 4 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_qcd(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_qcd
#' distance_matrix <- dis_qcd(toy_dataset, levels = c(0.4, 0.8)) # Changing
#' # the probability levels to compute the QCD-based estimators
#' distance_matrix <- dis_qcd(toy_dataset, freq = 0.5) # Considering only
#' # a single frequency for the computation of d_qcd
#' feature_dataset <- dis_qcd(toy_dataset, features = TRUE) # Computing
#' # the corresponding dataset of features
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' \deqn{d_{QCF}(\boldsymbol X_T, \boldsymbol Y_T)=\Bigg[\sum_{j_1=1}^{d}\sum_{j_2=1}^{d}\sum_{i=1}^{r}
#' \sum_{i'=1}^{r}\sum_{k=1}^{K}\Big(\Re\big({\widehat G_{j_1,j_2}^{\boldsymbol X_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})}\big)
#' -\Re\big({\widehat G_{j_1,j_2}^{\boldsymbol Y_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})\big)}\Big)^2+}
#' \deqn{\sum_{j_1=1}^{d}\sum_{j_2=1}^{d}\sum_{i=1}^{r}\sum_{i'=1}^{r}\sum_{k=1}^{K}\Big(\Im\big({\widehat G_{j_1,j_2}
#' ^{\boldsymbol X_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})}\big)
#' -\Im\big({\widehat G_{j_1,j_2}^{\boldsymbol Y_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})\big)}\Big)^2\Bigg]^{1/2},}
#' where \eqn{{\widehat G_{j_1,j_2}^{\boldsymbol X_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})}} and
#' \eqn{{\widehat G_{j_1,j_2}^{\boldsymbol Y_T}(\omega_{k}, \tau_{i}, \tau_{i^ {\prime}})}}
#' are estimates of the quantile cross-spectral densities (so-called smoothed CCR-periodograms)
#' with respect to the variables \eqn{j_1} and \eqn{j_2} and probability levels \eqn{\tau_i} and \eqn{\tau_{i^\prime}} for
#' series \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively, and \eqn{\Re(\cdot)} and \eqn{\Im(\cdot)}
#' denote the real part and imaginary part operators, respectively.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lopez2021quantile}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_qcf}}
#' @export

dis_qcd <- function(X, levels = c(0.1, 0.5, 0.9), freq  = NULL, features = FALSE,...) {

  check_mts(X)
  check_mts_rows(X)
  l <- length(X)
  c <- ncol(X[[1]])
  series_length <- nrow(X[[1]])
  l_levels <- length(levels)
  n_freq <- floor(series_length/2) + 1

  # Feature extraction stage

  matrix_psi <- matrix(nrow = l, ncol = 2*c^2 * l_levels^2 * n_freq)

  for (i in 1 : l) {

    matrix_psi[i,] <- auxiliary_qcd_function(X[[i]], levels = levels, freq = freq, type = 'clipped',...)

  }



  if (features == TRUE) {

    matrix_psi

  } else {

    stats::dist(matrix_psi) # Computation of distance matrix

  }




}
