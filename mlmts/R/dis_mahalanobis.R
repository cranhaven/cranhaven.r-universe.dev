
#' Constructs a pairwise distance matrix based on the Mahalanobis distance
#'
#' \code{dis_mahalanobis} returns a pairwise distance matrix based on the
#' Mahalanobis divergence introduced by \insertCite{singhal2005clustering;textual}{mlmts}.
#'
#' @param X A list of MTS (numerical matrices).
#' @return The computed pairwise distance matrix.
#' @examples
#' toy_dataset <- AtrialFibrillation$data[1 : 10] # Selecting the first 10 MTS from the
#' # dataset AtrialFibrillation
#' distance_matrix <- dis_mahalanobis(toy_dataset) # Computing the pairwise
#' # distance matrix based on the distance dis_mahalanobis.
#' @details
#' Given a collection of MTS, the function returns the pairwise distance matrix,
#' where the distance between two MTS \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T} is defined as
#' \deqn{d_{MD}^*(\boldsymbol X_T, \boldsymbol Y_T)=\frac{1}{2}\Big(d_{MD}
#' (\boldsymbol X_T, \boldsymbol Y_T)+d_{MD}(\boldsymbol Y_T, \boldsymbol X_T)\Big),}
#' with \deqn{	d_{MD}(\boldsymbol X_T, \boldsymbol Y_T)=\sqrt{(\overline{\boldsymbol X}_T
#' -\overline{\boldsymbol Y}_T)\boldsymbol \Sigma_{\boldsymbol X_T}^{*-1}(\overline
#' {\boldsymbol X}_T-\overline{\boldsymbol Y}_T)^\top},}
#' where \eqn{\overline{\boldsymbol X}_T} and \eqn{\overline{\boldsymbol Y}_T}
#' are vectors containing the column-wise means concerning series
#' \eqn{\boldsymbol X_T} and \eqn{\boldsymbol Y_T}, respectively,
#' \eqn{\boldsymbol \Sigma_{\boldsymbol X_T}} is the covariance matrix of \eqn{\boldsymbol X_T} and
#' \eqn{\boldsymbol \Sigma_{\boldsymbol X_T}^{*-1}} is the pseudo-inverse of \eqn{\boldsymbol
#' \Sigma_{\boldsymbol X_T}} calculated using SVD.
#' In the computation of \eqn{d_{MD}^*}, MTS \eqn{\boldsymbol X_T} is assumed to be the reference series.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{singhal2005clustering}{mlmts}
#'
#' }
#' @seealso
#' \code{\link{dis_mahalanobis_dtw}}
#' @export

dis_mahalanobis <- function(X) {

  check_mts(X)
  l <- length(X)
  distance_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    if(i > 1) {for (j in 1 : (i - 1)) {

      distance_matrix[i, j] <- auxiliary_mahalanobis_function_extra(X[[i]], X[[j]])

    }

    }

  }

  return(stats::as.dist(distance_matrix))

}

