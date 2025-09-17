#' Samples a multivariate normal with a Kronecker product covariance structure
#'
#' Samples \eqn{x ~ N(0, AxB)}
#'
#' @export
#' 
#' @param A an \eqn{m x n} matrix
#' @param B an \eqn{p x q} matrix
#' 
#' @examples
#' 
#' A = matrix(c(1,.5,.5,1), ncol=2)
#' B = diag(2)
#' x = kronSamp(A, B)
#' 
kronSamp = function(A, B) {
  y = matrix(0, nrow = nrow(A) * nrow(B), ncol=1)
  .Call(`_telefit_r_mvrnorm_postKron`, y, solve(A), solve(B), 1, TRUE)
}
