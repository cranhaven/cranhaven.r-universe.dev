#' Samples an Inverse-Wishart matrix
#'
#' Samples \eqn{W ~ IW(Psi, n)}
#'
#' @export
#' 
#' @param Psi an \eqn{n x n} scale matrix
#' @param n degrees of freedom parameter
#' 
#' @examples
#' 
#' A = matrix(c(1,.5,.5,1), ncol=2)
#' W = invWSamp(A, 3)
#' 
invWSamp = function(Psi, n) {
  .Call(`_telefit_r_mc2_rinvwishart`, Psi, n)
}