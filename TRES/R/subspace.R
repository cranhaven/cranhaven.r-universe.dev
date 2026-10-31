#' The distance between two subspaces.
#'
#' This function calculates the distance between the two subspaces with equal dimensions span\eqn{(A)} and span\eqn{(B)}, where \eqn{A \in R^{p\times u}} and \eqn{B \in R^{p\times u}} are the basis matrices of two subspaces. The distance is defined as
#' \deqn{\|P_{A} - P_{B}\|_F/\sqrt{2d},}
#' where \eqn{P} is the projection matrix onto the given subspace with the standard inner product, and \eqn{d} is the common dimension.
#'
#' @param  A A \eqn{p}-by-\eqn{u} full column rank matrix.
#' @param  B A \eqn{p}-by-\eqn{u} full column rank matrix.
#'
#' @return Returns a distance metric that is between 0 and 1
#'
#' @export

subspace <- function(A,B){
  Pa <- qr.Q(qr(A))
  Pa <- tcrossprod(Pa)
  Pb <- qr.Q(qr(B))
  Pb <- tcrossprod(Pb)
  u <- dim(A)[2]
  return(sqrt(sum((Pa-Pb)^2))/sqrt(2*u))
}
