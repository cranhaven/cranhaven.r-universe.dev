#' The function twomodedistance computes the two mode (unfolding) distance
#'
#' @param U An N times S matrix with coordinates in S dimensional Euclidean space.
#' @param V An R times S matrix with coordinates in S dimensional Euclidean space.
#' @return D a N by R matrix with Euclidean distances
#' @importFrom Rfast dista
#' @export
twomodedistance = function(U, V){
  D = dista(U,V)
  #D = sqrt(outer(rowSums(U^2), rep(1, nrow(V))) + outer(rep(1, nrow(U)), rowSums(V^2)) - 2 * U %*% t(V))
  return(D)
}
