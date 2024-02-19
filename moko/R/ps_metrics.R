#' Distance between vector and matrix
#'
#' This function computes and returns the minimum distance between a vector and
#' a matrix
#'
#' @param point numeric vector
#' @param set numeric matrix
#' @param method String stating which distance measure to be used. This must be one of:
#'   "euclidean" or "manhattan" (default).
#' @return numeric value indicating the minimum distance between \code{point} and \code{set}.
pdist <- function(point, set, method = "manhattan"){
  point <- t(as.matrix(point))
  dif <- t(apply(set, 1, function(set) set-point))
  if(method == "manhattan")
    d <- apply(dif, 1, function(dif) sum(abs(dif)))
  if(method == "euclidean")
    d <- apply(dif, 1, function(dif) sum((dif)^2)^0.5)
  return(min(d))
}

#' IGD: Inverted Generational Distance
#'
#' The IGD is a performance measure function of Pareto front fidelity and
#' corresponds to the average distance between all designs in the true set and
#' the closest design of the current set. Thus, the lower the IGD value, the
#' better the front is.
#'
#' @references Shimoyama, K., Jeong, S., & Obayashi, S. (2013, June).
#'   Kriging-surrogate-based optimization considering expected hypervolume
#'   improvement in non-constrained many-objective test problems. In 2013 IEEE
#'   \emph{Congress on Evolutionary Computation} (pp. 658-665). IEEE.
#'
#' @param aps An object of type \code{\link{ps}} containing the "actual" Pareto front
#' @param tps An object of type \code{\link{ps}} containing the "true" Pareto front
#' @param norm Logical (default: \code{TRUE}) indicating if both fronts should be normalized.
#' @inheritParams pdist
#'
#' @return returns the IGD metric
#' @export
#' @examples
#' \dontrun{
#' aps <- ps(matrix(rnorm(1:1000),ncol=2))
#' tps <- ps(matrix(rnorm(1:2000),ncol=2))
#' igd(aps,tps)
#'
#' tps <-nowacki_beam_tps$set[1:50 * 10,]
#' aps <- tps * 1.2
#' igd(aps,tps)
#' }
igd <- function(aps, tps, method = "manhattan", norm = TRUE){
  if(class(aps) == 'ps')
    aps <- aps$set
  if(class(tps) == 'ps')
    tps <- tps$set

  if (norm){
    ran <- apply(rbind(aps, tps), 2, range)
    aps <- normalize(aps, ran)
    tps <- normalize(tps, ran)
  }
  n <- nrow(tps)
  d <- apply(tps, 1, function(point) pdist(point, aps, method))
  igd <- sum(d)/n
  return(igd)
}
