#' @title To calculate the Octile distance.
#' @description To calculate the octile distance of two clusters.
#' @param x is a numeric vector or a matrix. It represents the values of a cluster.
#' @param y is a numeric vector or a matrix. It represents the values of a cluster.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' octile distance value from \code{x} and \code{y}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Octile distance value.
#' @examples
#'
#' x <- c(1,2)
#' y <- c(1,3)
#'
##' cluster1 <- matrix(x,ncol=2)
#' cluster2 <- matrix(y,ncol=2)
#'
#' octileDistance(x,y)
#'
#' octileDistance(cluster1,cluster2)
#'
#' @export


octileDistance <- function(x,y){
  D <- 1
  D2 <- sqrt(D + D)
  dx <- abs(x[1] - y[1])
  dy <- abs(x[2] - y[2])
  dist <- D * (dx + dy) + (D2 - 2 * D) * min(dx, dy)
  dist
}


