#' @title To show the formula of the Chebyshev distance.
#' @description To show the formula of the Chebyshev distance of two clusters.
#' @param x is a numeric vector or a matrix. It represents the values of a cluster.
#' @param y is a numeric vector or a matrix. It represents the values of a cluster.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' Chebyshev distance value from \code{x} and \code{y}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Chebyshev distance value and formula.
#' @examples
#'
#' x <- c(1,2)
#' y <- c(1,3)
#'
#' cluster1 <- matrix(x,ncol=2)
#' cluster2 <- matrix(y,ncol=2)
#'
#' chebyshevDistance(x,y)
#'
#' chebyshevDistance(cluster1,cluster2)
#'
#' @export

chebyshevDistance.details <- function(x,y){
  initImages("../man/images/chebyshevDistance.PNG")
  max((abs(y[1]) - abs(x[1])),(abs(y[2]) - abs(x[2])))
}

