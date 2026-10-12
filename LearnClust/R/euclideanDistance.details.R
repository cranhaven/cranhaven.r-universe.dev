#' @title To show the Euclidean distance formula.
#' @description To show the Euclidean distance formula and to calculate the Euclidean distance of two clusters.
#' @param x is a numeric vectoror a matrix. It represents the values of a cluster.
#' @param y is a numeric vectoror a matrix. It represents the values of a cluster.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' Euclidean distance value from \code{x} and \code{y}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Euclidean distance value and formula.
#' @examples
#'
#' x <- c(1,2)
#' y <- c(1,3)
#'
#' cluster1 <- matrix(x,ncol=2)
#' cluster2 <- matrix(y,ncol=2)
#'
#' edistance(x,y)
#'
#' edistance(cluster1,cluster2)
#'
#' @export

edistance.details <- function(x,y){
  initImages("../man/images/euclideanDistance.PNG")
  sqrt(((y[1] - x[1])^2) + ((y[2] - x[2])^2))
}
