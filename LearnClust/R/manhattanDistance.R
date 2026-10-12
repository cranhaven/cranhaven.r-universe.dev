#' @title To calculate the Manhattan distance.
#' @description To calculate the Manhattan distance of two clusters.
#' @param x is a numeric vectoror a matrix. It represents the values of a cluster.
#' @param y is a numeric vectoror a matrix. It represents the values of a cluster.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' Manhattan distance value from \code{x} and \code{y}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Manhattan distance value.
#' @examples
#'
#' x <- c(1,2)
#' y <- c(1,3)
#'
#' cluster1 <- matrix(x,ncol=2)
#' cluster2 <- matrix(y,ncol=2)
#'
#' mdistance(x,y)
#'
#' mdistance(cluster1,cluster2)
#'
#' @export


mdistance <-
function(x,y){
    abs(y[1] - x[1]) + abs(y[2] - x[2])
}



