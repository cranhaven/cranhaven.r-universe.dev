#' @title To show the formula and to return the Canberra distance.
#' @description To show the formula and to return the Canberra distance of two clusters.
#' @param x is a numeric vector or a matrix. It represents the values of a cluster.
#' @param y is a numeric vector or a matrix. It represents the values of a cluster.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' Canberra distance value from \code{x} and \code{y}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return canberra distance value with its formula.
#' @examples
#'
#' x <- c(1,2)
#' y <- c(1,3)
#'
#' cluster1 <- matrix(x,ncol=2)
#' cluster2 <- matrix(y,ncol=2)
#'
#' canberradistance(x,y)
#'
#' canberradistance(cluster1,cluster2)
#'
#' @export
#' @importFrom magick image_read

canberradistance.details <- function(x,y){
  initImages("../man/images/canberraDistance.PNG")
  canberraDistance <- 0
  if(((abs(y[1]) + abs(x[1])) == 0) | ((abs(y[2]) + abs(x[2])) == 0)){
      canberraDistance <- 0
  } else {
    canberraDistance <- (abs((y[1] - x[1]))/(abs(y[1]) + abs(x[1]))) + (abs((y[2] - x[2]))/(abs(y[2]) + abs(x[2])))
  }
  canberraDistance
}
