#' @title To calculate the Octile distance applying weights.
#' @description To calculate the Octile distance between clusters applying weights given.
#' @param cluster1 is a cluster.
#' @param cluster2 is a cluster.
#' @param weight is a numeric vector.
#' @details The function calculates the Octile distance value from \code{cluster1} and \code{cluster2}, applying weights to the cluster's components.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Octile distance applying weights value.
#' @examples
#'
#' cluster1 <- matrix(c(1,2),ncol=2)
#' cluster2 <- matrix(c(1,3),ncol=2)
#'
#' weight1 <- c(0.4,0.6)
#' weight2 <- c(2,12)
#'
#' octileDistanceW(cluster1,cluster2,weight1)
#'
#' octileDistanceW(cluster1,cluster2,weight2)
#'
#' @export

octileDistanceW <- function(cluster1,cluster2,weight){
  D <- 1
  D2 <- sqrt(D + D)
  value <- 0
  minimal <- c()
  if(is.null(weight)){
    for (index in c(1:ncol(cluster1))) {
      aux <- abs(cluster1[index] - cluster2[index])
      value <- value + aux
      minimal <- c(minimal, aux)
    }
  } else {
    for (index in c(1:ncol(cluster1))) {
      aux <- weight[index] * abs(cluster1[index] - cluster2[index])
      value <- value + aux
      minimal <- c(minimal, aux)
    }
  }
  dist <- D * value + (D2 - 2 * D) * min(minimal)
  dist
}
