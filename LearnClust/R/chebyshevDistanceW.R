#' @title To calculate the Chebyshev distance applying weights.
#' @description To calculate the Chebyshev distance between clusters applying weights given.
#' @param cluster1 is a cluster.
#' @param cluster2 is a cluster.
#' @param weight is a numeric vector.
#' @details The function calculates the Chebyshev distance value from \code{cluster1} and \code{cluster2}, applying weights to the cluster's components.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Chebyshev distance applying weights value.
#' @examples
#'
#' cluster1 <- matrix(c(1,2),ncol=2)
#' cluster2 <- matrix(c(1,3),ncol=2)
#'
#' weight1 <- c(0.4,0.6)
#' weight2 <- c(2,12)
#'
#' chebyshevDistanceW(cluster1,cluster2,weight1)
#'
#' chebyshevDistanceW(cluster1,cluster2,weight2)
#'
#' @export

chebyshevDistanceW <- function(cluster1,cluster2,weight){
  buffer <- c()
  if(is.null(weight)){
    for (index in c(1:ncol(cluster1))) {
      aux <- abs(cluster2[index] - cluster1[index])
      buffer <- c(buffer, aux)
    }
    res <- max(buffer)
  } else {
    for (index in c(1:ncol(cluster1))) {
      aux <- weight[index] * abs(cluster2[index] - cluster1[index])
      buffer <- c(buffer, aux)
    }
    res <- max(buffer)
  }
  res
}
