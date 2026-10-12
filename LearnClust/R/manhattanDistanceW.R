#' @title To calculate the Manhattan distance applying weights.
#' @description To calculate the Manhattan distance between clusters applying weights given.
#' @param cluster1 is a cluster.
#' @param cluster2 is a cluster.
#' @param weight is a numeric vector.
#' @details The function calculates the Manhattan distance value from \code{cluster1} and \code{cluster2}, applying weights to the cluster's components.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Manhattan distance applying weights value.
#' @examples
#'
#' cluster1 <- matrix(c(1,2),ncol=2)
#' cluster2 <- matrix(c(1,3),ncol=2)
#'
#' weight1 <- c(0.4,0.6)
#' weight2 <- c(2,12)
#'
#' mdistanceW(cluster1,cluster2,weight1)
#'
#' mdistanceW(cluster1,cluster2,weight2)
#'
#' @export

mdistanceW <-
  function(cluster1,cluster2,weight){
    res <- 0
    if(is.null(weight)){
      buffer <- 0
      for (index in c(1:ncol(cluster1))) {
        aux <- abs(cluster2[index]-cluster1[index])
        buffer <- buffer + aux
      }
      res <- buffer
    } else {
      buffer <- 0
      for (index in c(1:ncol(cluster1))) {
        aux <- weight[[index]] * abs(cluster2[index]-cluster1[index])
        buffer <- buffer + aux
      }
      res <- buffer
    }
    res
  }
