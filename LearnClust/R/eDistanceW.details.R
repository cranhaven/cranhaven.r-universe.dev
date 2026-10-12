#' @title To calculate the Euclidean distance applying weights.
#' @description To explain how to calculate the Euclidean distance between clusters applying weights given.
#' @param cluster1 is a cluster.
#' @param cluster2 is a cluster.
#' @param weight is a numeric vector.
#' @details The function calculates the Euclidean distance value from \code{cluster1} and \code{cluster2}, applying weights to the cluster's components.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Euclidean distance applying weights value. Explanation.
#' @examples
#'
#' cluster1 <- matrix(c(1,2),ncol=2)
#' cluster2 <- matrix(c(1,3),ncol=2)
#'
#' weight1 <- c(0.4,0.6)
#' weight2 <- c(2,12)
#'
#' edistanceW.details(cluster1,cluster2,weight1)
#'
#' edistanceW.details(cluster1,cluster2,weight2)
#'
#' @export

edistanceW.details <- function(cluster1,cluster2,weight){
  message("\n This function calculates the euclidean distance applying some weight to each element in the clusters.\n")
  message("\n It allows the algorithm to use some categories more importante than the others. \n")
  res <- 0
  if(is.null(weight)){
    message("\n Due to there is not weight, the formula does not change. \n")
    buffer <- 0
    for (index in c(1:ncol(cluster1))) {
      aux <- (cluster2[index]-cluster1[index])^2
      buffer <- buffer + aux
    }
    res <- sqrt(buffer)
  } else {
    message("\n Due to there is weight, the formula has to multiply values by each weight. \n")
    buffer <- 0
    for (index in c(1:ncol(cluster1))) {
      aux <- weight[[index]] * (cluster2[index]-cluster1[index])^2
      buffer <- buffer + aux
    }
    res <- sqrt(buffer)
  }
  message("\n Euclidean distance is ", res," \n")
  res
}
