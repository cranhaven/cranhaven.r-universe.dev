#' @title To get the clusters with maximal distance.
#' @description To get the clusters with the maximal distance value. By using the given \code{distance},
#' it gets the matrix index.
#' @param vector is a numeric vector
#' @param distance is a number. It should be in the matrix.
#' @details This function is part of the hierarchical clusterization method. The function uses the
#' \code{distance} value and gets the clustersId with the minimal distance.
#' @details For the divisive algorithm, it chooses the distances from a distances list.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A cluster.
#' @examples
#'
#' getClusterDivisive(2,c(1:10))
#'
#' getClusterDivisive(6,c(2,4,6,8,10,12))
#'
#' @export


getClusterDivisive <- function(distance,vector){
  found <- FALSE
  index <- 1
  while(!found & (index < length(vector))){

    if(vector[index] == distance){
      found <- TRUE
    } else {
      index <- index + 1
    }
  }
  cluster <- index %% (length(vector))
  cluster <- if(cluster == 0) (length(vector)) else cluster
  cluster
}

