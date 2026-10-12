#' @title To get the clusters with minimal distance.
#' @description To get the clusters with the minimal distance value. By using the given \code{distance},
#' it gets the matrix index.
#' @param matrix is a numeric matrix.
#' @param distance is a number. It should be in the matrix.
#' @details This function is part of the hierarchical clusterization method. The function uses the
#' \code{distance} value and gets the clustersId with the minimal distance.
#' @details For the divisive algorithm, it chooses the distances from a distances list.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return numeric vector with two cluster indexs.
#' @examples
#'
#' matrixExample <- matrix(c(1:10), ncol=2)
#'
#' getCluster(2,matrixExample)
#'
#' @export


getCluster <- function(distance,matrix){
    found <- FALSE
    index <- 1
    while(!found & (index < length(matrix))){

        if(matrix[index] == distance){
            found <- TRUE
        } else {
            index <- index + 1
        }
    }
    cluster1 <- trunc(index/(nrow(matrix))) + 1
    cluster1 <- if((index %% (nrow(matrix))) == 0) (cluster1 - 1) else cluster1
    cluster2 <- index %% (nrow(matrix))
    cluster2 <- if(cluster2 == 0) (nrow(matrix)) else cluster2
    clusters <- c(cluster1,cluster2)
}

