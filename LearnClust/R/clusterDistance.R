#' @title To calculate the distance between clusters.
#' @description To calculate the distance between clusters depending on the approach and distance type.
#' @param cluster1 is a matrix
#' @param cluster2 is a matrix
#' @param approach is a string. Type of function to apply.
#' @param distance is a string. Type of distance to use.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' final distance between \code{cluster1} and \code{cluster2} applying the approach definition, using the distance type given.
#' @details \code{approach} indicates the algorithm used to get the value. \code{distance} indicates the distance used to get the value. Possible values: {\code{'MAX'},
#' \code{'MIN'}, \code{'AVG'}}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Distance between clusters.
#' @examples
#'
#' cluster1 <- matrix(c(1,2),ncol=2)
#' cluster2 <- matrix(c(1,4),ncol=2)
#'
#' clusterDistance.details(cluster1,cluster2,'AVG','MAN')
#'
#' clusterDistance.details(cluster1,cluster2,'MAX','OCT')
#'
#' @export


clusterDistance <-
  function(cluster1,cluster2,approach,distance){
    clusterDist <- 0
    res <- c()
    for (i in seq_len(nrow(cluster1))){
      for (j in seq_len(nrow(cluster2))){
        if(distance == 'EUC'){
          dist <- edistance(cluster1[i,],cluster2[j,])
        } else if (distance == 'MAN'){
          dist <- mdistance(cluster1[i,],cluster2[j,])
        } else if (distance == 'CAN'){
          dist <- canberradistance(cluster1[i,],cluster2[j,])
        } else if (distance == 'CHE'){
          dist <- chebyshevDistance(cluster1[i,],cluster2[j,])
        } else if (distance == 'OCT'){
          dist <- octileDistance(cluster1[i,],cluster2[j,])
        }
        res <- c(res, dist)
      }
    }
    clusterDist <- clusterDistanceByApproach(res,approach)
  }


