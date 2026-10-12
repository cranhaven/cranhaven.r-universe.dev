#' @title To calculate distances applying weights.
#' @description To calculate distances between two clusters applying weights depending on the distance type.
#' @param cluster1 is a matrix.
#' @param cluster2 is a matrix.
#' @param distance is a string. The distance type to apply.
#' @param weight is a numeric vector.
#' @details This function calculates distance applying \code{distance} type and applying each weight to its characteristic.
#' @details Distance type could be \code{EUC}, \code{MAN}, \code{CAN}, \code{CHE} or  \code{OCT}.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Distance value applying weights.
#' @examples
#'
#' cluster1 <- matrix(c(2,3))
#' cluster2 <- matrix(c(4,5))
#'
#' weight1 <- c(0.6,0.4)
#' weight2 <- c(2,4)
#'
#' distances(cluster1, cluster2, 'MAN', weight1)
#'
#' distances(cluster1, cluster2, 'CHE', weight2)
#'
#' @export


distances <-
  function(cluster1,cluster2,distance,weight){
    clusterDist <- 0
    if(distance == 'EUC'){
      dist <- edistanceW(cluster1,cluster2,weight)
    } else if (distance == 'MAN'){
      dist <- mdistanceW(cluster1,cluster2,weight)
    } else if (distance == 'CAN'){
      dist <- canberradistanceW(cluster1,cluster2,weight)
    } else if (distance == 'CHE'){
      dist <- chebyshevDistanceW(cluster1,cluster2,weight)
    } else if (distance == 'OCT'){
      dist <- octileDistanceW(cluster1,cluster2,weight)
    }
    dist
  }
