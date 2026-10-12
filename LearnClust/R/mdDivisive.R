#' @title Matrix distance by distance and approach type.
#' @description To calculate the matrix distance by using \code{distance} and \code{approach} types.
#' @param list is a clusters list.
#' @param distance is a string. The distance type to be used.
#' @param approach is a string. The approach type to be used.
#' @param components is a clusters list. It contains every clusters with only one element. It is used to check if complementary condition is 'TRUE'.
#' @details This function is part of the divisive hierarchical clusterization method. The function calculates the
#' matrix distance by using the distance and approach types given.
#' @details The \code{list} parameter will be a list with the clusters as rows and columns.
#' @details The function avoids distances equal 0 and undefined clusters.
#' @details It also avoids distances between clusters that are not complementary because they can't be chosen to divide all the clusters.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Matrix distance.
#' @examples
#'
#' data <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' clusters <- toList(data)
#'
#' components <- toList(data)
#'
#' mdDivisive(clusters, 'EUC', 'MAX', components)
#'
#' mdDivisive(clusters, 'MAN', 'MIN', components)
#'
#' @export


mdDivisive <- function(list,distance,approach,components){
  res <- c()
  for (i in seq_len(length(list))){
    for (j in seq_len(length(list))){
      if(is.null(list[[i]]) | is.null(list[[j]])){
        dist <- 0
      } else if(i ==j){
        dist <- 0
      } else if (!complementaryClusters(components,list[[i]],list[[j]])){
        dist <- 0
      }
      else {
        dist <- clusterDistance(list[[i]],list[[j]],approach,distance)
      }
      res <- c(res, dist)
    }
  }
  #return value
  ret <- matrix(res, nrow=length(list))
}
