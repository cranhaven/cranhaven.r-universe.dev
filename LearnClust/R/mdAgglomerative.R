#' @title Matrix distance by distance and approach type.
#' @description To calculate the matrix distance by using \code{distance} and \code{approach} type.
#' @param list is a clusters list.
#' @param distance is a literal. The distance type to be used.
#' @param approach is a literal. The approach type to be used.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' matrix distance by using the distance and approach type given.
#' @details The \code{list} parameter will be a list with the clusters as rows and columns.
#' @details The function avoids distances equal 0 and undefined clusters.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A matrix distance.
#' @examples
#'
#' data <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' clusters <- toList(data)
#'
#' mdAgglomerative(clusters, 'EUC', 'MAX')
#'
#' mdAgglomerative(clusters, 'CHE', 'AVG')
#'
#' @export


mdAgglomerative <- function(list,distance,approach){
  res <- c()
  for (i in seq_len(length(list))){
    for (j in seq_len(length(list))){
      if(is.null(list[[i]]) | is.null(list[[j]])){
        dist <- 0
      } else if(i ==j){
        dist <- 0
      }
      else {
        dist <- clusterDistance(list[[i]],list[[j]],approach,distance)
      }
      res <- c(res, dist)
    }
  }
  ret <- matrix(res, nrow=length(list))
}

