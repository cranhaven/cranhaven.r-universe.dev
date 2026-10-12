#' @title Matrix distance by distance and approach type.
#' @description To explain how to calculate the matrix distance by using \code{distance} and \code{approach} type.
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
#' @return A matrix distance. Explanation.
#' @examples
#'
#' data <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' clusters <- toList(data)
#'
#' mdAgglomerative.details(clusters, 'EUC', 'MAX')
#'
#' mdAgglomerative.details(clusters, 'CHE', 'AVG')
#'
#' @export

mdAgglomerative.details <- function(list,distance,approach){
  message("\n 'mdAgglomerative' creates the matrix distance of every cluster given by 'list' parameter.\n")
  message("\n It uses", distance, "distance and", approach, "approach.\n")
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
  solution <- matrix(res, nrow=length(list))
  message("\n It returns the matrix with all the distances between clusters depending on distance and approach. \n")
  message("\n The matrix distance is: \n\n")
  print(solution)
}
