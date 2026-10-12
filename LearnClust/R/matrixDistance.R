#' @title Matrix distance by distance type
#' @description To calculate the matrix distance by using \code{distance} type.
#' @param list is a clusters list.
#' @param distance is a literal.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' matrix distance by using the distance type given.
#' @details The \code{list} parameter will be a list with the clusters as rows and columns.
#' @details The function avoids distances equal 0 and undefined clusters.
#' @examples
#'
#' data <- c(1:10)
#'
#' clusters <- toList(data)
#'
#' matrixDistance(clusters, 'EUC')
#' @export


matrixDistance <- function(list,distance){
  res <- c()
  for (i in seq_len(length(list))){
    for (j in seq_len(length(list))){
      if(is.null(list[[i]]) | is.null(list[[j]])){
        dist <- 0
      } else if(i ==j){
        dist <- 0
      }
      else {
        dist <- clusterDistance(list[[i]],list[[j]],'MAX',distance)
      }
      res <- c(res, dist)
    }
  }
  ret <- matrix(res, nrow=length(list))
}
