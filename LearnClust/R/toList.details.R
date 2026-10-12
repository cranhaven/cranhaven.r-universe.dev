#' @title To explain how to transform data into list
#' @description To explain how to transform \code{data} into list.
#' @param data could be a numeric vector, a matrix or a numeric data frame.
#' @details This function is part of the agglomerative hierarchical clusterization method. The function initializes
#' \code{data} content as a list.
#' @details In agglomerative algorithm, it adds a \code{TRUE} flag to each element, which indicates that the cluster is not grouped.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A list with cñlusters. Explanation.
#' @examples
#'
#' data <- c(1:10)
#'
#' matrix <- matrix(data,ncol=2)
#'
#' dataFrame <- data.frame(matrix)
#'
#' toList(data)
#'
#' toList(matrix)
#'
#' toList(dataFrame)
#'
#' @export

toList.details <- function(data){
  message("  'toList' creates a list initializing datas by creating clusters with each one \n")
  if (is.data.frame(data)){
    # print('dataframe')
    v <- dataFrameToVector(data)
  } else if (is.matrix(data)){
     # print('matrix')
    v <- matrixToVector(data)
  } else {
    # print('vector')
    v <- data
  }
  list <- c()
  for (i in (1:(length(v)/2))) {
    value1 <- v[2*i -1]
    value2 <- v[2*i]
    cluster <- matrix(c(value1,value2,TRUE),ncol=3)
    list[[i]] <- cluster
  }
  list
}
