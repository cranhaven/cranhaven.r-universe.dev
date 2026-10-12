#' @title To explain how to transform data into list
#' @description To explain how to transform \code{data} into list.
#' @param data could be a numeric vector, a matrix or a numeric data frame.
#' @details This function is part of the divisive hierarchical clusterization method. The function initializes
#' \code{data} content as a list.
#'
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A list with clusters. Explanation.
#' @examples
#'
#' data <- c(1:10)
#'
#' matrix <- matrix(data,ncol=2)
#'
#' dataFrame <- data.frame(matrix)
#'
#' toListDivisive.details(data)
#'
#' toListDivisive.details(matrix)
#'
#' toListDivisive.details(dataFrame)
#'
#' @export

toListDivisive.details <- function(data){
  message("  'toListDivisive' creates a list initializing datas by creating clusters with each one \n")
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
    cluster <- matrix(c(value1,value2),ncol=2)
    list[[i]] <- cluster
  }
  list
}
