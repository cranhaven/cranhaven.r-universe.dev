#' @title To transform data into list
#' @description To transform \code{data} into list.
#' @param data could be a numeric vector, a matrix or a numeric data frame.
#' @details This function is part of the divisive hierarchical clusterization method. The function initializes
#' \code{data} content as a list.
#'
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return a list with clusters.
#' @examples
#'
#' data <- c(1:10)
#'
#' matrix <- matrix(data,ncol=2)
#'
#' dataFrame <- data.frame(matrix)
#'
#' toListDivisive(data)
#'
#' toListDivisive(matrix)
#'
#' toListDivisive(dataFrame)
#'
#' @export




toListDivisive <- function(data){
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


dataFrameToVector <- function(dataframe){
  names(dataframe) <- c("col1", "col2")
  var1 <- as.vector(dataframe$col1)
  var2 <- as.vector(dataframe$col2)
  vector <- c()
  for (index in seq_len(length(var1))) {
    vector <- c(vector, as.integer(var1[index]), as.integer(var2[index]))
  }
  vector
}

matrixToVector <- function(matrix){
  var1 <- as.vector(matrix[,1])
  var2 <- as.vector(matrix[,2])
  vector <- c()
  for (index in seq_len(length(var1))) {
    vector <- c(vector, var1[index], var2[index])
  }
  vector
}
