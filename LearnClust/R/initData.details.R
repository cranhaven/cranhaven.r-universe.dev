#' @title To initialize data, hierarchical correlation algorithm.
#' @description To explain how to initialize data, hierarchical correlation algorithm.
#' @param data is a data frame with the main data.
#' @details This function is part of the hierarchical correlation method. The function initializes \code{data} transforming each row from the data frame into
#' a matrix with every row elements.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A cluster list. Initializing data. Explanation.
#' @examples
#'
#' data <- matrix(c(1,2,1,4,5,1,8,2,9,6,3,5,8,5,4),ncol= 3)
#'
#' dataFrame <- data.frame(data)
#'
#' initData.details(dataFrame)
#'
#' @export


initData.details <- function(data){
  message("\n This function initializes the input data creating a cluster with each row of the data frame. \n")
  message("\n It gets this data from the user: \n")
  print(data)
  solution <- list()
  for (row in c(1:nrow(data))) {
    rowData <- data[row,]
    values <- c()
    for (column in c(1:ncol(data))) {
      values <- c(values, rowData[,column])
    }
    listData <- matrix(values, ncol = ncol(data))
    solution[[length(solution) + 1]] <- listData
  }
  message("\n Each cluster will be a matrix with a row and the same columns as the initial data frame. \n")
  message("\n Initialized data will be:  \n")
  print(solution)
  solution
}
