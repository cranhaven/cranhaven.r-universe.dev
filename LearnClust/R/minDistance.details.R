#' @title Minimal distance
#' @description To explain how to get the matrix minimal value.
#' @param matrix is a numeric matrix. It could be a numeric vector.
#' @details This function is part of the hierarchical clusterization method. The function uses the numeric
#' vector or matrix \code{matrix} given and return the minimal value.
#' The function avoids distances equal 0, and initialize minimum value with an auxiliar function \code{initMin},
#' which gets the first matrix element with a valid distance.
#'
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Numeric value. Min value from a matrix. Explanation.
#' @examples
#'
#' matrixExample <- matrix(c(1:10), nrow=2)
#'
#' minDistance(1:10)
#'
#' minDistance.details(matrixExample)
#'
#' @export

minDistance.details <-
  function(matrix){
    message("\n 'minDistance' function gets the minimal value from a matrix. \n")
    message("\n It returns the minimal value avoiding 0 values. \n\n")
    min <- initMin(matrix)
    for (i in (1:length(matrix))){
      value <- matrix[i]
      if ((value < min) & (value != 0)){
        min <- value
      }
    }
    print(matrix)
    message("\n\n ",min, " is the minimal value of the matrix. \n")
    min
  }
