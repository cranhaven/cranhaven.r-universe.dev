#' @title Maximal distance
#' @description To explain how to get the matrix maximal value.
#' @param matrix is a numeric matrix. It could be a numeric vector.
#' @details This function is part of the hierarchical clusterization method. The function uses the numeric
#' vector or matrix \code{matrix} given and return the maximal value.
#' The function avoids distances equal 0, and initialize maximal value with an auxiliar function \code{initMax},
#' which gets the first matrix element with a valid distance.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Numeric value. Max value from a matrix. Explanation.
#' @examples
#'
#' matrixExample <- matrix(c(1:10), nrow=2)
#'
#' maxDistance.details(1:10)
#'
#' maxDistance.details(matrixExample)
#'
#' @export

maxDistance.details <-
  function(matrix){
    message("\n 'maxDistance' function gets the maximal value from a matrix. \n")
    message("\n It returns the maximal value avoiding 0 values. \n\n")
    max <- initMax(matrix)
    for (i in (1:length(matrix))){
      value <- matrix[i]
      if ((value > max) & (value != 0)){
        max <- value
      }
    }
    print(matrix)
    message("\n\n ",max, " is the maximal value of the matrix. \n")
    max
  }
