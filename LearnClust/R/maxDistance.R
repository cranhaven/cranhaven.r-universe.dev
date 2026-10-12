#' @title Maximal distance
#' @description Get the matrix maximal value.
#' @param matrix is a numeric matrix. It could be a numeric vector.
#' @details This function is part of the hierarchical clusterization method. The function uses the numeric
#' vector or matrix \code{matrix} given and return the maximal value.
#' The function avoids distances equal 0, and initialize maximal value with an auxiliar function \code{initMax},
#' which gets the first matrix element with a valid distance.
#'
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return numeric value. Max value from a matrix
#' @examples
#'
#' matrixExample <- matrix(c(1:10), nrow=2)
#'
#' maxDistance(1:10)
#'
#' maxDistance(matrixExample)
#'
#' @export


maxDistance <-
  function(matrix){
    max <- initMax(matrix)
    for (i in (1:length(matrix))){
      value <- matrix[i]
      if ((value > max) & (value != 0)){
        max <- value
      }
    }
    max
  }

initMax <- function(matrix){
  #print(matrix)
  value <- 0
  found <- FALSE
  index <- 1
  while (!found && index < length(matrix)){
    value <- matrix[index]
    if(value > 0){
      found <- TRUE
    } else {
      index <- index + 1
    }
  }
  value
}
