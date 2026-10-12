#' @title Minimal distance
#' @description Get the matrix minimal value.
#' @param matrix is a numeric matrix. It could be a numeric vector.
#' @details This function is part of the hierarchical clusterization method. The function uses the numeric
#' vector or matrix \code{matrix} given and return the minimal value.
#' The function avoids distances equal 0, and initialize minimum value with an auxiliar function \code{initMin},
#' which gets the first matrix element with a valid distance.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Numeric value. Min value from a matrix.
#' @examples
#'
#' matrixExample <- matrix(c(1:10), nrow=2)
#'
#' minDistance(1:10)
#'
#' minDistance(matrixExample)
#'
#' @export



minDistance <-
function(matrix){
    min <- initMin(matrix)
    for (i in (1:length(matrix))){
        value <- matrix[i]
        if ((value < min) & (value != 0)){
            min <- value
        }
    }
    min
}

initMin <- function(matrix){
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
