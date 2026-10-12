#' @title To initialize target, hierarchical correlation algorithm.
#' @description To initialize target, hierarchical correlation algorithm. It checks if target is valid, if not, it initializes the target
#' @param target is a numeric vector, a matrix or a data frame.
#' @param data is a data frame with the main data.
#' @details This function is part of the hierarchical correlation method. The function initializes \code{target} and checks if it is an acceptable target.
#' @details The function transforms the target into a matrix. Then, it checks if the target has only one row and the same columns have the main data.
#' @details If it is not an acceptable target, the function will notice the problem and will initialize a new target with every column with value 0.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A cluster. Explanation.
#' @examples
#'
#' data <- matrix(c(1,2,1,4,5,1,8,2,9,6,3,5,8,5,4),ncol= 3)
#'
#' dataFrame <- data.frame(data)
#'
#' target1 <- matrix(c(2,3))
#'
#' target2 <- matrix(c(2,3,6))
#'
#' initTarget.details(target1,dataFrame)
#'
#' initTarget.details(target2,dataFrame)
#'
#' @export

initTarget.details <- function(target,data){
  message("\n This function initializes the target and checks if it is a valid target. \n")
  message("\n It gets this target from the user: \n")
  print(target)
  if(is.data.frame(target)){
    target <- initData(target)[[1]]
  } else if (is.matrix(target)){
    target <- target
  } else if (is.vector(target)){
    target <- matrix(target, ncol = length(target))
  }
  message("\n After transforming the target into matrix, it checks if it is acceptable\n")
  if(nrow(target) == 1 & ncol(target) == ncol(data)){
    message("\n If the target has the same columns as main data and only one row, it is a valid target! \n")
    target <- target
  } else {
    message("\n If the target does not have the same columns as main data or more than one row, \n it is not a valid target! \n")
    message("\n The function will initialize as a '0's matrix. \n")
    target <- matrix(rep(0,ncol(data)),ncol = ncol(data))
  }
  message("\n Target used will be:  \n")
  print(target)
  target
}
