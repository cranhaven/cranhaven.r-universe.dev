#' @title To initialize target, hierarchical correlation algorithm.
#' @description To initialize target, hierarchical correlation algorithm. It checks if target is valid, if not, it initializes the target
#' @param target is a numeric vector, a matrix or a data frame.
#' @param data is a data frame with the main data.
#' @details This function is part of the hierarchical correlation method. The function initializes \code{target} and checks if it is a valid target.
#' @details The function transforms the target into a matrix. Then, it checks if the target has only one row and the same columns has the main data.
#' @details If it is not a valid target, the function will notice the problem and will initialized a new target with every column with value 0.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A cluster.
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
#' initTarget(target1,dataFrame)
#'
#' initTarget(target2,dataFrame)
#'
#' @export

initTarget <- function(target,data){
  if(is.data.frame(target)){
    target <- initData(target)[[1]]
  } else if (is.matrix(target)){
    target <- target
  } else if (is.vector(target)){
    target <- matrix(target, ncol = length(target))
  }
  if(nrow(target) == 1 & ncol(target) == ncol(data)){
    target <- target
  } else {
    message('\n invalid Target! \n\n')
    target <- matrix(rep(0,ncol(data)),ncol = ncol(data))
  }
  target
}
