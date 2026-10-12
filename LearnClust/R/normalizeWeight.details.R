#' @title To normalize weight values.
#' @description To explain how to normalize weight values if \code{normalize} = TRUE.
#' @param normalize is a boolean value.
#' @param weight is a numeric vector.
#' @param data is a data.frame.
#' @details This function allows users to normalize weights.
#' @details If there is not any weight, the function will create a numeric vector of "1".
#' @details If normalize = TRUE, the function will make every weight value as a "[0:1]" value.
#' @details If normalize = FALSE, the function will not make any changes, weights will be the same.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Numeric vector with updated weights. Explanation.
#' @examples
#'
#' data <- data.frame(matrix(c(1:10),ncol = 2))
#'
#' weight1 <- c(0.6,0.4)
#' weight2 <- c(2,4)
#'
#' normalizeWeight.details(FALSE, weight1, data)
#'
#' normalizeWeight.details(TRUE, weight2, data)
#'
#' normalizeWeight.details(FALSE, weight2, data)
#'
#' @export


normalizeWeight.details <- function(normalize,weight,data){
  message("\n This function normalizes weight values. \n")
  if(!is.null(weight)){
    message("\n This are the initial weights: \n")
    for (i in c(1:length(weight))) {
      message("  ", weight[i])
    }
  }
  message("\n It checks if there is a weights vector. If not, it creates a vector with ", ncol(data), " '1's.  \n")
  if(is.null(weight)){
    weight <- c()
    value <- 1
    for (j in c(1:ncol(data))) {
      weight <- c(weight, value)
    }
  }

  res <- c()
  if(normalize){
    message("\n Due to the fact that 'normalize' = TRUE, weight vector changes every weight as the \n initial value divided between the total sum of the vector. \n")
    message("\n FinalWeight[i] = WeightValue[i]/TotalSum  \n")
    total <- sum(weight)
    for (index in c(1:length(weight))) {
      currentValue <- weight[index]
      normalizedValue <- currentValue/total
      res  <- c(res, normalizedValue)
    }

  } else {
    message("\n Due to the fact that 'normalize' = FALSE, weight vector does not change. \n")
    res <- weight
  }
  message("\n These are the new weights: \n")
  for (i in c(1:length(res))) {
    message("  ", res[[i]])
  }
  res
}
