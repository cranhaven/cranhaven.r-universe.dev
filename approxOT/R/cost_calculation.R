#' Calculate cost matrix
#'
#' @param X matrix of values in first sample. Observations should be by column, not rows.
#' @param Y matrix of Values in second sample. Observations should be by column, not rows.
#' @param ground_p power of the Lp norm to use in cost calculation.
#'
#' @return matrix of costs
#' @export
#'
#' @examples
#' X <- matrix(rnorm(10*100), 10, 100)
#' Y <- matrix(rnorm(10*100), 10, 100)
#' # the Euclidean distance
#' cost <- cost_calc(X, Y, ground_p = 2)
cost_calc <- function(X, Y, ground_p){
  
  if (!is.double(ground_p) ) ground_p <- as.double(ground_p)
  if (nrow(X) != nrow(Y)) {
    stop("Rows of X and Y should be equal to have same dimension. Observations should be unique by column")
  }
  
  return( cost_calculation_(X, Y, ground_p) )
}