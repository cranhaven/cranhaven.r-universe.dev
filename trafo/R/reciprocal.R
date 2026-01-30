#' Reciprocal transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Reciprocal transformation. 
#'
#' @param object an object of type lm. 
#' @return An object of class \code{trafo}. Methods such as 
#' \code{\link{as.data.frame.trafo}} and \code{\link{print.trafo}} can 
#' be used for this class.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable 
#' reciprocal(object = lm_cars)
#' @export

reciprocal <- function(object) {
  
  trafo <- "reciprocal"
  woparam(object = object, trafo = trafo)
}