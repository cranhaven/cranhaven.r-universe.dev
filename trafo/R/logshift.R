#' Log shift transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Log shift transformation. The shift parameter is determined by y + shift > 0. 
#'
#' @param object an object of type lm. 
#' @return an object of class \code{trafo}.
# #' @examples
# #' # Load data
# #' data("cars", package = "datasets")
# #' 
# #' # Fit linear model
# #' lm_cars <- lm(dist ~ speed, data = cars)
# #' 
# #' # Transform dependent variable 
# #' logshift(object = lm_cars)
#' @keywords internal

logshift <- function(object) {
  
  trafo <- "logshift"
  woparam(object = object, trafo = trafo)
}