#' Log transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Log transformation. The Log transformation is only defined for positive 
#' response values. In case the response contains zero or negative values
#' a shift is automatically added such that y + shift > 0. 
#'
#' @param object an object of type lm. 
#' @return An object of class \code{trafo}. Methods such as 
#' \code{\link{as.data.frame.trafo}} and \code{\link{print.trafo}} can 
#' be used for this class.
#' @references 
#' Box GEP, Cox DR (1964). An Analysis of Transformations. Journal of the Royal 
#' Statistical Society B, 26(2), 211-252.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable 
#' logtrafo(object = lm_cars)
#' @export

logtrafo <- function(object) {
  
  trafo <- "log"
  trafo <- check_negy(object = object, trafo = trafo)
  woparam(object = object, trafo = trafo)
}