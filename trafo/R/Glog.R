#' Glog transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Glog transformation. 
#'
#' @param object an object of type lm. 
#' @return An object of class \code{trafo}. Methods such as 
#' \code{\link{as.data.frame.trafo}} and \code{\link{print.trafo}} can 
#' be used for this class.    
#' @references 
#' Durbin BP, Hardin JS, Hawkins DM, Rocke DM (2002). A Variance-stabilizing 
#' Transformation for Gene-expression Microarray Data. Bioinformatics, 18, 
#' 105-110.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable 
#' glog(object = lm_cars)
#' @export

glog <- function(object) {
  
  trafo <- "glog"
  woparam(object = object, trafo = trafo)
}