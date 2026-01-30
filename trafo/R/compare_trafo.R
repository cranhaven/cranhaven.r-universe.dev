#' Compares linear models with transformed dependent variable
#'
#' Function \code{trafo_compare} compares linear models where the dependent 
#' variable is transformed by different transformations. 
#'
#' @param object an object of type lm
#' @param trafos a list of two \code{trafo} objects based on the same model 
#' given in object.
#' @param std logical. If \code{TRUE}, the transformed models are returned 
#' based on the standardized/scaled transformation. Defaults to \code{FALSE}.
#' @return An object of class \code{trafo_compare}. Methods such as 
#' \code{\link{diagnostics.trafo_compare}}, \code{\link{print.trafo_compare}},
#' \code{\link{plot.trafo_compare}} and \code{\link{summary.trafo_compare}} can 
#' be used for this class.  
#' @seealso \code{\link{bickeldoksum}}, \code{\link{boxcox}}, \code{\link{dual}}, 
#' \code{\link{glog}}, \code{\link{gpower}}, \code{\link{log}}, 
#' \code{\link{logshiftopt}}, \code{\link{manly}}, \code{\link{modulus}}, 
#' \code{\link{neglog}}, \code{\link{sqrtshift}}, \code{\link{yeojohnson}}   
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform with Bickel-Doksum transformation
#' bd_trafo <- bickeldoksum(object = lm_cars, plotit = FALSE)
#' 
#' # Transform with Box-Cox transformation
#' bc_trafo <- boxcox(object = lm_cars, method = "skew", plotit = FALSE)
#' 
#' # Compare transformed models
#' trafo_compare(object = lm_cars, trafos = list(bd_trafo, bc_trafo))
#' @export

trafo_compare <- function(object, trafos, std = FALSE) {
  
  check_compare_trafo(object = object, trafos = trafos, std = std)
  
  trafoOne <- trafos[[1]]
  trafoTwo <- trafos[[2]]
  if (inherits(trafoOne, "woparam")) {
    paramOne <- "woparam"
  } else if (inherits(trafoOne, "oneparam")) {
    paramOne <- "oneparam"
  }
  if (inherits(trafoTwo, "woparam")) {
    paramTwo <- "woparam"
  } else if (inherits(trafoTwo, "oneparam")) {
    paramTwo <- "oneparam"
  }
  
  trafoOne_mod <- get_modelt(object = object, trans_mod = trafoOne, std = std)
  trafoTwo_mod <- get_modelt(object = object, trans_mod = trafoTwo, std = std)
  
  compare_out <- list(trafoOne = trafoOne_mod,
                      trafoTwo = trafoTwo_mod,
                      trafos = c(trafoOne$family, trafoTwo$family), 
                      method = c(trafoOne$method, trafoTwo$method), 
                      lambdahat = c(trafoOne$lambdahat, 
                                    trafoTwo$lambdahat), 
                      std = std, 
                      param = c(paramOne, paramTwo))
  
  class(compare_out) <- "trafo_compare"
  
  return(compare_out)
  
}