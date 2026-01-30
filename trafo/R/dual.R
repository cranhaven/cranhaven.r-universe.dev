#' Dual transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Dual transformation. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given value 
#' for the transformation parameter. Defaults to "estim".
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),
#' (iii) Kurtosis optimization ("kurt"), (iv) Divergence minimization by 
#' Kolmogorov-Smirnov ("div.ks"), by Cramer-von-Mises ("div.cvm") or by 
#' Kullback-Leibler ("div.kl"). Defaults to "ml".
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter.
#' The Dual transformation is not defined for negative values of 
#' lambda. Defaults to \code{c(0, 2)}.
#' @param plotit logical. If \code{TRUE}, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' Defaults to \code{TRUE}.
#' @return An object of class \code{trafo}. Methods such as 
#' \code{\link{as.data.frame.trafo}} and \code{\link{print.trafo}} can 
#' be used for this class.    
#' @references
#' Yang Z (2006). A modified family of power transformations. Economics Letters, 
#' 92(1), 14-19.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable using divergence minimization following
#' # Cramer-von-Mises
#' dual(object = lm_cars, method = "div.cvm", plotit = TRUE)
#' @export

dual <- function(object, lambda = "estim", method = "ml", 
                    lambdarange = c(0, 2), plotit = TRUE) {
  
  trafo <- "dual"
  oneparam(object = object, trafo = trafo, lambda = lambda, method = method, 
           lambdarange = lambdarange, plotit = plotit)
  
}
