#' Box-Cox transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Box-Cox transformation. The transformation parameter can either be 
#' estimated using different estimation methods or given. The Box-Cox 
#' transformation is only defined for positive response values. In case the 
#' response contains zero or negative values a shift is automatically added such 
#' that y + shift > 0. 
#'
#' @param object an object of type \code{lm}. 
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
#' Defaults to \code{c(-2, 2)}.
#' @param plotit logical. If \code{TRUE}, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' Defaults to \code{TRUE}.
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
#' # Transform dependent variable using skewness minimization
#' boxcox(object = lm_cars, method = "skew", plotit = FALSE)
#' @export

boxcox <- function(object, lambda ="estim", method = "ml", 
                      lambdarange = c(-2, 2), plotit = TRUE) {
  
  # if(any(object$model[, paste0(formula(object)[2])] <= 0)) {
  #   trafo <- "boxcox"
  # } else {
  #   trafo <- "boxcoxshift"
  #   cat(paste0(formula(object)[2]), " contains zero or negative values. Thus, 
  #       a shift is included such that y + shift > 0.")
  # }
  
  trafo <- "boxcox"
  trafo <- check_negy(object = object, trafo = trafo)

  oneparam(object = object, trafo = trafo, lambda = lambda, method = method, 
           lambdarange = lambdarange, plotit = plotit)
  
}