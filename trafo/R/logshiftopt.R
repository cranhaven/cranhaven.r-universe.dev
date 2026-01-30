#' Log shift opt transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Log shift opt transformation. The transformation parameter can either be 
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
#' Defaults to \code{NULL}. In this case the lambdarange is set to the range 
#' of the data. In case the lowest value is negative the absolute value of the
#' lowest value plus 1 is the lower bound for the range. 
#' @param plotit logical. If \code{TRUE}, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' Defaults to \code{TRUE}.
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
#' # Transform dependent variable using divergence minimization following
#' # Kolmogorov-Smirnof
#' logshiftopt(object = lm_cars, method = "div.ks", plotit = FALSE)
#' @export

logshiftopt <- function(object, lambda ="estim", method = "ml", 
                      lambdarange = NULL, plotit = TRUE) {
  
  trafo <- "logshiftopt"
  if (is.null(lambdarange)) {
    span <- range(object$model[, paste0(formula(object)[2])])
    if ((span[1] + 1) <= 1) {
      lower = abs(span[1]) + 1
    } else {
      lower <- -span[1] + 1
    }
    upper <- diff(span)
    
    lambdarange <- c(lower,upper)
    cat(paste0("The default lambdarange for the Log shift opt transformation is calculated dependent on the data range. The lower value is set to ", lambdarange[1], " and the upper value to ", lambdarange[2], "\n"))
    cat("\n")
  }
  oneparam(object = object, trafo = trafo, lambda = lambda, method = method, 
           lambdarange = lambdarange, plotit = plotit)
}