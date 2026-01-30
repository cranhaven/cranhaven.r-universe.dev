#' Fits transformed linear models
#'
#' Function \code{trafo_lm} fits linear models with transformed dependent 
#' variable. The main return are two \code{lm} objects where one is the 
#' untransformed linear model and the other one the transformed linear model. 
#'
#' @param object an object of type \code{lm}. 
#' @param trafo a character string. Different transformations can be used 
#' for transforming the dependent variable in a linear model: 
#' (i)  "bickeldoksum", (ii) "boxcox", (iii) "dual", 
#' (iv) "glog", (v) "gpower", (vi) "log", (vii) "logshiftopt", (viii) "manly", 
#' (ix) "modulus", (x) "neglog", (xi) "reciprocal", (xii) "yeojohnson".
#' Defaults to "boxcox".
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
#' Defaults to \code{NULL} which means that the default value of the chosen 
#' transformation is used.
#' @param std logical. If \code{TRUE}, the transformed model is returned based 
#' on the standardized/scaled transformation. Defaults to \code{FALSE}.
#' @param custom_trafo a list. The list has two elements where the first element 
#' is a function specifying the desired transformation and the second element is 
#' a function specifying the corresponding standardized transformation. 
#' Defaults to \code{NULL}.
#' @return An object of class \code{trafo_lm}. Methods such as 
#' \code{\link{diagnostics.trafo_lm}}, \code{\link{print.trafo_lm}},
#' \code{\link{plot.trafo_lm}} and \code{\link{summary.trafo_lm}} can 
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
#' # Compare untransformed and transformed model
#' trafo_lm(object = lm_cars, trafo = "bickeldoksum", method = "skew", 
#' lambdarange = c(1e-11, 2))
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export

trafo_lm <- function(object, trafo = "boxcox", lambda = "estim", method = "ml", 
                     lambdarange = NULL, std = FALSE, 
                     custom_trafo = NULL) {
 
  check_trafomod_lm(object = object, trafo = trafo, std = std, 
                    custom_trafo = custom_trafo)
  
  plotit <- FALSE
  
  trafo <- check_negy(object = object, trafo = trafo)

  
  if (trafo %in% c("bickeldoksum", "boxcoxshift", "boxcox", "dual", "gpower", 
                   "manly", "modulus", "logshiftopt", "sqrtshift", 
                   "yeojohnson")) {
    trans_mod <- oneparam(object = object, trafo = trafo, lambda = lambda, 
                          method = method, lambdarange = lambdarange, 
                          plotit = plotit)
  } else if (trafo %in% c("log", "logshift", "reciprocal", "neglog", "glog")) {
    trans_mod <- woparam(object = object, trafo = trafo,
                         custom_trafo = custom_trafo)
  } else if (trafo == "custom_one") {
    trafo <- "custom"
    trans_mod <- oneparam(object = object, trafo = trafo, lambda = lambda, 
                          method = method, lambdarange = lambdarange, 
                          plotit = plotit, custom_trafo = custom_trafo)
  } else if (trafo == "custom_wo") {
    trafo <- "custom"
    trans_mod <- woparam(object = object, trafo = trafo,
                         custom_trafo = custom_trafo)
  }
  
  

  # Get original lm object
  orig_mod <- object 
  
  
  # Get transformed lm object
  trafo_mod <- get_modelt(object = object, trans_mod = trans_mod, std = std)

  
  # Return new class
  trafo_out <- list(orig_mod = orig_mod,
                    trafo_mod = trafo_mod, 
                    trafo = trafo, 
                    method = method, 
                    lambdahat = trans_mod$lambdahat, 
                    std = std)
  
  if (inherits(trans_mod, "woparam")) {
    class(trafo_out) <- c("trafo_lm", "woparam")
  } else if (inherits(trans_mod, "oneparam")) {
    class(trafo_out) <- c("trafo_lm", "oneparam")
  }
  
  return(trafo_out)
}


