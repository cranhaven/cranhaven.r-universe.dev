#' Diagnostics for an untransformed and a transformed model
#'
#' Returns information about the applied transformation and selected diagnostics 
#' to check model assumptions. The return helps to compare the untransformed and 
#' the transformed model with regard to model assumptions.
#' 
#' @param object an object of type \code{trafo_lm}
#' @param ... additional arguments that are not used in this method
#' @return An object of class \code{diagnostics.trafo_lm}. The method 
#' \code{\link{print.diagnostics.trafo_lm}} can be used for this class. 
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Compare transformed models
#' BD_lm <- trafo_lm(object = lm_cars, trafo = "bickeldoksum", 
#' method = "skew", lambdarange = c(1e-11, 2))
#' 
#' # Get diagnostics
#' diagnostics(BD_lm)
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export

diagnostics.trafo_lm <- function(object, ...) {
  
  formula <- NULL
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  if (inherits(object, "woparam")) {
  param <- "woparam"
  } else if (inherits(object, "oneparam")) {
    param <- "oneparam"
  }
  modOne <- object$orig_mod
  modOne$name <- "Untransformed model"
  modTwo <- object$trafo_mod
  modTwo$name <- "Transformed model"
  
  diagnose <- diagnostics_internal(modOne = modOne, modTwo = modTwo)
  
  diagnose_out <- list(trafo = trafo, 
                       method = method, 
                       lambdahat = lambdahat, 
                       param = param, 
                       std = object$std,
                       norm_resid = diagnose$norm_resid, 
                       norm_ranef = diagnose$norm_ranef, 
                       hetero = diagnose$hetero)

  class(diagnose_out) <- "diagnostics.trafo_lm"
  
  return(diagnose_out)
}



#' Prints diagnostics of an untransformed and a transformed model
#'
#' Prints diagnostics of an untransformed and a transformed model.
#' 
#' @param x an object of type \code{diagnostics.trafo_lm}
#' @param ... additional arguments that are not used in this method
#' @export

print.diagnostics.trafo_lm <- function(x, ...) {
  # 
  cat("Diagnostics: Untransformed vs transformed model \n")
  cat("\n")
  cat("Transformation: ",x$trafo," \n")
  if (x$param == "oneparam") {
     cat("Estimation method: ", x$method, " \n")
     cat("Optimal Parameter: ", x$lambdahat, " \n")
   }
  cat("\n")
  cat("Residual diagnostics:\n")
  cat("\n")
  cat("Normality:\n")
  cat("Pearson residuals:\n")
  print(x$norm_resid)
  if (!is.null(x$norm_ranef)) {
     cat("Standardized random effects:\n")
     print(x$norm_ranef) 
  }
  if (!is.null(x$hetero)) {
    cat("\n")
    cat("Heteroscedasticity:\n")
    print(x$hetero)
  }
  invisible(x)
}




