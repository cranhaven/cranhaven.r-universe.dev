#' Diagnostics for two differently transformed models
#'
#' Returns information about the applied transformations and selected 
#' diagnostics to check model assumptions. Two models are compared where
#' the dependent variable is transformed by different transformations.
#' 
#' @param object an object of type \code{trafo_compare}
#' @param ... additional arguments that are not used in this method
#' @return An object of class \code{diagnostics.trafo_compare}. The method 
#' \code{\link{print.diagnostics.trafo_compare}} can be used for this class.    
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
#' compare <- trafo_compare(object = lm_cars, trafos = list(bd_trafo, bc_trafo))
#' 
#' # Get diagnostics
#' diagnostics(compare)
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export

diagnostics.trafo_compare <- function(object, ...) {

  
  formula <- NULL
  
  trafos <- object$trafos
  method <- object$method
  lambdahat <- object$lambdahat
  param <- object$param
  
  modOne <- object$trafoOne
  modOne$name <- trafos[[1]]
  modTwo <- object$trafoTwo
  modTwo$name <- trafos[[2]]
  
  
  if (modOne$name == modTwo$name && method[[1]] != method[[2]]) {
    modOne$name <- paste0(modOne$name, "_", method[[1]])
    modTwo$name <- paste0(modTwo$name, "_", method[[2]])
  } else if (modOne$name == modTwo$name && method[[1]] != method[[2]]) {
    modOne$name <- paste0(modOne$name, "1")
    modTwo$name <- paste0(modOne$name, "2")
    
    warning("The same transformation and estimation method is used.")
  }
  
  
  diagnose <- diagnostics_internal(modOne = modOne, modTwo = modTwo)
  
  diagnose_out <- list(trafos = trafos, 
                       method = method, 
                       lambdahat = lambdahat, 
                       param = param, 
                       std = object$std,
                       norm_resid = diagnose$norm_resid, 
                       norm_ranef = diagnose$norm_ranef, 
                       hetero = diagnose$hetero)

  class(diagnose_out) <- "diagnostics.trafo_compare"
  
  return(diagnose_out)
}



#' Prints diagnostics of two trafo objects
#' 
#' Prints diagnostics of two trafo objects.
#' 
#' @param x an object of type \code{diagnostics.trafo_compare}
#' @param ... additional arguments that are not used in this method
#' @export

print.diagnostics.trafo_compare <- function(x, ...) {
  # 
  cat("Diagnostics of two transformed models \n")
  cat("\n")
  
  if(x$param[[1]] == "oneparam" && x$param[[2]] == "oneparam") {
    cat("Transformations: ",x$trafos[[1]], "and",x$trafos[[2]],"\n")
    cat("Estimation methods: ", x$method[[1]], "and", x$method[[2]], " \n")
    cat("Optimal Parameters: ", x$lambdahat[[1]], "and", x$lambdahat[[2]]," \n")
    cat("\n")
    } else if (x$param[[1]] == "oneparam" && x$param[[2]] == "woparam") {
      cat("Transformations: ",x$trafos[[1]], "and",x$trafos[[2]],"\n")
      cat("Estimation methods: ", x$method[[1]], " and no estimation \n")
      cat("Optimal Parameters: ", x$lambdahat[[1]]," and no parameter \n")
      cat("\n")
      } else if (x$param[[1]] == "woparam" && x$param[[2]] == "oneparam") {
        cat("Transformations: ",x$trafos[[1]], "and",x$trafos[[2]],"\n")
        cat("Estimation methods: No estimation and", x$method[[2]], " \n")
        cat("Optimal Parameters: No parameter and", x$lambdahat[[2]]," \n")
        cat("\n")
        } else if (x$param[[1]] == "woparam" && x$param[[2]] == "woparam") {
          cat("Transformations: ",x$trafos[[1]], "and",x$trafos[[2]],"\n")
          cat("Estimation methods: No estimation \n")
          cat("Optimal Parameters: No parameter  \n")
          cat("\n")
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




