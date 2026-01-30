#' Prints object of type trafo
#' 
#' @param x an object of type \code{trafo}.
#' @param ... other parameters that can be passed to the function.
#' @export

print.trafo <- function(x, ...){

 
  cat(x$family, "Transformation \n")
  if (inherits(x, "oneparam")) {
    cat("\n")
    cat("Estimation method: ", x$method, " \n")
    cat("Optimal parameter: ", x$lambdahat, " \n")
    if (x$method == "ml" | x$method == "reml") {
      cat("Loglike: ",x$measoptim,"\n") 
    } else if (x$method == "skew" | x$method == "pskew" ) {
      cat("Skewness: ",x$measoptim,"\n")
    } else if (x$method == "div.ks" | x$method == "div.cvm" | 
               x$method == "div.kl") {
      cat("Divergence: ", x$measoptim,"\n")
    }
  }
  cat("\n")
  cat("Summary of transformed variables \n")
  print(summary(as.numeric(x$yt)))

   
 invisible(x)
}



#' Data frame with transformed variables
#' 
#' The data frame that is returned contains the variables that are used
#' in the model and additionally a variable with the transformed dependent
#' variable. To the variable name of the dependent variable a t is added for
#' transformed.
#' 
#' @param x an object of type \code{trafo}.
#' @param row.names	NULL or a character vector giving the row names for the 
#' data frame. Missing values are not allowed.
#' @param optional	logical. If TRUE, setting row names and converting column 
#' names (to syntactic names: see make.names) is optional. Note that all of R's 
#' base package as.data.frame() methods use optional only for column names 
#' treatment, basically with the meaning of 
#' data.frame(*, check.names = !optional)
#' @param std logical. If \code{TRUE}, the data is transformed by the 
#' standardized/scaled transformation. Defaults to \code{FALSE}.
#' @param ... other parameters that can be passed to the function.
#' @return A data frame with the original variables and the transformed variable.
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
#' # Transform dependent variable using divergence minimization following
#' # Kolmogorov-Smirnov
#' logshiftopt_trafo <- logshiftopt(object = lm_cars, method = "div.ks", 
#' plotit = FALSE)
#' 
#' # Get a data frame with the added transformed variable
#' as.data.frame(logshiftopt_trafo)
#' @export

as.data.frame.trafo <- function(x, row.names = NULL, optional = FALSE, 
                                std = FALSE, ...) {
  
  formula <- NULL
  
  if (inherits(x$object, "lm")) {
    data <- x$object$model 
    transformed_dependent <- paste0(as.character(formula(x$object$terms)[2]), "t")
    
    if (std == TRUE) {
      data[, transformed_dependent] <- x$zt
    
    } else if (std == FALSE) {
      data[, transformed_dependent] <- x$yt
    }
  
    data <- as.data.frame(data, row.names = row.names, optional = optional, ...)
  } else if (inherits(x$object, "lme")) {
   data <- x$object$data
   transformed_dependent <- paste0(as.character(formula(x$object$terms)[2]), "t")
   data[, transformed_dependent] <- as.numeric(x$yt)
   
   data <- as.data.frame(data, row.names = row.names, optional = optional, ...)
  }
  return(data)
}
