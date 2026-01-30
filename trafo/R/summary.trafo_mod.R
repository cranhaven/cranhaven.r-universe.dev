#' Summary for linear models with untransformed and transformed dependent 
#' variable
#'
#' The summary method for class \code{trafo_lm} contains a summary for an 
#' untransformed and a transformed model. The resulting summary is based on the 
#' summary for objects of type \code{lm}. 
#' 
#' @param object an object of type \code{trafo_lm}
#' @param ... additional arguments that are not used in this method
#' @return An object of class \code{summary.trafo_lm}. The method 
#' \code{\link{print.summary.trafo_lm}} can be used for this class.
#' @export

summary.trafo_lm <- function(object, ...) {
  
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

  
  sums <- summary_internal(modOne = modOne, modTwo = modTwo, 
                           compare = FALSE, std = object$std)
  
  sum_out <- list(trafo = trafo, 
                  method = method, 
                  lambdahat = lambdahat, 
                  orig_sum = sums$modOne_sum, 
                  trafo_sum = sums$modTwo_sum, 
                  param = param, 
                  std = object$std)

  class(sum_out) <- "summary.trafo_lm"
  
  return(sum_out)
}



#' Print summary trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_lm}
#' @param x an object of type \code{summary.trafo_lm}
#' @param ... additional arguments that are not used in this method
#' @export

print.summary.trafo_lm <- function(x, ...) {
  # 
  # 
  # cat("Applied transformation \n")
  # cat("Transformation: ",x$trafo," \n")
  # if (x$param == "oneparam") {
  #   cat("Estimation method: ", x$method, " \n")
  #   cat("Optimal Parameter: ", x$lambdahat, " \n")
  # }
  # cat("\n")
  # cat("Residual diagnostics:\n")
  # 
  # cat("\n")
  # cat("Normality:\n")
  # cat("Pearson residuals:\n")
  # print(x$norm_resid)
  # if (!is.null(x$norm_ranef)) {
  #   cat("Standardized random effects:\n")
  #   print(x$norm_ranef) 
  # }
  # cat("\n")
  # cat("Heteroscedasticity:\n")
  # print(x$hetero)
  # cat("\n")
  cat("Summary of untransformed model \n")
  print(x$orig_sum)
  cat("\n")
  cat("Summary of transformed model: ", x$trafo,"transformation \n")
  cat("\n")
  cat("Formula in call: ",x$trafo_sum$formula, "\n")
  print(x$trafo_sum)
  cat("\n")
  if (x$std == TRUE) {
    #cat("Note that the standard errors are missing due to the lack of methods 
    #  for correct standard errors in transformed models using standardized
    #    transformation. \n")
  }
  
  invisible(x)
}




