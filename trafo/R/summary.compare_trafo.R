#' Summary for two differently transformed models
#'
#' The summary contains the summary for two transformed models. The summary is 
#' based on the summary for objects of type \code{lm}. 
#' 
#' @param object an object of type \code{trafo_compare}
#' @param ... additional arguments that are not used in this method
#' @return An object of class \code{summary.trafo_compare}. The method 
#' \code{\link{print.summary.trafo_compare}} can be used for this class.
#' @export

summary.trafo_compare <- function(object, ...) {
  
  formula <- NULL
  
  trafos <- object$trafos
  method <- object$method
  lambdahat <- object$lambdahat
  param <- object$param
  
  modOne <- object$trafoOne
  modOne$name <- trafos[[1]]
  modTwo <- object$trafoTwo
  modTwo$name <- trafos[[2]]
  
  sums <- summary_internal(modOne = modOne, modTwo = modTwo, 
                           compare = TRUE, std = object$std)
  
  
  sum_out <- list(trafo = trafos, 
                  method = method, 
                  lambdahat = lambdahat, 
                  trafoOne_sum  = sums$modOne_sum, 
                  trafoTwo_sum  = sums$modTwo_sum,
                  std = object$std)
  
  class(sum_out) <- "summary.trafo_compare"
  
  return(sum_out)
}



#' Prints summary of trafo_compare objects
#'
#' Prints objects to be shown in the summary function for objects of 
#' type \code{trafo_compare}.
#' 
#' @param x an object of type \code{summary.trafo_compare}
#' @param ... additional arguments that are not used in this method
#' @export

print.summary.trafo_compare <- function(x, ...) {
  
  cat("Summary of model with ", x$trafo[[1]], "\n")
  print(x$trafoOne_sum)
  cat("\n")
  cat("Summary of model with ", x$trafo[[2]], "\n")
  print(x$trafoTwo_sum)
  cat("\n")
  if (x$std == TRUE) {
    #cat("Note that the standard errors can be biased. \n")
  }
  invisible(x)
}




