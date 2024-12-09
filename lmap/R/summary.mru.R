#' Summarizing Multinomial Logistic Unfolding model
#' 											   
#' The function summary.mru gives a summary from an object from mru()
#'
#' @param object An object resulting from mru
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from mru
#'
#' @export
summary.mru = function(object,...){

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  cat("Fitted multinomial restricted unfolding model", "\n")
  cat("Residual deviance:", object$deviance, "\n")
  cat("Number of fitted parameters:", object$npar, "\n" )
  cat("AIC:", object$AIC, "\n" )
  cat("BIC:", object$BIC, "\n" )
  cat("\n")
  cat("B coefficients:", "\n")
  print(round(object$B, digits = 2))
  cat("\n")
  cat("V coefficients:", "\n")
  print(round(object$V, digits = 2))
  cat("\n")
}
