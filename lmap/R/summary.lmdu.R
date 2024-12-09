#' Summarizing Logistic MDU models
#'
#' The function summary.lmdu gives a summary from an object from lmdu()
#'
#' @param object An object resulting from lmdu
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from lmdu
#'
#' @export

summary.lmdu = function(object,...){

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  if(is.null(object$X)) cat("Fitted logistic MDU model", "\n")
  else cat("Fitted logistic restricted MDU model", "\n")
  cat("Residual deviance:", object$deviance, "\n")
  cat("Number of fitted parameters:", object$npar, "\n" )
  cat("AIC:", object$AIC, "\n" )
  cat("BIC:", object$BIC, "\n" )
  cat("\n")
  if(!is.null(object$X)){
    cat("B coefficients:", "\n")
    print(round(object$B, digits = 2))
    cat("\n")
  }
  cat("V coefficients:", "\n")
  print(round(object$V, digits = 2))
  cat("\n")

}
