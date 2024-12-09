#'Summarizing Cumulative Logistic MDU models
#'										 
#' The function summary.lmdu gives a summary from an object from clmdu()
#'
#' @param object An object resulting from clmdu
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from clmdu
#'
#' @export

summary.clmdu = function(object,...){

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  if(is.null(object$X)) cat("Fitted cumulative logistic MDU model", "\n")
  else cat("Fitted cumulative logistic restricted MDU model", "\n")
  cat("Residual deviance:", object$deviance, "\n")
  cat("Number of fitted parameters:", object$npar, "\n" )
  cat("AIC:", object$AIC, "\n" )
  cat("BIC:", object$BIC, "\n" )
  cat("\n")
  if(!is.null(object$X)){
    cat("B coefficients:", "\n")
    rownames(object$B) = object$xnames
    print(round(object$B, digits = 2))
    cat("\n")
  }
  cat("V coefficients:", "\n")
  rownames(object$V) = object$ynames
  print(round(object$V, digits = 2))
  cat("\n")

}
