#' Summarizing Logistic PCA models
#' 
#' The function summary.lpca gives a summary from an object from lpca()
#'
#' @param object An object resulting from lpca
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from lpca
#'
#' @export
summary.lpca = function(object,...){

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  if(is.null(object$X)) cat("Fitted logistic PCA model", "\n")
  else cat("Fitted logistic reduced rank model", "\n")
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
