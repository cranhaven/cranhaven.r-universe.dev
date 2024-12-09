#' Summarizing Cumulative Logistic PCA models
#' 
#' The function summary.clpca gives a summary from an object from clpca()
#'
#' @param object An object resulting from clpca
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from clpca
#'
#' @export

summary.clpca = function(object,...){

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  if(is.null(object$X)) cat("Fitted cumulative logistic PCA model", "\n")
  else cat("Fitted cumulative logistic reduced rank model", "\n")
  cat("Residual deviance:", object$deviance, "\n")
  cat("Number of fitted parameters:", object$npar, "\n" )
  cat("AIC:", object$AIC, "\n" )
  cat("BIC:", object$BIC, "\n" )
  cat("\n")
  if(!is.null(object$X)){
    rownames(object$B) = object$xnames
    cat("B coefficients:", "\n")
    print(round(object$B, digits = 2))
    cat("\n")
  }
  cat("V coefficients:", "\n")
  rownames(object$V) = object$ynames
  print(round(object$V, digits = 2))
  cat("\n")

}
