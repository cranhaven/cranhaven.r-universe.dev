#' Summarizing an Extended Steretype Model
#' 
#' The function summary.esm gives a summary from an object from esm()
#'
#' @param object An object resulting from esm
#' @param \dots additional arguments to be passed.
#'
#' @return Summary of the results obtained from esm
#'
#' @export
summary.esm = function(object,...){
  Q = ncol(object$A)
  coef.matrix = rbind(object$bm[1:Q],object$A)
  rownames(coef.matrix)[1] = "1"

  resids = object$bm[-(1:Q)]
  #names(resids) = rownames(object$bm)[-(1:Q)]

  cat("\n")
  cat("Call:", "\n")
  print(object$call)
  cat("\n")
  cat("Fitted extended stereotype model", "\n")
  cat("Residual deviance:", object$deviance, "\n")
  cat("Number of fitted parameters", object$df, "\n" )
  cat("AIC", object$AIC, "\n" )
  cat("\n")
  cat("Coefficients:", "\n")
  print(round(coef.matrix, digits = 2))
  cat("\n")
  if(length(object$bm) > (Q)){
    cat("Residual Associations:", "\n")
    print(round(resids, digits = 2))
  }
}
