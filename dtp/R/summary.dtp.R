#' Summary of a nardl model
#'
#' \code{summary} method for a \code{\link{dtp}} model.
#'
#' @param object is the object of the function
#' @param ... not used
#'
#' @return an object of the S3 class \code{summary.dtp} with the
#' following components:
#'
#' @importFrom stats printCoefmat
#' @rdname summary.dtp
#' @name summary.dtp
#' @export
summary.dtp<-function(object,...){

  res0<-cbind(object$alpha,object$std,object$lw,object$up,object$tval,object$pval)
  rownames(res0)<-c("initial",object$nmes)
  colnames(res0)<-c("Estimate", "Std.Err", "Lower" , "Upper",  "Z value", "Pr(>z)")
  res1<-cbind(object$alpha1,object$std1,object$lw1,object$up1,object$tval1,object$pval1)
  rownames(res1)<-c("beta1","gamma")
  colnames(res1)<-c("Estimate", "Std.Err", "Lower" , "Upper", "Z value", "Pr(>z)")
  res2<-cbind(object$alpha2,object$std2,object$lw2,object$up2,object$tval2,object$pval2)
  rownames(res2)<-"beta2"
  colnames(res2)<-c("Estimate", "Std.Err", "Lower" , "Upper",  "Z value", "Pr(>z)")

  cat ("\n")
  cat ("==============================================================\n")
  cat ("Threshold Estimate                        ", exp(object$qhat), "\n")
  cat ("Confidence Interval - Uncorrected         ", exp(object$qcfi_0), "\n")
  cat ("Confidence Interval - Het Corrected Quad  ", exp(object$qcfi_1), "\n")
  cat ("Confidence Interval - Het Corrected NP    ", exp(object$qcfi_2), "\n")
  cat ("==============================================================\n")
  cat ("Regime-independent regressors: \n")
  cat ("--------------------------------------------------------------\n")
  printCoefmat(res0,has.Pvalue = TRUE,signif.stars = TRUE)
  cat ("==============================================================\n")
  cat ("Regime 1 : Threshold variable less than   ", exp(object$qhat), "\n")
  cat ("Number of observations                    ", sum(object$da),"\n")
  cat ("--------------------------------------------------------------\n")
  printCoefmat(res1,has.Pvalue = TRUE,signif.stars = TRUE)
  cat ("==============================================================\n")
  cat ("Regime 2 : Threshold variable greater than", exp(object$qhat), "\n")
  cat ("Number of observations                    ", sum(object$db),"\n")
  cat ("--------------------------------------------------------------\n")
  printCoefmat(res2,has.Pvalue = TRUE,signif.stars = TRUE)
  cat ("==============================================================\n")
  cat ("Lineraity test              \n")
  cat ("--------------------------------------------------------------\n")
  cat("Wald Tests (LM):", object$lm,"P-value:",object$lmpv, "\n")
  cat ("--------------------------------------------------------------\n")
  cat("Fisher Tests (F):", object$fm,"P-value:",object$fmpv, "\n")
  cat ("--------------------------------------------------------------\n")
  cat("LRT Tests (LM):", object$lgrt,"P-value:",object$lgrtpv, "\n")
  cat ("==============================================================\n")
}
