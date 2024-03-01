summary.penmodel <- function(object, correlation=FALSE, ...){
   
  ans <- object[c("estimates")]
  class(ans) <- "summary.penmodel"
  tval <-  object$estimates/object$se
  if(attr(object, "robust"))   tval <-  object$estimates/object$se.robust
  pval <- 2 * pt(abs(tval), 1, lower.tail = FALSE)
  
  if(attr(object, "robust")) {
    ans$estimates <- cbind(object$estimates, object$se, object$se.robust, tval, pval)
    dimnames(ans$estimates) <- list(names(object$estimates), c("Estimate", "Std. Error", "Robust SE", "t value", "Pr(>|t|)"))
    ans$varcov <- object$varcov
    ans$varcov.robust <- object$varcov.robust
  }
  else {
    ans$estimates <- cbind(object$estimates, object$se, tval, pval)
    dimnames(ans$estimates) <- list(names(object$estimates), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$varcov <- object$varcov
    ans$varcov.robust <- NULL
  }
  
  if(correlation) {
  	ans$correlation <- object$varcov/object$se^2
  	ans$correlation.robust <- object$varcov.robust/object$se.robust^2
  	}
  else{
  	ans$correlation <- NULL
  	ans$correlation.robust <- NULL
  	}
  attr(ans, "robust") <- attr(object,"robust")
  return(ans) 

}
