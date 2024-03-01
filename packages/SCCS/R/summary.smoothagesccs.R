summary.smoothagesccs <- function(object, conf.int = 0.95) {
  
  fit <- object
  beta <- fit$coef
  se <- fit$se
  rval <- list()
  tmp <- cbind(as.matrix(fit$coef), as.matrix(exp(fit$coef)), as.matrix(fit$se), as.matrix((fit$coef)/fit$se), as.matrix(1 - pchisq((fit$coef/fit$se)^2, 1)))
  
  #tmp <- round(tmp, 4)
  dimnames(tmp) <- list(names(fit$coef), c("coef", "exp(coef)",
                                            "se(coef)", "z", "Pr(>|z|)"))
  #printCoefmat(tmp, signif.stars=TRUE, P.values=TRUE, has.Pvalue=TRUE)
  #ri<-exp(fit$coef)
  #lo<-exp(fit$coef - 1.96*ses)
  #hi<-exp(as.vector(coef(mod)) + 1.96*ses)
  z <- qnorm((1 + conf.int)/2, 0, 1)
  tmp1 <- cbind(as.matrix(exp(beta)), as.matrix(exp(-beta)), as.matrix(exp(beta - z * se)), as.matrix(exp(beta + z * se)))
  dimnames(tmp1) <- list(names(beta), c("exp(coef)", "exp(-coef)",
                                             paste("lower .", round(100 * conf.int, 2), sep = ""),
                                             paste("upper .", round(100 * conf.int, 2), sep = "")))
  rval$coefficients <- tmp
  rval$conf.int <- tmp1
 
  #rval$n <- paste0(1, " cases")
  rval$nevent <- NULL 
  rval$smp <- fit$smp
  rval$smoothingpara <- format(fit$smoothingpara,  scientific = T,digits=2)
  rval$cv <- fit$cv
  #if (is.R()) class(rval) <- "summary.coxph"
  #else        oldClass(rval) <- "summary.coxph"
  
  if (is.R()) class(rval) <- "summary.smoothagesccs"
  else        oldClass(rval) <- "summary.smoothagesccs"
  
  rval
  
}
