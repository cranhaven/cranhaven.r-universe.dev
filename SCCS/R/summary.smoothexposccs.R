summary.smoothexposccs <- function(object, conf.int = 0.95) {
  
  fit <- object
  beta <- fit$coef
  se <- fit$se_age
  rval <- list()
  tmp <- cbind(fit$coef, exp(fit$coef), se, (fit$coef)/se, 1 - pchisq((fit$coef/se)^2, 1))
  
  #tmp <- round(tmp, 4)
  dimnames(tmp) <- list(names(fit$coef), c("coef", "exp(coef)",
                                           "se(coef)", "z", "Pr(>|z|)"))
  #printCoefmat(tmp, signif.stars=TRUE, P.values=TRUE, has.Pvalue=TRUE)
  #ri<-exp(fit$coef)
  #lo<-exp(fit$coef - 1.96*ses)
  #hi<-exp(as.vector(coef(mod)) + 1.96*ses)
  z <- qnorm((1 + conf.int)/2, 0, 1)
  tmp1 <- cbind(exp(beta), exp(-beta), exp(beta - z * se), exp(beta + z * se))
  dimnames(tmp1) <- list(names(beta), c("exp(coef)", "exp(-coef)",
                                        paste("lower .", round(100 * conf.int, 2), sep = ""),
                                        paste("upper .", round(100 * conf.int, 2), sep = "")))
  rval$coefficients <- tmp
  rval$conf.int <- tmp1
  
  #rval$n <- paste0(1, " cases")
  rval$nevent <- NULL 
  rval$smp <- format(fit$smoothingpara,  scientific = T,digits=2)
  rval$crossvalidation <- round(fit$cv, 2)
  #if (is.R()) class(rval) <- "summary.coxph"
  #else        oldClass(rval) <- "summary.coxph"
  
  if (is.R()) class(rval) <- "summarysm.smoothexposccs"
  else        oldClass(rval) <- "summarysm.smoothexposccs"
  
  rval
  
}
