summary.sccs <- function(mod, sandwich, ses, ncases, nevents) {
  
  rval <- list()
  tmp <- cbind(coef(mod), exp(coef(mod)), ses, coef(mod)/ses, 1 - pchisq((coef(mod)/ ses)^2, 1))
  
  #tmp <- round(tmp, 4)
  dimnames(tmp) <- list(names(coef(mod)), c("coef", "exp(coef)",
                                            "se(coef)", "z", "Pr(>|z|)"))
  #printCoefmat(tmp, signif.stars=TRUE, P.values=TRUE, has.Pvalue=TRUE)
  ri<-exp(as.vector(coef(mod)))
  lo<-exp(as.vector(coef(mod)) - 1.96*ses)
  hi<-exp(as.vector(coef(mod)) + 1.96*ses)
  
  tmp1 <- cbind(ri, exp(-coef(mod)), lo, hi)
  dimnames(tmp1) <- list(names(coef(mod)), c("exp(coef)", "exp(-coef)",
                                             paste("lower .", 95, sep = ""),
                                             paste("upper .", 95, sep = "")))
  rval$coefficients <- tmp
  rval$conf.int <- tmp1
  rval$VarCov <- sandwich
  #rval$rsq <- 1
  #rval$logtest <- 0
  #rval$waldtest <- 0
  # rval$sctest <- 0
  # rval$used.robust <- 0
  rval$n <- paste0(ncases, " cases")
  rval$nevent <- nevents 
  #if (is.R()) class(rval) <- "summary.coxph"
  #else        oldClass(rval) <- "summary.coxph"
  
  if (is.R()) class(rval) <- "summary.sccs"
  else        oldClass(rval) <- "summary.sccs"
  
  rval
  
}
