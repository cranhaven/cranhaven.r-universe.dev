penetrance <- function(fit, fixed, age, CI=TRUE, MC=100){
 
  if(CI & MC==0) stop("The numbers of Monte-Carlo samples to use for constructing CIs should be specified.")
  if(!CI) MC <- 0
  agemin <- attr(fit, "agemin")
  base.dist <- attr(fit, "base.dist")
  frailty.dist <- attr(fit, "frailty.dist")
  nbase <- attr(fit, "nbase")
  cuts0 <- attr(fit, "cuts") - agemin
  
  k <- ifelse(is.null(frailty.dist) || frailty.dist=="none", 0, 1)
  nvar <- length(fit$estimates)-nbase-k
  if(length(fixed)!= nvar) stop("The size of fixed is incorrect.")
  if(min(age) < agemin) stop(paste("minimum age should be greater than", agemin))
  
  est <- penf(fit$estimates, x=fixed, age=age, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, cuts=cuts0)  
  
  if(CI){
    sest <- mvrnorm(n=MC, fit$estimates, fit$varcov) 
    i <- 0
    pen <- matrix(0, ncol=length(age), nrow=MC)
    for(a in age){
      i <- i + 1
      pen[, i] <- apply(sest, 1, penf, age=a, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, x=fixed, cuts=cuts0)
    }
    se = apply(pen, 2, function(x) sd(x, na.rm=TRUE))
    out <- data.frame(age=age, penentrance=est, CI=t(apply(pen, 2, quantile, prob=c(0.025, 0.975))), se=se)
    colnames(out)[3:4]<-c("lower","upper")
    rownames(out) <- 1:dim(out)[1]
  }
  else out <- data.frame(age=age, penetrance=est)
  if(is.null(frailty.dist)) names(fixed) <- names(fit$estimates)[-c(1:nbase)]
  else names(fixed) <- names(fit$estimates)[-c(1:(nbase+1))]
  
  cat("Fixed covariate values: ")
  cat(paste(names(fit$estimates)[-c(1:nbase)], fixed, sep=" = "))
  cat("\n")
  return(out)    
}

