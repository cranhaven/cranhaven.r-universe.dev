penetrance_c <- function(fit, event=1, fixed, age, CI=TRUE, MC=100){
 
  if(CI & MC==0) stop("The numbers of Monte-Carlo samples to use for constructing CIs should be specified.")
  if(!CI) MC <- 0
  agemin <- attr(fit, "agemin")
  base.dist <- attr(fit, "base.dist")
  frailty.dist <- attr(fit, "frailty.dist")
  nbase <- attr(fit, "nbase")
  cuts <- attr(fit, "cuts") 
  X1 <- attr(fit, "X1")
  X2 <- attr(fit, "X2")
  if(!event %in% c(1,2)) stop("event should be either 1 or 2.")
  if(!is.list(fixed)) stop("fixed should be specified as a list of two vectors of covariate values for competing events.")
  
  nk <- ifelse(is.null(frailty.dist), 0, ifelse(frailty.dist%in%c("gamma","lognormal"), 2, 3))
  nb <- sum(nbase)
  nvar1 <- dim(X1)[2]
  nvar2 <- dim(X2)[2]

  if(length(fixed[[1]])!= nvar1) stop("The size of fixed[[1]] is incorrect.")
  if(length(fixed[[2]])!= nvar2) stop("The size of fixed[[2]] is incorrect.")
  if(min(age) < agemin) stop(paste("age should be greater than", agemin))
  
  est <- fit$estimates
  base.est1 <- exp(est[1:nbase[1]])
  base.est2 <- exp(est[(nbase[1]+1):nb])
  if(base.dist[1]=="lognormal") base.est1[1] <- log(base.est1[1]) 
  if(base.dist[2]=="lognormal") base.est2[1] <- log(base.est2[1])
  base.est <- list(base.est1, base.est2)
  vbeta1 <- est[(nb+1):(nb+nvar1)]
  vbeta2 <- est[(nb+nvar1+1):(nb+nvar1+nvar2)]
  vbeta <- list(vbeta1, vbeta2)
  kappa <- exp(est[(nb+nvar1+nvar2+1):length(est)])
  est <- penf_c(event=event, base.est=base.est, vbeta=vbeta, kappa=kappa, x=fixed, age=age, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, cuts=cuts)  

  if(CI){
    if(is.null(fit$varcov.robust)) sest <- mvrnorm(n=MC, fit$estimates, fit$varcov) 
    else sest <- mvrnorm(n=MC, fit$estimates, fit$varcov.robust) 
    pen <- matrix(NA, ncol=length(age), nrow=MC)
    for(i in 1:MC){
      iest <- sest[i,]
      ibase.est <- list(exp(iest[1:nbase[1]]), exp(iest[(nbase[1]+1):nb]))
      ivbeta <- list(iest[(nb+1):(nb+nvar1)], iest[(nb+nvar1+1):(nb+nvar1+nvar2)])
      ikappa <- exp(iest[(nb+nvar1+nvar2+1):length(iest)])
      ipen <- try(penf_c(event=event, age=age, base.est=ibase.est, vbeta=ivbeta, kappa=ikappa, x=fixed, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, cuts=cuts))
      #if (class(ipen) != "try-error") pen[i, ] <- ipen
      pen[i, ] <- ipen
    }
    se = apply(pen, 2, function(x) sd(x, na.rm=TRUE))
    out <- data.frame(age=age, penentrance=est, CI=t(apply(pen, 2, quantile, prob=c(0.025, 0.975), na.rm=TRUE)), se=se)
    colnames(out)[3:4]<-c("lower","upper")
    rownames(out) <- 1:dim(out)[1]
  }
  else out <- data.frame(age=age, penetrance=est)

  names(fixed[[1]]) <- names(fit$estimates)[(nb+1):(nb+nvar1)]
  names(fixed[[2]]) <- names(fit$estimates)[(nb+nvar1+1):(nb+nvar1+nvar2)]
  
  return(out)    
}

