penf_c <- function(event, base.est, vbeta, kappa=NULL, x, age, base.dist="Weibull", frailty.dist=NULL, agemin=20, cuts=NULL){

  xbeta1 <- sum(vbeta[[1]]*x[[1]])
  xbeta2 <- sum(vbeta[[2]]*x[[2]])
  xbeta <- c(xbeta1, xbeta2)
  cuts0 <- cuts - agemin
  if(is.null(frailty.dist) | frailty.dist=="none"){
    pen <- sapply(age, function(x) integrate(integ_nofrailty, lower = 0, upper = x-agemin, event = event,
                                             base.dist=base.dist, base.est=base.est, xbeta=xbeta, cuts=cuts0,
                                             kappa=kappa)$value)
  }
  else{    
    if(frailty.dist %in% c("gamma", "lognormal")){
      pen <- sapply(age, function(x) integrate(integ_frailty, lower = 0, upper = x-agemin, event = event,
                       base.dist=base.dist, base.est=base.est, frailty.dist=frailty.dist, xbeta=xbeta, cuts=cuts0,
                       kappa=kappa)$value)
    }
    else if(frailty.dist=="cgamma"){
     
      pen <- sapply(age, function(x) integrate(integ_cgamma, lower = 0, upper = x-agemin, event = event,
                                               base.dist=base.dist, base.est=base.est, xbeta=xbeta, cuts=cuts0,
                                               kappa=kappa)$value)

    }
    else if(frailty.dist=="clognormal"){
      k0 <- kappa[3]; k1 <- kappa[1]; k2 <- kappa[2]
      sigma <- matrix(c(k1, sqrt(k1*k2)*k0, sqrt(k1*k2)*k0, k2), nrow=2)
      gh <- mgauss.hermite(6, mu=c(0,0), sigma=sigma, prune=0) #6(n=32); 5(n=21)
      
      pen <- sapply(age, function(x) integrate(integ_clognormal, lower = 0, upper = x-agemin, event = event,
                                               base.dist=base.dist, base.est=base.est, xbeta=xbeta, cuts=cuts0,
                                               sigma=sigma, gh=gh)$value)
    }
  }

  return(pen)
}

