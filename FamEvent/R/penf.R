penf <- function(est, x, age, base.dist="Weibull", frailty.dist=NULL, agemin=20, cuts=NULL){
  nbase <- length(est)-length(x)
  base.est <- exp(est[1:nbase])
  if(base.dist=="lognormal") base.est[1] <- est[1] 
  if(is.null(frailty.dist) || frailty.dist=="none"){
    xbeta <- sum(est[-c(1:nbase)]*x)
    H <- cumhaz(base.dist, age-agemin, base.est, cuts=cuts)*exp(xbeta) 
    pen <- 1-exp(-H)
  }
  else{    
    k <- est[nbase+1]
    xbeta <- sum(est[-c(1:(nbase+1))]*x)
    H <- cumhaz(base.dist, age-agemin, base.est, cuts=cuts)*exp(xbeta) 
    pen <- 1-laplace(dist=frailty.dist, g=H, k=k)
  }





#  names(pen) <- age
  return(pen)
}

