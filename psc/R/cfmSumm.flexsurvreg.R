#' Counter Factual Model - summary
#'
#' A generic function to provide a summary of a Counter factual model of class
#' 'glm'
#'
#' @param pscOb an object of class 'psc'
#' @param bootCI a boolean to determine if bootstrapping CIs are required
#' @param nboot Number of bootstraps
#' @importFrom mvtnorm rmvnorm
#' @importFrom survival Surv
#' @return A summary of a cfm object
cfmSumm.flexsurvreg <-   function(pscOb,bootCI=TRUE,nboot=1000){

  s.est <- s_ci <-  NULL

  ### Set-Up
  lam <- pscOb$lam
  kn <- pscOb$kn
  k <- pscOb$k
  haz_co <- pscOb$haz_co
  cov_co <- pscOb$cov_co

  ### Data structures
  Y <- pscOb$DC$Y
  X <- pscOb$DC$X
  maxTime <- max(Y[,1],na.rm=T);maxTime

  ### Estimating survival function for population
  s.est <-  spline_surv_est(lam=lam,kn=kn,k=k,haz_co=haz_co,
                            cov_co=cov_co,cov=X,tm=Y$time,beta=0)

  ### Sampling from CFM parameters to get estimate of CI
  if(bootCI){
    vc <- pscOb$sig
    mu <- c(pscOb$haz_co,pscOb$cov_co)
    rest <- mvtnorm::rmvnorm(nboot,mu,vc)
    s_boot <- mapply(boot_sest,1:nboot,MoreArgs=list(pscOb=pscOb,lam=lam,kn=kn,k=k,cov=X
                                                     ,tm=Y$time,rest=rest,beta=0))
    s_ci <- t(mapply(function(i) quantile(s_boot[i,],p=c(0.025,0.975)),1:nrow(s_boot)))
    s.est <- cbind(s.est,s_ci)
    names(s.est)[4:5] <- c("lo","hi")
  }
  s.est
}
