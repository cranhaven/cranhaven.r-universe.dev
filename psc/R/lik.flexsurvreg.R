#' Likelihood function for a psc model of class 'flexsurvreg'
#'
#' A function which defines the likelihood for a PSC model where the Counter
#' Factual Model (CFM) takes the form of a 'flexsurvreg' object and an efficacy
#' parameter (\eqn{\beta}) is being estimated.  For more details on fitting please see
#' ?pscfit and ?pscEst
#'
#' @param beta a parameter to be estimate
#' @param DC_clean a cleaned dataset including covariates to match the CFM
#' @details A likelihood function for use by pscfit for a model of class
#' 'flexsurvreg'
#'
lik.flexsurvreg <- function(beta,DC_clean){
  lam <- DC_clean$model_extract$lam
  kn <- DC_clean$model_extract$kn
  k <- DC_clean$model_extract$k
  haz_co <- DC_clean$model_extract$haz_co
  cov_co <- DC_clean$model_extract$cov_co

  time <- DC_clean$out$time;time
  cen <- DC_clean$out$cen
  cov <- DC_clean$cov

  logt <- log(time)
  lp <- cov%*%cov_co

  z <- NULL
  z_h <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)

    zt_h <- (modp(logt-kn[(i+1)])^2 - lam[(i+1)]*modp(logt-kn[1])^2 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^2)
    z_h <- cbind(z_h,zt_h)

  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  h0 <- (H0/time)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)])

  H<- H0*exp(lp+beta)
  h<- h0*exp(lp+beta)
  S <- exp(-H)
  f <- S*h

  #l <- sum(cen*log(f+1e-16) + (1-cen)*log(S+1e-16))
  #-l

  ll <- rowSums(cbind(cen*log(f+1e-16),(1-cen)*log(S+1e-16)),na.rm=T)
  -sum(ll)

}
