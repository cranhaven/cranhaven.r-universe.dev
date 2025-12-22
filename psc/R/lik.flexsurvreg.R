#' Likelihood function for a psc model of class 'flexsurvreg'
#'
#' A function which defines the likelihood for a PSC model where the Counter
#' Factual Model (CFM) takes the form of a 'flexsurvreg' object and an efficacy
#' parameter (\eqn{\beta}) is being estimated.  For more details on fitting please see
#' ?pscfit and ?pscEst
#'
#' @param beta a parameter to be estimate
#' @param pscOb A pscOb object containing a cleaned dataset including covariates to match the CFM
#' @details A likelihood function for use by pscfit for a model of class
#' 'flexsurvreg'
#' @importFrom survival Surv
#' @return the results of a likelihood functions
lik.flexsurvreg <- function(beta,pscOb){

  co <- pscOb$co
  haz_co <- co[names(co)%in%names(pscOb$haz_co)]
  cov_co <- co[names(co)%in%names(pscOb$cov_co)]

  lam <-   pscOb$lam
  kn <- pscOb$kn
  k <- pscOb$k

  time <- pscOb$DC$Y$time;time
  cen <- pscOb$DC$Y$cen
  cov <- pscOb$DC$X

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

  ### Specifying treatment for MTC
  if(!is.null(pscOb$DC$trt)){
    trt <- pscOb$DC$trt
    trt <- factor(trt)
    lev <- levels(trt)

    ### Error trap - beta supplied must match number of treatment levels
    if(length(beta)!=length(lev)){ stop(paste("beta does not match numebr of
           levels for mulitple treatment effects. No. levels for beta =",
                                              length(beta)," to correspond to", lev))
    }

    ### Linear predictor of the treatment effect
    beta <- model.matrix(~-1+trt)%*%beta
  }

  ##### Specifying
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
