#' Function for estimating initial parameter values  'flexsurvreg'
#'
#' A function which performs the Bayesian MCMC estimation procedure for
#' estimating the efficacy parameter (\eqn{\beta}) using personalised sunthetic
#' controls methodology.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC_clean a cleaned dataset ontained using dataComb().
#' @param nsim the number of MCMC simulations to run
#' @param start the stating value for
#' @param start.se the stating value fo
#' @param trt an optional vector denoting treatment allocations where multiple
#'     treatment comparisons are being made
#' @details An MCMC routine for fitting a psc model
#' @return a matrix containing the draws form the posterior distribution
#' @import utils
#'
pscEst.glm <- function(CFM,DC_clean,nsim,start,start.se,trt=trt){

  cov_co <- DC_clean$model_extract$cov_co;cov_co
  sig <- DC_clean$model_extract$sig;sig
  est <- c(cov_co)
  trt.con <- is.null(trt)

  ####### Bayesian Estimation
  beta <- start
  parm <- matrix(NA,nsim,length(est)+length(beta)+1)
  parm[1,]<- c(est,beta,NA);parm[1,]

  ### multiplier for target distribution
  mult <- (2*start.se);mult

  ## Progress Bar
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)

  for(n in 2:nsim){

  ## progress bar
  setTxtProgressBar(pb, n)

  ### Drawing Samples
  cand <- rmvnorm(1,est,sig);cand
  cand.beta <- rnorm(length(beta),start,mult);cand.beta #Check this bit
  parm[n,] <- c(cand,cand.beta,NA)

  ### partitioning covariates into baseline hazard and coefficients
  DC_cand <- DC_clean
  DC_cand$model_extract$cov_co <- as.numeric(cand)

  ### cadidate log Hazard Ratio
  beta.old <- parm[n-1,-c(1:length(cand),ncol(parm))];beta.old
  beta.new <- parm[n,-c(1:length(cand),ncol(parm))];beta.new

  ### Prior contribution
  pr.cand <- -dmvnorm(cand,est,sig,log=T)
  pr.old <- dmvnorm(beta.old,rep(0,length(beta)),diag(length(beta))*1000,log=T)
  pr.new <- dmvnorm(beta.new,rep(0,length(beta)),diag(length(beta))*1000,log=T)

  parm[1:3,]
  if(trt.con){
    ### Likelihood evaluation
    l.old <- lik.glm(beta.old,DC_cand) + pr.old + pr.cand
    l.new <- lik.glm(beta.new,DC_cand)  + pr.new + pr.cand
  }

  if(!trt.con){
    l.old <- lik.glm.mtc(beta.old,DC_cand) + pr.old + pr.cand;l.old
    l.new <- lik.glm.mtc(beta.new,DC_cand)  + pr.new + pr.cand;l.new
  }


  ### Accept/Reject
  parm[n,ncol(parm)] <- l.new
  if(!acc(l.old,l.new)) {
    parm[n,-c(1:length(cand),ncol(parm))] <- beta.old
    parm[n,ncol(parm)] <- l.old
  }

}

  parm

}



