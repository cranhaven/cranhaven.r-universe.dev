#' Calculates the Log Likelihood for a new sample given the current state (i.e. log[L(G|x)P(G)])
#' 
#' Algorithm implemented according to Engelhardt et al. 2017.
#' The function can be replaced by an user defined version if necessary.
#'  
#' @param pars                 sampled hidden influence for state k (w_new) at time tn+1
#' @param Step                 time step of the sample algorithm corresponding to the given vector of time points 
#' @param OBSERVATIONS         observed values at the given time step/point
#' @param parameters           model parameters estimates
#' @param INPUT                discrete input function e.g. stimuli
#' @param x_0                  initial values at the given time step/point
#' @param EPS_inner            current hidden inputs at time tn
#' @param D                    diagonal weight matrix of the current Gibbs step
#' @param GIBBS_PAR            GIBBS_PAR[["BETA"]] and GIBBS_PAR[["ALPHA"]]; prespecified or calculated vector of state weights
#' @param k                    number state corresponding to the given hidden influence (w_new)
#' @param MU_JUMP              mean of the normal distributed proposal distribution
#' @param SIGMA_JUMP           variance of the normal distributed proposal distribution
#' @param eps_new              current sample vector of the hidden influences (including all states)
#' @param objectivfunc,        link function to match observations with modeled states


#' @return                     returns the log-likelihood for two given hidden inputs 
LOGLIKELIHOOD_func  <- function(pars,Step,OBSERVATIONS,x_0,parameters,EPS_inner,INPUT,D,GIBBS_PAR,k,MU_JUMP,SIGMA_JUMP,eps_new,objectivfunc){
  

  EPS_inner<-rbind(EPS_inner,eps_new)
  EPS_PARS <- EPS_inner
  
  EPS_PARS[2,k] <- pars

  
  RATIO_partial_new <- PARTIALLIKELIHOOD_func(Step,OBSERVATIONS,x_0,parameters,INPUT,EPS_PARS,GIBBS_PAR[["BETA"]],GIBBS_PAR[["ALPHA"]],objectivfunc) 
  
  if (!is.na(RATIO_partial_new)){
  
  r                 <- RATIO_partial_new+mvtnorm::dmvnorm(EPS_PARS[2,],EPS_inner[1,],D,log=TRUE)
  return(r)
  } 
  
  return(NA)
  
}

#partial
PARTIALLIKELIHOOD_func <- function(STEP,OBSERVATIONS,x_0,parameters,input,W,BETA,ALPHA,objective){

  TIME <-  c(OBSERVATIONS[STEP-1,1],OBSERVATIONS[STEP,1])

  X <- ode_solv(TIME,x_0,parameters,input,W)

  if(any(is.na(X))) return(NA)
# SUM <-  sum(-log((1+(1/(2*BETA))*(((OBSERVATIONS[2,-1]-sapply(1:length(OBSERVATIONS[2,-1]),objective(tail(X,1),parameters),y=tail(X,1),parameter=parameters,USE.NAMES = TRUE))^2)))^(ALPHA+0.5)))
 SUM <-  sum(-log((1+(1/(2*BETA))*(((OBSERVATIONS[2,-1]-objective(tail(X,1),parameters))^2)))^(ALPHA+0.5)))
 
  return(SUM)
  
  
}
