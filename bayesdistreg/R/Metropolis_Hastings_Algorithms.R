#===============================================================================================#
#' Random Walk Metropolis-Hastings Algorithm
#'
#' \code{RWMH} computes random draws of parameters using a specified proposal distribution.
#' The default is the normal distribution
#'
#' @param data data required for the posterior distribution. First column is the outcome
#' @param propob a list of mean and variance-covariance of the normal proposal distribution
#' (default: NULL i.e. internally generated)
#' @param posterior the posterior distribution. It is set to null in order to use the logit posterior.
#' The user can specify log posterior as a function of parameters and data (pars,data)
#' @param iter number of random draws desired
#' @param burn burn-in period for the Random Walk MH algorithm
#' @param vscale a positive value to scale up or down the variance-covariance matrix in
#' the proposal distribution
#' @param start starting values of parameters for the MH algorithm.
#' It is automatically generated from the proposal distribution but the user can also specify.
#' @param prior the prior distribution (default: "Normal", alternative: "Uniform")
#' @param mu the mean of the normal prior distribution (default:0)
#' @param sig the variance of the normal prior distribution (default:10)
#' @return val a list of matrix of draws Matpram and the acceptance rate
#'
#' @examples
#' y = indicat(faithful$waiting,70)
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' data = data.frame(y,x); propob<- lapl_aprx(y,x)
#' RWMHob_n<- RWMH(data=data,propob,iter = 102, burn = 2) # prior="Normal"
#' RWMHob_u<- RWMH(data=data,propob,prior="Uniform",iter = 102, burn = 2)
#' par(mfrow=c(3,1));invisible(apply(RWMHob_n$Matpram,2,function(x)plot(density(x))))
#' invisible(apply(RWMHob_u$Matpram,2,function(x)plot(density(x))));par(mfrow=c(1,1))
#'
#' @export

RWMH<- function(data,propob=NULL,posterior=NULL,iter=1500,burn=500,vscale=1.5,
                start=NULL,prior="Normal",mu=0,sig=10){
  if(is.null(posterior)){
    logpost<- function(start,data) posterior(start,data,Log=TRUE,mu=mu,sig=sig,prior=prior)
  #define posterior distribution
  }
  if(is.null(propob)){
    propob = lapl_aprx(data[,1],data[,-1])
  }
  varprop = vscale*propob$var
  npar = length(propob$mode)
  Mat = array(0, c(iter, npar))
  if(is.null(start)){
    start = MASS::mvrnorm(n=1,propob$mode,varprop)
  }
  e = 0.000001 # specify step unif(-e,e) e = 0.000001
  Mat[1,] = start; AccptRate<-0
  for(i in 2:iter){
      start= Mat[i-1,]
      prop = MASS::mvrnorm(n=1,start,varprop) + stats::runif(1,-e,e)#make a draw from proposal dist
      lpa = logpost(prop,data); lpb = logpost(start,data)
      accprob = exp(lpa-lpb)
      # the other part cancels out because the normal distribution is symmetric
      if(stats::runif(1)< accprob){
        Mat[i,]=prop
        AccptRate<- AccptRate +1
      }else{
        Mat[i,]=start
      }
  }
  AcceptanceRate = AccptRate/iter
  val = list(Matpram=Mat[-c(1:burn),],AcceptanceRate=AcceptanceRate)
  cat("Random Walk MH algorithm successful. Acceptance ratio = ", AcceptanceRate," \n")
  return(val)
  }


#===============================================================================================#
#' Independence Metropolis-Hastings Algorithm
#'
#' \code{IndepMH} computes random draws of parameters using a specified proposal distribution.
#'
#' @param data data required for the posterior distribution
#' @param propob a list of mean and variance-covariance of the normal proposal distribution (default:NULL)
#' @param posterior the posterior distribution. It is set to null in order to use the logit posterior.
#' The user can specify log posterior as a function of parameters and data (pars,data)
#' @param iter number of random draws desired (default: 1500)
#' @param burn burn-in period for the MH algorithm (default: 500)
#' @param vscale a positive value to scale up or down the variance-covariance matrix in
#' the proposal distribution
#' @param start starting values of parameters for the MH algorithm.
#' It is automatically generated but the user can also specify.
#' @param prior the prior distribution (default: "Normal", alternative: "Uniform")
#' @param mu the mean of the normal prior distribution (default:0)
#' @param sig the variance of the normal prior distribution (default:10)
#' @return val a list of matrix of draws pardraws and the acceptance rate
#'
#' @examples
#' y = indicat(faithful$waiting,70)
#' x = scale(cbind(faithful$eruptions,faithful$eruptions^2))
#' data = data.frame(y,x); propob<- lapl_aprx(y,x)
#' IndepMH_n<- IndepMH(data=data,propob,iter = 102, burn = 2) # prior="Normal"
#' IndepMH_u<- IndepMH(data=data,propob,prior="Uniform",iter = 102, burn = 2) # prior="Uniform"
#' par(mfrow=c(3,1));invisible(apply(IndepMH_n$Matpram,2,function(x)plot(density(x))))
#' invisible(apply(IndepMH_u$Matpram,2,function(x)plot(density(x))));par(mfrow=c(1,1))
#'
#' @export

IndepMH<- function(data,propob=NULL,posterior=NULL,iter=1500,burn=500,vscale=1.5,
                   start=NULL,prior="Uniform",mu=0,sig=10){
  if(is.null(posterior)){
    logpost<- function(start,data) posterior(start,data,Log=T,mu=mu,sig=sig,prior=prior)
    #define posterior distribution
  }
  if(is.null(propob)){
    propob = lapl_aprx(data[,1],data[,-1])
  }
  varprop = vscale*propob$var 
  npar = length(propob$mode)
  Mat = array(0, c(iter, npar))
  if(is.null(start)){
    start = MASS::mvrnorm(n=1,propob$mode,varprop)
  }
  Mat[1,] = start; AccptRate<-0
  for(i in 2:iter){
    start= Mat[i-1,]
    prop = MASS::mvrnorm(n=1,propob$mode,varprop)#make a draw from proposal dist
    lpa = logpost(prop,data); lpb = logpost(start,data)
    accprob = exp(lpa-lpb)
    # the other part cancels out because the normal distribution is symmetric
    if(stats::runif(1)< accprob){
      Mat[i,]=prop
      AccptRate<- AccptRate +1
    }else{
      Mat[i,]=start
    }
  }
  Accept_Rate = AccptRate/iter
  val = list(Matpram=Mat[-c(1:burn),],Accept_Rate = AccptRate/iter)
  cat("IndepMH algorithm successful. Acceptance ratio = ", Accept_Rate," \n")
  return(val)
}
