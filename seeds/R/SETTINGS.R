#' Automatic Calculation of optimal Initial Parameters
#' 
#' Implemented according to Engelhardt et al. 2017.
#' 
#' The function can be replaced by an user defined version if necessary.
#'  
#' @param VARIANCE             standard error of the observed stat dynamics (per time point)
#' @param N                    number of system states
#' @param BETA_LAMDBA          mcmc tuning parameter (weighting of observed states)
#' @param alphainit            mcmc tuning parameter (weighting of observed states)
#' @param betainit             mcmc tuning parameter (weighting of observed states)
#' @param ROH                  mcmc tuning parameter 
#' @param R                    mcmc tuning parameter 
#' 
#' @return                     A list of optimal initial parameters; i.e. R, Roh, Alpha, Beta, Tau, Lambda1, Lambda2
#'
#'
#'
SETTINGS <- function(VARIANCE,N,BETA_LAMDBA,alphainit,betainit,R=c(1000,1000),ROH=c(10,10)){
  
  
    if (length(alphainit)!=N) alphainit = rep(1,N)
    if (length(betainit)!=N)  {betainit  = rep(1,N)}
    
    
    CONTAINER  <- VARIANCE[,2]
    
    for (i in 3:length(VARIANCE[1,]-1)){
      CONTAINER= c(CONTAINER,VARIANCE[,i])
    }
    

    PHI   <- MASS::fitdistr(1/CONTAINER, "gamma")


    ALPHA <- rep(1,N)*PHI[[1]][1]*alphainit
    BETA  <- rep(1,N)*(PHI[[1]][2])*BETA_LAMDBA*betainit

    R[2]      <- 1000
    ROH[2]    <- 10
    R[1]      <- 1000
    ROH[1]    <- 10
    
    TAU     <- rep(1,N)
    LAMBDA1 <- rep(1,N)
    LAMBDA2 <- 1 
    
    LIST <- list(R,ROH,ALPHA,BETA,TAU,LAMBDA1,LAMBDA2)
    names(LIST) <- c("R","ROH","ALPHA","BETA","TAU","LAMBDA1","LAMBDA2")
    return(LIST)

}
