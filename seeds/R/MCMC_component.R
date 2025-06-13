#' Componentwise Adapted Metropolis Hastings Sampler 
#' 
#' Algorithm implemented according to Engelhardt et al. 2017.
#' 
#' The function can be replaced by an user defined version if necessary
#'  
#' @param objective            objective function
#' @param LOGLIKELIHOOD_func   likelihood function
#' @param STEP_SIZE            number of samples per mcmc step. This should be greater than numberStates*500.Values have direct influence on the runtime.
#' @param STEP_SIZE_INNER      number of inner samples. This should be greater 15 to guarantee a reasonable exploration of the sample space. Values have direct influnce on the runtime.
#' @param EPSILON              vector of hidden influences (placeholder for customized version)
#' @param JUMP_SCALE           ODE system
#' @param STEP                 time step of the sample algorithm corresponding to the given vector of time points 
#' @param OBSERVATIONS         observed state dynamics e.g. protein concentrations
#' @param Y0                   initial values of the system
#' @param INPUTDATA            discrete input function e.g. stimuli
#' @param PARAMETER            model parameters estimates
#' @param EPSILON_ACT          vector of current hidden influences
#' @param SIGMA                current variance of the prior for the hidden influences (calculated during the Gibbs update)
#' @param DIAG                 diagonal weight matrix of the current Gibbs step
#' @param GIBBS_par            GIBBS_PAR[["BETA"]] and GIBBS_PAR[["ALPHA"]]; prespecified or calculated vector of state weights
#' @param N                    number of system states
#' @param BURNIN               number of dismissed samples during burn-in



#' @return                     A matrix with the sampled hidden inputs (row-wise)


MCMC_component <- function(LOGLIKELIHOOD_func, STEP_SIZE, STEP_SIZE_INNER , EPSILON, JUMP_SCALE,
                           STEP,OBSERVATIONS,Y0,INPUTDATA,PARAMETER,EPSILON_ACT,SIGMA,DIAG,GIBBS_par, N, BURNIN,objective){

  number_species <- N
  epsilon_container <- matrix(rep(0,(STEP_SIZE+1)*number_species),((STEP_SIZE+1)))

    
    
    epsilon_container[2,] <- EPSILON_ACT[2,]



for (ii in 2:STEP_SIZE+1){

  eps0 <- epsilon_container[ii-1,]

  eps1 <- matrix(rep(eps0,STEP_SIZE_INNER),ncol=number_species,nrow=STEP_SIZE_INNER,byrow=TRUE)

  k <- sample(1:number_species,1) 


  MU_jump <- EPSILON_ACT[1,k]
  
  eps1[,k] <- rnorm(STEP_SIZE_INNER,MU_jump,JUMP_SCALE)

        ratio_new <- sapply(eps1[,k],LOGLIKELIHOOD_func,Step=STEP,OBSERVATIONS=OBSERVATIONS,x_0=Y0,parameters=PARAMETER,EPS_inner=EPSILON_ACT[1,],
                        D=DIAG*SIGMA,GIBBS_PAR=GIBBS_par,k=k,MU_JUMP=MU_jump,SIGMA_JUMP=JUMP_SCALE,eps_new=eps1[1,],INPUT=INPUTDATA,objectivfunc=objective)
    
        ratio_old <- LOGLIKELIHOOD_func(epsilon_container[ii-1,k],STEP,OBSERVATIONS,Y0,PARAMETER,EPSILON_ACT[1,],INPUTDATA,DIAG*SIGMA,GIBBS_par,k,MU_jump,JUMP_SCALE,eps_new=epsilon_container[ii-1,],objective)

        ratio     <- exp(ratio_new-ratio_old-dnorm(eps1[,k],MU_jump,JUMP_SCALE,log=TRUE)+dnorm(epsilon_container[ii-1,k],MU_jump,JUMP_SCALE,log=TRUE))

        
     if (sum(is.na(ratio))==STEP_SIZE_INNER) {
       epsilon_container[ii,] <- epsilon_container[ii-1,]
       }
    else{
     # if (0.95 < max((ratio),na.rm=T)){
      if (0.95 < min(1,max((ratio),na.rm=T))){
        Dummy <- matrix(eps1[which(ratio==max((ratio),na.rm=T)),],ncol=number_species)
        epsilon_container[ii,] <- Dummy[which.min(Dummy[,k]),]
        
      }
      else{
        epsilon_container[ii,] <- epsilon_container[ii-1,]
        } 
    }

}
  return(epsilon_container)
  
} 
