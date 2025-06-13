#' Gibbs Update
#' 
#' Algorithm implemented according to Engelhardt et al. 2017. The BDEN defines a conditional Gaussian prior 
#' over each hidden input. The scale of the variance of the Gaussian prior is a strongly decaying and smooth 
#' distribution peaking at zero, which depends on parameters Lambda2, Tau and Sigma. The parameter Tau is itself 
#' given by an exponential distribution (one for each component of the hidden influence vector) with parameters 
#' Lambda1. In consequence, sparsity is dependent on the parameter vector Lambda1, whereas smoothness is 
#' mainly controlled by Lambda2. These parameters are drawn from hyper-priors, which can be set in a non-informative 
#' manner or with respect to prior knowledge about the degree of shrinkage and smoothness of the hidden influences (Engelhardt et al. 2017). 
#' 
#' The function can be replaced by an user defined version if necessary
#' 
#' 
#' @param D                    diagonal weight matrix of the current Gibbs step
#' @param EPS_inner            row-wise vector of current hidden influences [tn,tn+1] 
#' @param R                    parameter for needed for the Gibbs update (for details see Engelhardt et al. 2017)
#' @param ROH                  parameter for needed for the Gibbs update (for details see Engelhardt et al. 2017)
#' @param SIGMA_0              prior variance of the prior for the hidden influences
#' @param n                    number of system states
#' @param SIGMA                current variance of the prior for the hidden influences (calculated during the Gibbs update)
#' @param LAMBDA2              current parameter (smoothness) needed for the Gibbs update (for details see Engelhardt et al. 2017) 
#' @param LAMBDA1              current parameter (sparsity)  needed for the Gibbs update (for details see Engelhardt et al. 2017)   
#' @param TAU                  current parameter (smoothness) needed for the Gibbs update (for details see Engelhardt et al. 2017) 
#'
#'
#' @return                     A list of updated Gibbs parameters; i.e. Sigma, Lambda1, Lambda2, Tau

#' 
GIBBS_update  <- function(D,EPS_inner,R,ROH,SIGMA_0,n,SIGMA,LAMBDA2,LAMBDA1,TAU){

  SIGMA_A       <- (n/2)

  SIGMA_B       <-  sum(diag(1/((SIGMA_0*0.5)+0.5*((EPS_inner[2,]-EPS_inner[1,])*(D+diag(1,n))^-1*(EPS_inner[2,]-EPS_inner[1,])))))



  SIGMA   <- 1/rgamma(1, shape = SIGMA_A, scale = SIGMA_B)

  LAMBDA2_A    <- n/2+R[2]
  LAMBDA2_B    <- 1/((2*SIGMA)^-1*sum((EPS_inner[2,]-EPS_inner[1,])^2)+ROH[2])

  LAMBDA2     <- rgamma(1, shape = LAMBDA2_A, scale = LAMBDA2_B)

  for (j in 1:n){

    if ((EPS_inner[2,j]-EPS_inner[1,j]) < 0.001){

      EPS_diff = 0.001 #10 for UVB?

    } else{
      EPS_diff <- EPS_inner[2,j]-EPS_inner[1,j]
    }

    TAU_A  <- sqrt((LAMBDA1[j]*SIGMA)/EPS_diff^2)
    TAU_B  <- LAMBDA1[j] 

    TAU[j] <- statmod::rinvgauss(1,TAU_A,TAU_B);
                            
    if ((TAU[j] == 0)||is.na(TAU[j])){
      TAU[j] <- 1;  
    }
  }
  for (j in 1:n){

    LAMBDA1_A  <- 1+R[1]
    LAMBDA1_B  <- 1/(0.5*(TAU[j]^-1)+ROH[1]) 
 
    LAMBDA1[j] <- rgamma(1, shape = LAMBDA1_A, scale = LAMBDA1_B)
  }       
  LIST <- list(SIGMA,LAMBDA1,LAMBDA2,TAU)
  names(LIST) <- c("SIGMA","LAMBDA1","LAMBDA2","TAU")
  return(LIST)
}
