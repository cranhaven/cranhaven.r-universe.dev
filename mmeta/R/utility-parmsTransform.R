

parmsToOriginal <- function(parms_transformed) {
  
  
  a1 <- exp(parms_transformed[1]); 
  b1 <- exp(parms_transformed[2])
  a2 <- exp(parms_transformed[3]); 
  b2 <- exp(parms_transformed[4])
  if(length(parms_transformed) >= 5){
    eta <- parms_transformed[5]
    cc <- sqrt(a1*a2*b1*b2)/sqrt((a1+b1+1)*(a2+b2+1))
    upper_bound <- cc/max(a1*b2, a2*b1)
    lower_bound <- -cc/max(a1*a2, b1*b2)
    rho <- (upper_bound-lower_bound)*expit(eta) + lower_bound
  } else{
    rho <- 0  
  }
  parm_origianl <- c(a1,b1,a2,b2,rho)
  names(parm_origianl) <- c('a1','b1','a2','b2','rho')
  return(parm_origianl)
}



parmsToTransformed <- function(parms_original) {
  a1 <- parms_original[1]
  b1 <- parms_original[2]
  a2 <- parms_original[3]
  b2 <- parms_original[4]
  loga1 <- log(a1); 
  logb1 <- log(b1)
  loga2 <- log(a2); 
  logb2 <- log(b2)
  if(length(parms_original) >= NUM_PARAMETERS_SARMANOV){
    rho <- parms_original[5]
    cc <- sqrt(a1*a2*b1*b2)/sqrt((a1+b1+1)*(a2+b2+1))
    upper_bound <- cc/max(a1*b2, a2*b1)
    lower_bound <- -cc/max(a1*a2, b1*b2)
    eta <- logit((rho - lower_bound) / (upper_bound-lower_bound))
  } else{
    rho <- 0  
  }
  
  parms_transformed <- c(loga1,logb1,loga2,logb2,eta)
  names(parms_transformed) <- c('loga1','logb1','loga2','logb2','eta')
  return(parms_transformed)
}




parmsPosterior <- function(parms_prior, data){
  ### load parameter
  a1 <- parms_prior$a1
  b1 <- parms_prior$b1
  a2 <- parms_prior$a2
  b2 <- parms_prior$b2
  
  if('rho' %in% names(parms_prior)){
    rho <- parms_prior$rho
    model <- 'Samarnov'
  } else model <- 'Independent'
  
  ### load data
  y1 <- data$y1
  n1 <- data$n1
  y2 <- data$y2
  n2 <- data$n2
  ## calculate posterior parameters
  alpha1 <- a1+y1
  beta1 <- b1+n1-y1
  alpha2 <- a2+y2
  beta2 <- b2+n2-y2
  if(model == 'Samarnov'){
    parms_posterior  <- c(alpha1,beta1,alpha2,beta2,rho)
    names(parms_posterior) <- c('alpha1', 'beta1', 'alpha2', 'beta2','rho')  
  } else{
    parms_posterior  <- c(alpha1,beta1,alpha2,beta2)
    names(parms_posterior) <- c('alpha1', 'beta1', 'alpha2', 'beta2')  
  }
  
  
  return(parms_posterior)
}
