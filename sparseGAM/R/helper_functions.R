####################
# Helper functions #
####################

#' Prior density Psi. No need for normalizing constant C_d as it cancels out
#' @keywords internal
Psi = function(beta, lambda) {
  m = length(beta)
  dens = lambda^m * exp(-lambda*sqrt(sum(beta^2)))
  
  return(dens)
}

#' pStar function
#' #' @keywords internal
pStar = function(beta, lambda1, lambda0, kappa) {
  Psi1 = Psi(beta=beta, lambda=lambda1)
  Psi0 = Psi(beta=beta, lambda=lambda0)
  
  ## if a coefficient is really large then both these will 
  ## numerically be zero because R can't handle such small numbers
  if ((kappa*Psi1) == 0 & (1 - kappa)*Psi0 == 0) {
    p = 1
  } else {
    p = (kappa*Psi1) / (kappa*Psi1 + (1 - kappa)*Psi0)
  }
  return(p)
}

#' Lambda star function
#' @keywords internal
lambdaStar = function(beta, lambda1, lambda0, kappa) {
  p = pStar(beta = beta, lambda1 = lambda1,
            lambda0 = lambda0, kappa = kappa)
  
  l = lambda1*p + lambda0*(1 - p)
  return(l)
}

#' EM algorithm for SB-GAM.
#' Here, lambda0 is a single tuning parameter
#' @keywords internal
SSGL_EM = function(y, X, groups, 
                   family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
                   n, G, a, b, weights, lambda0, lambda1, beta0.init, beta.init, kappa.init, 
                   nb.size, gamma.shape, max.iter, tol){
  
  ## Coercion
  family <- match.arg(family)
  
  ## Initialize the following values
  difference = 100*tol
  counter = 0
  pstar.k = rep(0,G)
  lambdastar.k=rep(0,G) # To hold lambdastar for each group of coefficients
  
  ## Initialize parameters
  beta0 = beta0.init
  beta = beta.init
  kappa = kappa.init
  
  ## Update the parameters
  while( (difference > tol) & (counter < max.iter) ){
  
    ## Iterate counter  
    counter = counter+1
    ## Keep track of old beta
    beta.old = beta
      
    ##############
    ##############
    ### E-step ###
    ##############
    ##############
    for(k in 1:G){
      ## Which groups are active
      active = which(groups == k)
      ## Update pStar
      pstar.k[k] = pStar(beta.old[active], lambda1, lambda0, kappa)
      # Update lambda.k.star for groups 1,...,p 
      lambdastar.k[k] = lambda1*pstar.k[k] + lambda0*(1-pstar.k[k])
    }
    
    ############## 
    ##############
    ### M-step ###
    ##############
    ##############
    
    ## Update kappa
    kappa = (a-1 + sum(pstar.k))/(a+b+G-2)
    
    ## Update beta0 and beta
    ## Note that grpreg solves is (1/2n)*loglik(beta0,beta) + pen(beta)
    ## so we have to divide by n in the penalty
    if(family=="gaussian" || family=="binomial" || family=="poisson"){
      
      solve.obj = grpreg::grpreg(X, y, group=groups, penalty="grLasso", family=family, 
                                 lambda=1, group.multiplier=weights*(lambdastar.k/n))
      beta0 = solve.obj$beta[1]
      beta = solve.obj$beta[-1]
      loss = solve.obj$loss
    
    } else if(family=="negativebinomial"){
      
      solve.obj = grpreg.nb(y=y, X=X, groups=groups, nb.size=nb.size, penalty="gLASSO", 
                            weights=weights*(lambdastar.k/n), lambda=1)  
      beta0 = solve.obj$beta0
      beta = solve.obj$beta
      loss = solve.obj$loss
      
    } else if(family=="gamma"){
      
      solve.obj = grpreg.gamma(y=y, X=X, groups=groups, gamma.shape=gamma.shape, penalty="gLASSO",
                               weights=weights*(lambdastar.k/n), lambda=1)
      beta0 = solve.obj$beta0
      beta = solve.obj$beta
      loss = solve.obj$loss
    }
    
    ## Update diff
    diff = sum((beta-beta.old)^2)
  }
  
  ## Store beta0, beta, kappa in a list
  SSGL.EM.output <- list(beta0 = beta0,
                          beta = beta,
                          kappa = kappa,
                          loss = loss)
  
  # Return list
  return(SSGL.EM.output)
}