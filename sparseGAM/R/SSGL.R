###################################
###################################
## FUNCTION FOR IMPLEMENTING THE ##
## SB-GAM MODELS.                ##
###################################
###################################

# This function implements group-regularized regression models in the exponential
# dispersion family with the spike-and-slab group lasso (SSGL) penalty.

# INPUTS:
# y = n x 1 vector of responses (y_1, ...., y_n) for training data
# X = n x p design matrix for training data, where ith row is (x_{i1},..., x_{ip})
# X.test = n.test x p design matrix for test data. If missing, then program computes in-sample
#          predictions on training data X
# groups = p x 1 vector of group indices or factor level names for each of the p individual covariates.
# family = the exponential disperison family. Currently allows "gaussian", "binomial", "poisson," 
#          "negativebinomial", and "gamma". Default is "gaussian" for non-binary responses and
#          "binomial" for binary responses, but "poisson" or "negativebinomial" may be more
#          appropriate for count data and "gamma" may be more appropriate for right-skewed,
#          continuous data.
# nb.size = known shape parameter for negative binomial regression. Default is 1
# gamma.shape = known shape parameter for gamma regression. Default is 1
# weights = group-specific weights. Default is to use the square roots of the group sizes.
# nlambda0 = number of lambda0's in our grid. Default is 20.
# lambda0 = a grid of values for the spike hyperparameter. Note that the program automatically orders 
#           theese in descending order, so the solution path is from least sparse to most sparse. 
#           If lambda0 is not specified, the program chooses a default equispaced grid from 100 to 5.
# lambda1 = a fixed small value for thte slab hyperparameter. Default is 1
# a = shape hyperparameter in B(a,b) prior on mixing proportion. Default is 1
# b = shape hyperparameter in B(a,b) prior on mixing proportion. Default is G for number of groups
# max.iter = maximum number of iterations. Default is 100
# tol = convergence criteria. Default is 1e-6
# print.iter = boolean variable whether to print the current lambda0 in our grid. Default is TRUE

# OUTPUT:
# lambda0 = grid of lambda0's in descending order. 
#	beta0 = L x 1 vector of estimated intercepts. The kth entry in mu corresponds to the kth spike parameter 
#         in our descending lambda0 grid.
#	beta = dp x L matrix of estimated basis coefficients. The kth column in beta corresponds to the kth 
#        spike parameter in our descending lambda0 grid.
# mu.pred = n.test x L matrix of predicted mean response values based on test data in X.test. If
#           X.test was left blank or X.test=X, then in-sample predictions on X.train are returned.
# classifications = p x L matrix of classifications, where "1" indicates that the variable was selected 
#                   and "0" indicates that the variable was not selected. The kth column in classifications 
#                   corresponds to the kth spike parameter in our descending lambda0 grid.
# loss = L x 1 vector of negative log-likelihoods for each fit. The lth entry in loss corresponds to 
#        the lth entry in our lambda0 grid.


SSGL = function(y, X, X.test, groups, 
                family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
                nb.size=1, gamma.shape=1, weights, nlambda0=20, lambda0, lambda1, a, b, 
                max.iter=100, tol = 1e-6, print.iter=TRUE) {
  
  
  ##################
  ##################
  ### PRE-CHECKS ###
  ##################
  ##################

  ## Enumerate groups if not already done
  group.numbers = as.numeric(groups)
  
  ## Number of groups and covariates overall
  X = as.matrix(X)
  G = length(unique(group.numbers))
  n = dim(X)[1]
  J = dim(X)[2]

  ## If test data is missing, make it the same as the training data
  if(missing(X.test)) X.test = X
  n.test = dim(X.test)[1]
  
  ## Check that dimensions are conformable
  if(length(y) != dim(X)[1])
    stop("Non-conformable dimensions of y and X.")
  ## Check that X and X.test have the same number of columns
  if(dim(X.test)[2] != J)
    stop("X and X.test should have the same number of columns.")

  ## Set group-specific weights
  if(missing(weights)){
    weights = rep(0, G)
    for(g in 1:G){
      weights[g] = sqrt(as.vector(table(group.numbers))[g])
    }
  }
  
  ## Coercion
  family <- match.arg(family)
  
  ## Check that the data can be used for the respective family
  if(family=="poisson" || family=="negativebinomial"){
    if(any(y<0))
      stop("All counts y must be greater than or equal to zero.")
    if(any(y-floor(y)!=0))
      stop("All counts y must be whole numbers.")
  }
  if(family=="negativebinomial"){
    ## Check that nb.size is strictly positive
    if (nb.size<=0)
      stop("Size parameter for negative binomial density must be strictly positive.")
    ## Check that J is less than or equal to n
    if(J > n) {
      stop("For group-regularized negative binomial regression, we require the
          total number of covariates to be less than or equal to sample size. 
          Consider reducing the number of covariates.")
    }
  }
  if(family=="binomial"){
    if(any(y<0))
      stop("All binary responses must be either '0' or '1.'")
    if(any(y>1))
      stop("All binary responses must be either '0' or '1.'")
    if(any(y-floor(y)!=0))
      stop("All binary responses must be either '0' or '1.'")
  }
  if(family=="gamma"){
    if(any(y<=0))
      stop("All responses y must be strictly positive.")
    if(gamma.shape<=0)
      stop("Shape parameter for gamma density must be strictly positive.")
    ## Check that J is less than or equal to n
    if(J > n) {
      stop("For group-regularized gamma regression, we require the total
          number of covariates to be less than or equal to sample size. 
          Consider reducing the number of covariates.")
    }
  }
  
  ## Check that weights are all greater than or equal to 0
  if(!missing(weights)){
    if(any(weights<0))
      stop("All group-specific weights should be nonnegative.")
  }
  ## Number of lambdas
  if(nlambda0 < 1)
    stop("The number of lambdas must be at least one.")
  ## If user specified lambda, check that all lambdas are greater than 0
  if(!missing(lambda0)) {
    nlambda0 = length(lambda0) # Override nlambda0 with the length of lambda0
    
    if (any(lambda0<=0))
      stop("All lambda0s should be strictly positive.")
  }  
  
  ## Default parameters for missing arguments
  L = nlambda0
  if(missing(lambda1)) lambda1 = 1
  if(missing(a)) a = 1
  if(missing(b)) b = G
  ## Check hyperparameters to be safe
  if ((lambda1 <= 0) || (a <= 0) || (b <= 0))
    stop("Please make sure that all hyperparameters are strictly positive.")
  if(missing(lambda0)){ 
    if(family=="gaussian"){
      lambda0=seq(from=n, to=max(n/nlambda0,lambda1+1), length=nlambda0)
    } else if(family=="binomial"){
        lambda0 = seq(from=40, to=max(40/nlambda0,lambda1+1), length=nlambda0)
    } else if(family=="poisson" || family=="negativebinomial" || family=="gamma"){
        if(n >= G){
          lambda0 = seq(from=n, to=max(n/nlambda0,5*lambda1), length=nlambda0)
        } else if(G > n){
          lambda0 = seq(from=G, to=max(G/nlambda0,20*lambda1), length=nlambda0)
        }
    }
  }
  
  ## Initialize values for beta0, beta, and theta
  beta0.init = 0                # initial beta0
  beta.init = rep(0,J)          # initial beta
  kappa.init = 0.5              # initial kappa
  
  ## Matrices and vectors to hold solutions
  beta0 = rep(0,L)
  beta = matrix(0, nrow=J, ncol=L)
  loss = rep(0,L)
  
  ####################
  ####################
  ### EM algorithm ###
  ####################
  ####################
  
  for(l in 1:L){

    ## To output iteration
    if(print.iter==TRUE)
      cat("lambda0 = ", lambda0[l], "\n")
    
    ## Run EM algorithm on training data
    EM_output = SSGL_EM(y=y, X=X, groups=group.numbers, family=family, 
                           n=n, G=G, a=a, b=b, weights=weights, 
                           lambda0=lambda0[l], lambda1=lambda1, 
                           beta0.init=beta0.init, beta.init=beta.init, 
                           kappa.init=kappa.init, nb.size=nb.size, 
                           gamma.shape=gamma.shape, max.iter=max.iter,tol=tol)
    
    ## Save values for the next pass and also store them as a 'warm start'
    ## to the next lambda0 in our grid
    if(L>1){
      beta0[l] = EM_output$beta0
      beta[,l] = EM_output$beta
      loss[l] = EM_output$loss
    
      beta0.init = beta[l]
      beta.init = beta[,l]
    } else if (L==1) {
      beta0 = EM_output$beta0
      beta = EM_output$beta
      loss = EM_output$loss
    
      beta0.init = beta0
      beta.init = beta
    }

    kappa.init = EM_output$kappa
  }
  
  # Row names for beta
  beta = as.matrix(beta)
  rownames(beta) = groups
  
  ## Compute predictions bmatrix on test data
  mu.pred = matrix(0, n.test, L) # To compute predictions on test set
  if(L>1){ 
    for(l in 1:L){
      if(family=="gaussian"){  
        mu.pred[,l] = rep(beta0[l],dim(X.test)[1])+X.test%*%beta[,l]
      } else if(family=="binomial") {
        mu.pred[,l] = 1/(1+exp(-(rep(beta0[l],dim(X.test)[1])+X.test%*%beta[,l])))
      } else if(family=="poisson" || family=="negativebinomial" || family=="gamma"){
        mu.pred[,l] = exp(rep(beta0[l],dim(X.test)[1])+X.test%*%beta[,l])
      }
    }
  } else if(L==1) {
      if(family=="gaussian"){  
        mu.pred[,1] = rep(beta0,dim(X.test)[1])+X.test%*%beta
      } else if(family=="binomial") {
        mu.pred[,1] = 1/(1+exp(-(rep(beta0,dim(X.test)[1])+X.test%*%beta)))
      } else if(family=="poisson" || family=="negativebinomial" || family=="gamma"){
        mu.pred[,1] = exp(rep(beta0,dim(X.test)[1])+X.test%*%beta)
      }
  }
  
  ## Compute classifications
  classifications = matrix(0, nrow=G, ncol=L)
  
  if(L>1){
    for(l in 1:L){
      for (g in 1:G) {
        active = which(group.numbers == g)
      
        ## Update classifications
        if(!identical(as.numeric(beta[active,l]), rep(0,length(active))))
          classifications[g,l] = 1   
      }
    }
  } else if(L==1) {
    for (g in 1:G) {
      active = which(group.numbers == g)
      
      ## Update classifications
      if(!identical(as.numeric(beta[active]), rep(0,length(active))))
        classifications[g,1] = 1   
    }
  }
  ## Row names for classifications
  row.names(classifications) = unique(groups)
  
    
  #####################
  #####################
  ### Return a list ###
  #####################
  #####################
  SSGL.output <- list(lambda0=lambda0,
                      beta0=beta0,
                      beta=beta,
                      mu.pred=mu.pred,
                      classifications=classifications,
                      loss=loss) 
  # Return list
  return(SSGL.output)
}