##########################################
##########################################
## FUNCTION FOR IMPLEMENTING K-FOLD     ##
## CROSS-VALIDATION FOR SPARSE BAYESIAN ##
## GENERALIZED ADDITIVE MODELS          ##
##########################################
##########################################

# This function implements K-fold cross-validation for sparse Bayesian GAMs
# in the exponential dispersion family. 

# INPUTS:
# y = n x 1 vector of observations (y_1, ...., y_n)
# X = n x p design matrix, where ith row is (x_{i1},..., x_{ip})
# df = number of basis functions to use. Default is d=6
# family = the exponential dispersion family.Allows for "gaussian", "binomial", "poisson",
#          "negativebinomial", or "gamma". 
# nb.size = known size parameter for negative binomial regression. Default is nb.size=1
# gamma.shape = known shape parameter for gamma regression. Default is gamma.shape=1
# nfolds = number of folds in K-fold cross-validation. Default is nfolds=5.
# nlambda0 = number of spike hyperparameters to use. Default is 100
# lambda0 = a grid of spike hyperparameters. If the user does not specify this, then the program
#          chooses a grid automatically
# lambda1 = slab hyperparameter in SSGL. Default is lambda1=1
# a = shape hyperparameter for B(a,b) prior on mixing proportion. Default is a=1
# b = shape hyperparameter for B(a,b) prior on mixing proportion. Default is b=dim(X)[2]
# max.iter = maximum number of iterations. Default is 100
# tol = convergence criteria. Default is 1e-6
# print.fold = boolean variable whether to print the current fold. Default is TRUE

# OUTPUT:
# lambda0 = grid of L lambda0's in descending order.
# cve = L x 1 vector of mean cross-validation error across all K folds. The kth entry in cve corresponds
#       to the kth spike hyperparameter in our lambda0 grid. The CVE on each of the K validation sets 
#       is the mean loss (negative log-likelihood) evaluated on that set.
# cvse = L x 1 vector of standard errors for cross-validation error across all K folds. 
#        The kth entry in cvse corresponds to the kth spike hyperparameter in our lambda0 grid.
# lambda.min = value of lambda0 that minimizes mean cross-validation error.

cv.SBGAM = function(y, X, df=6, family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
                    nb.size=1, gamma.shape=1, nfolds=5, nlambda0=20, lambda0, lambda1, a, b,
                    max.iter=100, tol = 1e-6, print.fold=TRUE) {
  
  ##################
  ##################
  ### PRE-CHECKS ###
  ##################
  ##################
  
  ## Coercion
  family = match.arg(family)
   
  ## Number of groups and covariates overall
  n = dim(X)[1]
  p = dim(X)[2]
  d = as.integer(df) # force d to be an integer
  # Set weights all equal to 1, because the group sizes are all equal
  weights = rep(1,p)

  ## Check that dimensions are conformal
  if( length(y) != dim(X)[1] )
    stop("Non-conformal dimensions and X.")
  ## Check that degrees of freedom is >=3
  if( df <= 2 )
    stop("Please enter a positive integer greater than or equal to three for degrees of freedom.")
  ## Check that the ladder is increasing and that all relevant hyperparameters are positive
     
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
    ## Check that d*p is less than or equal to n
    if(d*p > n) {
      stop("For group-regularized negative binomial regression, we require the total
          number of basis coefficients to be less than or equal to sample size. 
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
    if(d*p > n) {
      stop("For group-regularized gamma regression, we require the total number
          of basis coefficients to be less than or equal to sample size. 
          Consider reducing the number of covariates.")
    }
  }
  
  ## Number of lambdas
  if(nlambda0 < 2)
    stop("For cross-validation, nlambda0 should be at least 2.")
  ## If user specified lambda, check that all lambdas are greater than 0
  if(!missing(lambda0)) {
    nlambda = length(lambda0) # Override nlambda with the length of lambda
    
    if (any(lambda0<=0))
      stop("All lambda0's should be strictly positive.")
  } 
  
  ## Default parameters for missing arguments
  if(missing(lambda1)) lambda1 = 1
  if(missing(a)) a = 1
  if(missing(b)) b = p
  ## Check hyperparameters to be safe
  if ((lambda1 <= 0) || (a <= 0) || (b <= 0))
    stop("Please make sure that all hyperparameters are strictly positive.")
  
  
  ################################
  ################################
  ### CONSTRUCT B-SPLINE BASIS ###
  ### EXPANSION MATRICES       ###
  ################################
  ################################

  ## Designate the groups of basis coefficients
  groups = rep(1:p, each=d)   
  
  ## Create n x dp B-spline matrix X.tilde = [X.tilde_1, ..., X.tilde_p], where each X.tilde_j is n x d
  ## X.tilde is for training data
  X.tilde = matrix(0, nrow=n, ncol=d*p)
  if(family=="gaussian" || family=="binomial" || family=="poisson"){
    for(j in 1:p){
      X.tilde[,((j-1)*d+1):(j*d)] = splines::bs(X[,j], df=d, intercept=TRUE)
    } 
  } else if(family=="negativebinomial" || family=="gamma"){
      for(j in 1:p){
        ## Negative binomial and gamma regression are based on LSA to the MLE, 
        ## so we need intercept=FALSE, otherwise MLE will return NA values
        X.tilde[,((j-1)*d+1):(j*d)] = splines::bs(X[,j], df=d, intercept=FALSE)
      }
  }

   
  #######################################
  #######################################
  ### Fit the appropriate group model ###
  #######################################
  #######################################
    
  ## Fit sparse GAM with SSGL penalty
  if(!missing(lambda0)){
      cv.ssgl.mod = cv.SSGL(y=y, X=X.tilde, groups=groups, family=family,
                            nb.size=nb.size, gamma.shape=gamma.shape, weights=weights, nfolds=nfolds,
                            nlambda0=nlambda0, lambda0=lambda0, a=a, b=b, max.iter=max.iter,
                            tol=tol, print.fold=print.fold)
  } else {
      cv.ssgl.mod = cv.SSGL(y=y, X=X.tilde, groups=groups, family=family,
                            nb.size=nb.size, gamma.shape=gamma.shape, weights=weights, nfolds=nfolds,
                            nlambda0=nlambda0, a=a, b=b, max.iter=max.iter,
                            tol=tol, print.fold=print.fold)
  }
   
  ## Output
  lambda0=cv.ssgl.mod$lambda0
  cve=cv.ssgl.mod$cve
  cvse=cv.ssgl.mod$cvse
  lambda0.min=cv.ssgl.mod$lambda0.min
   
   
  #####################
  #####################
  ### Return a list ###
  #####################
  #####################
  cv.SBGAM.output <- list(lambda0=lambda0,
                          cve=cve,
                          cvse=cvse,
                          lambda0.min=lambda0.min) 
  # Return list
  return(cv.SBGAM.output)
}