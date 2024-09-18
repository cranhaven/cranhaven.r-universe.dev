######################################
######################################
## FUNCTION FOR IMPLEMENTING SPARSE ##
## BAYESIAN GAM MODELS.             ##
######################################
######################################

# This function implements sparse Bayesian generalized additive models in the exponential
# dispersion family with the spike-and-slab group lasso (SSGL) penalty.

# INPUTS:
# y = n x 1 vector of observations (y_1, ...., y_n)
# X = n x p design matrix, where ith row is (x_{i1},..., x_{ip})
# X.test = n.test x p design matrix for test data. If missing, then the program sets X.test=X
#          and computes in-sample predictions on training data X. X.test must have the same 
#          number of columns as X, but not necessarily the same number of rows. 
# df = number of basis functions to use. Default is d=6
# family = the exponential dispersion family.Allows for "gaussian", "binomial", "poisson",
#          "negativebinomial", or "gamma". 
# nb.size = known size parameter for negative binomial regression. Default is nb.size=1
# gamma.shape = known shape parameter for gamma regression. Default is gamma.shape=1
# nlambda0 = number of spike hyperparameters to use. Default is 100
# lambda0 = a grid of spike hyperparameters. If the user does not specify this, then the program
#          chooses a grid automatically
# lambda1 = slab hyperparameter in SSGL. Default is lambda1=1
# a = shape hyperparameter for B(a,b) prior on mixing proportion. Default is a=1
# b = shape hyperparameter for B(a,b) prior on mixing proportion. Default is b=
# max.iter = maximum number of iterations. Default is 100
# tol = convergence criteria. Default is 1e-6
# print.iter = boolean variable whether to print the current lambda0 in our grid. Default is TRUE

# OUTPUT:
# lambda0 = grid of lambda0's in descending order.
# f.pred = list of n.test x p matrices, where the lth matrix corresponds to the lth entry in our
#          lambda0 grid. The jth column in each matrix corresponds to the function estimates for
#          the jth covariate.
# mu.pred = n.test x L matrix of predicted mean response values based on test data in X.test. If
#           X.test was left blank or X.test=X, then in-sample predictions on X.train are returned.
# classifications = p x L matrix of group classifications, where G is the number of groups. "1" indicates
#                   that the group was selected and "0" indicates that the group was not selected. 
#                   The lth column in this matrix corresponds to the lth entry in our lambda0 grid.
# beta0 = L x 1 vector of intercept estimates. The lth entry of beta0 corresponds to the lth entry in
#         our lambda grid.
# beta = dp x L matrix of regression coefficient estimates. The lth column of beta corresponds to the
#        lth entry in our lambda0 grid.
# loss = L x 1 vector of negative log-likelihoods for each fit. The lth entry in loss corresponds to 
#        the lth entry in our lambda0 grid.


SBGAM = function(y, X, X.test, df=6,  
                family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
                nb.size=1, gamma.shape=1, nlambda0=20, lambda0, lambda1, a, b, 
                max.iter=100, tol = 1e-6, print.iter=TRUE) {
  
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
  
  ## Set test data as training data if test data not provided.
  X = as.matrix(X)
  if(missing(X.test)) X.test = X
  n.test = dim(X.test)[1]
  
  ## Check that X and X.test have the same number of columns
  if(dim(X.test)[2] != dim(X)[2])
    stop("X and X.test should have the same number of columns.")
  
  ## Number of lambdas
  if(nlambda0 < 1)
    stop("The number of lambda0's must be at least one.")
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
  
  ## Create n.test x dp B-spline matrix X.tilde.test = [X.tilde.test_1, ..., X.tilde.test_p]
  ## X.tilde.test is for test data
  X.tilde.test = matrix(0, nrow=n.test, ncol=d*p)
  if(family=="gaussian" || family=="binomial" || family=="poisson"){
    for(j in 1:p){
      X.tilde.test[,((j-1)*d+1):(j*d)] = splines::bs(X.test[,j], df=d, intercept=TRUE)
    }
  } else if(family=="negativebinomial" || family=="gamma"){
    for(j in 1:p){
      ## Negative binomial and gamma regression are based on LSA to the MLE, 
      # so we need intercept=FALSE, otherwise MLE will return NA values
      X.tilde.test[,((j-1)*d+1):(j*d)] = splines::bs(X.test[,j], df=d, intercept=FALSE)
    }
  }
   
  #######################################
  #######################################
  ### Fit the appropriate group model ###
  #######################################
  #######################################
    
  ## Fit sparse GAM with SSGL penalty
  if(!missing(lambda0)){
      ssgl.mod = SSGL(y=y, X=X.tilde, X.test=X.tilde.test, groups=groups, family=family,
                      nb.size=nb.size, gamma.shape=gamma.shape, weights=weights,
                      nlambda0=nlambda0, lambda0=lambda0, a=a, b=b, max.iter=max.iter,
                      tol=tol, print.iter=print.iter)
  } else {
      ssgl.mod = SSGL(y=y, X=X.tilde, X.test=X.tilde.test, groups=groups, family=family,
                      nb.size=nb.size, gamma.shape=gamma.shape, weights=weights,
                      nlambda0=nlambda0, a=a, b=b, max.iter=max.iter,
                      tol=tol, print.iter=print.iter)
  }
   
  ## Lambdas
  lambda0 = ssgl.mod$lambda0
  L = length(lambda0)
  ## Estimates of regression coefficients
  beta0 = ssgl.mod$beta0
  beta = ssgl.mod$beta
  ## Estimate of loss function
  loss = ssgl.mod$loss
  ## Predictions for mu.pred
  mu.pred = ssgl.mod$mu.pred
  ## Classifications
  classifications = ssgl.mod$classifications
  
  ## Compute the function evaluations for the individual functions
  f.pred.ind = matrix(0, nrow=n.test, ncol=p)
  
  ## Fill in f.hat list
  if (L>1){
    f.pred = vector(mode = "list", length = L)
    for(l in 1:L){
      for(j in 1:p){
        active = which(groups == j)
        f.pred.ind[,j] = X.tilde.test[,(d*(j-1)+1):(d*j)] %*% as.matrix(beta[active,l])
      }
      f.pred[[l]] = f.pred.ind
    }
  } else if(L==1){
    for(j in 1:p){
      active=which(groups==j)
      f.pred.ind[,j] = X.tilde.test[,(d*(j-1)+1):(d*j)] %*% as.matrix(beta[active]) 
    }
    f.pred = f.pred.ind
  }
   
   
  #####################
  #####################
  ### Return a list ###
  #####################
  #####################
  SBGAM.output <- list(lambda0 = lambda0,
                       f.pred = f.pred,
                       mu.pred = mu.pred,
                       classifications = classifications,
                       beta0 = beta0,
                       beta = beta,
                       loss = loss) 
  # Return list
  return(SBGAM.output)
}