#################################################
#################################################
## FUNCTION FOR IMPLEMENTING GROUP-REGULARIZED ##
## GAMMA REGRESSION MODELS                     ##
#################################################
#################################################

# This function implements K-fold cross-validation for group-regularized regression
# models in the exponential dispersion family with the spike-and-slab group lasso
# (SSGL) penalty.

# INPUTS:
# y = n x 1 vector of responses (y_1, ...., y_n) for training data
# X = n x p design matrix for training data, where ith row is (x_{i1},..., x_{ip})
# groups = J x 1 vector of group indices or factor level names for each of the p individual covariates.
# family = the exponential disperison family. Currently allows "gaussian", "binomial", "poisson," 
#          "negativebinomial", and "gamma". Default is "gaussian" for non-binary responses and
#          "binomial" for binary responses, but "poisson" or "negativebinomial" may be more
#          appropriate for count data and "gamma" may be more appropriate for right-skewed,
#          continuous data.
# nb.size = known shape parameter for negative binomial regression. Default is 1
# gamma.shape = known shape parameter for gamma regression. Default is 1
# weights = group-specific weights. Default is to use the square roots of the group 
# nfolds = number of folds to use in K-fold cross-validation. Default is nfolds=5.
# nlambda0 = number of lambda0's in our grid. Default is 20.
# lambda0 = a grid of values for the spike hyperparameter. Note that the program automatically orders 
#           theese in descending order, so the solution path is from least sparse to most sparse. 
#           If lambda0 is not specified, the program chooses a default equispaced grid from 100 to 5.
# lambda1 = a fixed small value for thte slab hyperparameter. Default is 1
# a = shape hyperparameter in B(a,b) prior on mixing proportion. Default is 1
# b = shape hyperparameter in B(a,b) prior on mixing proportion. Default is G for number of groups
# max.iter = maximum number of iterations. Default is 10,000
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

cv.SSGL = function(y, X, groups, 
                   family=c("gaussian","binomial","poisson","negativebinomial","gamma"), 
                   nb.size=1, gamma.shape=1, weights, nfolds=5, nlambda0=20,
                   lambda0, lambda1, a, b, max.iter=100, tol=1e-6, print.fold=TRUE) {

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

  ## Check that dimensions are conformable
  if(length(y) != dim(X)[1])
    stop("Non-conformable dimensions of y and X.")  
  
  ## Coercion
  if(missing(family)){
    if(all(y==1 || y==0)){ 
      family=="binomial"
    } else {
      family=="gaussian"
    }
  }
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
  
  ## Set group-specific weights
  if(missing(weights)){
    weights = rep(0, G)
    for(g in 1:G){
      weights[g] = sqrt(as.vector(table(group.numbers))[g])
    }
  }
  
  ## Check that weights are all greater than or equal to 0
  if(!missing(weights)){
    if(any(weights<0))
      stop("All group-specific weights should be nonnegative.")
  }
  
  ## Number of lambdas
  if(nlambda0 < 2)
    stop("For cross-validation, nlambda0 should be at least 2.")  
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
  
  ## Randomly shuffle the data
  new.order = sample(n)
  X.new = X[new.order,]
  y.new = y[new.order]
  
  ## Create K equally-sized folds
  folds = cut(seq(1,n), breaks=nfolds, labels=FALSE)
  
  ## To store the cross-validation error
  folds.cve = matrix(0, nfolds, nlambda0)
  
  for(k in 1:nfolds){
    
    ## To output iteration
    if(print.fold==TRUE)
      cat("Fold number", k, "\n")
    
    
    ## Indices for validation set
    val_ind = which(folds==k,arr.ind=TRUE)
    
    y.val = y.new[val_ind] 
    X.val = X.new[val_ind, ]
    
    ## Indices for training set
    y.train = y.new[-val_ind]
    X.train = X.new[-val_ind, ]
    
    ## Train model on training set
    SSGL.mod.train = SSGL(y=y.train, X=X.train, X.test=X.val, groups=group.numbers, family=family,
                          nb.size=nb.size, gamma.shape=gamma.shape, weights=weights,
                          nlambda0=nlambda0, lambda0=lambda0, lambda1=lambda1,
                          a=a, b=b, max.iter=max.iter, tol=tol, print.iter=FALSE)
    
    ## Compute cross-validation error (prediction error) on test set
    beta0.hat = SSGL.mod.train$beta0
    beta.hat = SSGL.mod.train$beta

    for(l in 1:nlambda0){
        ## Store CVE for all lambdas in the kth row of folds.cve
        folds.cve[k,l] = mean((y.val-SSGL.mod.train$mu.pred[,l])^2)
    } 
  }
  
  ## Mean cross-validation error
  cve = colMeans(folds.cve)
  ## CVE standard error
  cvse = apply(folds.cve,2,stats::sd)
  ## Lambda which minimizes cve
  min.cve.index = which.min(cve)
  lambda0.min = lambda0[min.cve.index]
  
  
  ## Return a list
 cv.SSGL.output <- list(lambda0=lambda0,
                        cve=cve,
                        cvse=cvse,
                        lambda0.min=lambda0.min)
  # Return list
  return(cv.SSGL.output)
}