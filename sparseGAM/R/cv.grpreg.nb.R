############################################
############################################
## FUNCTION FOR IMPLEMENTING K-FOLD CROSS ##
## VALIDATION IN GROUP-REGULARIZED        ##
## NEGATIVE BINOMIAL REGRESSION MODELS    ##
############################################
############################################

# This function implements K-fold cross-validation for group-regularized
# negative binomial regression models. We employ the least squares approximation 
# approach of Wang and Leng (2007), and hence, the program does not allow for the 
# number of columns of X to be greater than sample size.


# INPUTS:
# y = n x 1 vector of responses (y_1, ...., y_n) for training data
# X = n x p design matrix for training data, where J is the total number of coefficients
# groups = p x 1 vector of group indices or factor level names for each of the p individual covariates.
# nb.size = known size parameter alpha in NB(alpha, mu_i) distribution for the responses.
#           Default is nb.size=1
# penalty = group regularization method to apply. Options are "gLASSO" for group lasso,
#           "gSCAD" for group SCAD, and "gMCP" for group MCP. Negative binomial regression for
#           the SSGL penalty is available in the stand-alone SSGL function.
# nfolds = number of folds in K-fold cross-validation. Default is nfolds=10
# weights = group-specific weights for the penalty. Default is to use the square roots of the 
#           group sizes
# taper = tapering term in group SCAD and group MCP controlling how rapidly the penalty 
#         tapers off. Default is taper=4 for group SCAD and taper=3 for group MCP. This is ignored 
#         if "gLASSO" is specified as the penalty.
# nlambda = number of tuning parameters to use. Default is 100
# lambda = a grid of tuning parameters. If the user does not specify this, then the program
#          chooses a grid automatically
# max.iter = maximum number of iterations. Default is 10,000
# tol = convergence criteria. Default is 1e-6

# OUTPUT:
# lambda = grid of L lambda's in descending order.
# cve = L x 1 vector of mean cross-validation error across all K folds. The kth entry in cve corresponds
#       to the kth regularization parameter in our lambda grid. The CVE on each of the K validation sets 
#       is the mean loss (negative log-likelihood) evaluated on that set.
# cvse = L x 1 vector of standard errors for cross-validation error across all K folds. 
#        The kth entry in cvse corresponds to the kth regularization parameter in our lambda grid.
# lambda.min = value of lambda that minimizes mean cross-validation error.

cv.grpreg.nb = function(y, X, groups, nb.size=1, penalty=c("gLASSO","gSCAD","gMCP"),
                        nfolds=10, weights, taper, nlambda=100, lambda, max.iter=10000, tol=1e-4) {

  ##################
  ##################
  ### PRE-CHECKS ###
  ##################
  ##################
  
  ## Coercion
  penalty = match.arg(penalty)
  
  ## Enumerate groups if not already done
  group.numbers = as.numeric(groups)
  
  ## Number of groups and covariates overall
  X = as.matrix(X)
  G = length(unique(group.numbers))
  n = dim(X)[1]
  J = dim(X)[2]
  
  ## Check that J is less than or equal to (nfolds-1)/nfolds * n
  if(J > (nfolds-1)/nfolds*n){
    stop("For cross-validation in group-regularized negative binomial regression, we require the 
         total number of covariates to be less than or equal to sample size*(nfolds-1)/nfolds. 
         Consider reducing the number of covariates.")
  }
  
  ## Set taper parameter if not specified.
  if(missing(taper)){
    if(penalty=="gSCAD") taper=4
    if(penalty=="gMCP") taper=3
  }
  
  ## Set group-specific weights
  if(missing(weights)){
    weights = rep(0, G)
    for(g in 1:G){
      weights[g] = sqrt(as.vector(table(group.numbers))[g])
    }
  }
  
  ## Check that dimensions are conformable
  if(length(y) != dim(X)[1])
    stop("Non-conformable dimensions of y and X.")
  
  ## Check that taper parameter is greater than 2 for gSCAD and greater than 1 for gMCP
  if(penalty=="gSCAD"){
    if(taper<=2)
      stop("The taper parameter must be greater than 2 for the group SCAD penalty.")
  }
  if(penalty=="gMCP"){
    if(taper<=1)
      stop("The taper parameter must be greater than 1 for the group MCP penalty.")
  }
  
  ## Check that nb.size is strictly positive
  if (nb.size<=0)
    stop("Size parameter for negative binomial density must be strictly positive.")
  ## Check that the response variables are strictly nonnegative whole numbers
  if(any(y<0))
    stop("All counts y must be greater than or equal to zero.")
  if(any(y!=floor(y)))
    stop("All counts y must be whole numbers.")
  ## Check that weights are all greater than or equal to 0
  if(!missing(weights)){
    if(any(weights<0))
      stop("All group-specific weights should be nonnegative.")
  }
  
  ## Number of lambdas
  if(nlambda < 2)
      stop("For cross-validation, nlambda should be at least 2.")
  ## If user specified lambda, check that all lambdas are greater than 0
  if(!missing(lambda)) {
    nlambda = length(lambda) # Override nlambda with the length of lambda

    if (any(lambda<=0))
      stop("All lambdas should be strictly positive.")
  }  
  
  ## If lambda is not specified
  if(missing(lambda)) {
    max.lambda = 1
    eps = .05
    
    ## Create grid of lambdas
    if(nlambda==1){ 
      lambda = max.lambda*eps 
    } else if(nlambda > 1) { 
      lambda = rep(0, nlambda)
      lambda[1] = max.lambda
      lambda[nlambda] = max.lambda*eps
      
      if(nlambda >= 3){
        for(l in 2:(nlambda-1)){
          ## equispaced lambdas on log scale
          loglambda = log(lambda[1])-(l-1)*((log(lambda[1])-log(lambda[nlambda]))/(nlambda-1))
          lambda[l] = exp(loglambda)
        }
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
  folds.cve = matrix(0, nfolds, nlambda)

  for(k in 1:nfolds){
    
    ## Indices for validation set
    val_ind = which(folds==k,arr.ind=TRUE)
    
    y.val = y.new[val_ind] 
    X.val = X.new[val_ind, ]
    
    ## Indices for training set
    y.train = y.new[-val_ind]
    X.train = X.new[-val_ind, ]
    
    ## Train model on training set
    nb.mod.train = grpreg.nb(y=y.train, X=X.train, groups=group.numbers, nb.size=nb.size,
                             penalty=penalty, weights=weights, taper=taper, lambda=lambda,
                             nlambda=nlambda, max.iter=max.iter, tol=tol)
    
    ## Compute cross-validation error (prediction error) on test set
    beta0.hat = nb.mod.train$beta0
    beta.hat = nb.mod.train$beta
    
    # To compute predicted value for a single observation
    mu.pred.ind = matrix(0, length(val_ind), nlambda) 
    # Compute CVE
    for(l in 1:nlambda){
      for(m in 1:dim(X.val)[1]){
        eta.pred.ind = t(as.matrix(X.val[m,])) %*% beta.hat[,l] + beta0.hat[l]
        mu.pred.ind[m,l] = exp(eta.pred.ind)
      }
      
      ## Store CVE for all lambdas in the kth row of folds.cve
      folds.cve[k,l] = mean((y.val-mu.pred.ind[,l])^2)
    }
  }
  
  ## Mean cross-validation error
  cve = colMeans(folds.cve)
  ## CVE standard error
  cvse = apply(folds.cve,2,stats::sd)
  ## Lambda which minimizes cve
  min.cve.index = which.min(cve)
  lambda.min = lambda[min.cve.index]
  
  
  ## Return a list
 cv.grpreg.nb.output <- list(lambda=lambda,
                             cve=cve,
                             cvse=cvse,
                             lambda.min=lambda.min)
  # Return list
  return(cv.grpreg.nb.output)
}