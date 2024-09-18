#############################################
#############################################
## FUNCTION FOR IMPLEMENTING K-FOLD        ##
## CROSS-VALIDATION FOR SPARSE FREQUENTIST ##
## GENERALIZED ADDITIVE MODELS             ##
#############################################
#############################################

# This function implements K-fold cross-validation for sparse frequentist GAMs
# in the exponential dispersion family. 

# INPUTS:
# y = n x 1 vector of observations (y_1, ...., y_n)
# X = n x p design matrix, where ith row is (x_{i1},..., x_{ip})
# df = number of basis functions to use. Default is d=6
# family = the exponential dispersion family.Allows for "gaussian", "binomial", "poisson",
#          "negativebinomial", or "gamma". 
# penalty = group regularization method to use on the groups of basis coefficients. The options 
#           are "gLASSO", "gSCAD", or "gMCP".
# nb.size = known size parameter for negative binomial regression. Default is nb.size=1
# gamma.shape = known shape parameter for gamma regression. Default is gamma.shape=1
# nfolds = number of folds to use in K-fold cross-validation. Default is nfolds=10
# nlambda = number of tuning parameters to use. Default is 100
# lambda = a grid of tuning parameters. If the user does not specify this, then the program
#          chooses a grid automatically
# taper = tapering term in group SCAD and group MCP controlling how rapidly the penalty 
#         tapers off. Default is taper=4 for group SCAD and taper=3 for group MCP. This is ignored 
#         if "gLASSO" is specified as the penalty.
# max.iter = maximum number of iterations. Default is 10,000
# tol = convergence criteria. Default is 1e-4

# OUTPUT:
# lambda = grid of L lambda's in descending order.
# cve = L x 1 vector of mean cross-validation error across all K folds. The kth entry in cve corresponds
#       to the kth regularization parameter in our lambda grid. The CVE on each of the K validation sets 
#       is the mean loss (negative log-likelihood) evaluated on that set.
# cvse = L x 1 vector of standard errors for cross-validation error across all K folds. 
#        The kth entry in cvse corresponds to the kth regularization parameter in our lambda grid.
# lambda.min = value of lambda that minimizes mean cross-validation error.


cv.SFGAM = function(y, X, df=6, family=c("gaussian","binomial", "poisson", "negativebinomial","gamma"),
                    nb.size=1, gamma.shape=1, penalty=c("gLASSO","gMCP","gSCAD"), taper, 
                    nfolds=10, nlambda=100, lambda, max.iter=10000, tol=1e-4) {
  
  ##################
  ##################
  ### PRE-CHECKS ###
  ##################
  ##################
  
  ## Coercion
  penalty = match.arg(penalty)
  family = match.arg(family)

  ## Number of groups and covariates overall
  n = dim(X)[1]
  p = dim(X)[2]
  d = as.integer(df) # force d to be an integer

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
  
  ## Set taper parameter if not specified.
  if(missing(taper)){
    if(penalty=="gSCAD") taper=4
    if(penalty=="gMCP") taper=3
  }
  
  ## Check that taper parameter is greater than 2 for gSCAD and greater than 1 for gMCP
  if(penalty=="gSCAD"){
    if(taper<=2)
      stop("The taper parameter must be greater than 2 for the group SCAD penalty.")
  }
  if(penalty=="gMCP"){
    if(taper<=1)
      stop("The taper parameter must be greater than 1 for the group MCP penalty.")
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
  
  ## Train model on training set
  if(family=="gaussian" || family=="binomial" || family=="poisson"){
   
     if(penalty=="gLASSO"){
        if(!missing(lambda)){
          cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grLasso",
                                       nlambda=nlambda, lambda=lambda, nfolds=nfolds, 
                                       eps=tol, max.iter=max.iter)
        } else {
          cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grLasso",
                                       nlambda=nlambda, nfolds=nfolds, eps=tol, max.iter=max.iter)
        }
      
      } else if(penalty=="gSCAD"){
          if(!missing(lambda)){
              cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grSCAD",
                                           nlambda=nlambda, lambda=lambda, nfolds=nfolds, 
                                           gamma=taper, eps=tol, max.iter=max.iter)
            } else {
              cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grSCAD",
                                           nlambda=nlambda, nfolds=nfolds, gamma=taper, eps=tol, 
                                           max.iter=max.iter)
            }
      } else if(penalty=="gMCP"){
          if(!missing(lambda)){
              cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grMCP",
                                           nlambda=nlambda, lambda=lambda, nfolds=nfolds, 
                                           gamma=taper, eps=tol, max.iter=max.iter)
            } else {
              cv.SFGAM = grpreg::cv.grpreg(X=X.tilde, y=y, group=groups, family=family, penalty="grMCP",
                                           nlambda=nlambda, nfolds=nfolds, gamma=taper, eps=tol, 
                                           max.iter=max.iter)
            }
      }  
      
      ## Save desired values
      lambda = cv.SFGAM$lambda
      cve = cv.SFGAM$cve
      cvse = cv.SFGAM$cvse
      lambda.min = cv.SFGAM$lambda.min
        
    } else if(family=="negativebinomial"){
        if(!missing(lambda)){
            cv.SFGAM = cv.grpreg.nb(y=y, X=X.tilde, groups=groups, nb.size=nb.size, penalty=penalty,
                                    nfolds=nfolds, nlambda=nlambda, lambda=lambda, max.iter=max.iter,
                                    tol = tol)
        } else {
            cv.SFGAM = cv.grpreg.nb(y=y, X=X.tilde, groups=groups, nb.size=nb.size, penalty=penalty,
                                    nfolds=nfolds, nlambda=nlambda, max.iter=max.iter, tol = tol)
        }
        
      ## Save desired values
      lambda = cv.SFGAM$lambda
      cve = cv.SFGAM$cve
      cvse = cv.SFGAM$cvse
      lambda.min = cv.SFGAM$lambda.min
    
    } else if(family=="gamma"){
        if(!missing(lambda)){
            cv.SFGAM = cv.grpreg.gamma(y=y, X.tilde, groups=groups, gamma.shape=gamma.shape,
                                       penalty=penalty, nfolds=nfolds, taper=taper, lambda=lambda,
                                       nlambda=nlambda, max.iter=max.iter, tol=tol)
        } else {
            cv.SFGAM = cv.grpreg.gamma(y=y, X.tilde, groups=groups, gamma.shape=gamma.shape,
                                       penalty=penalty, nfolds=nfolds, taper=taper, nlambda=nlambda,
                                       max.iter=max.iter, tol=tol)
        }
        
        ## Save desired values
        lambda = cv.SFGAM$lambda
        cve = cv.SFGAM$cve
        cvse = cv.SFGAM$cvse
        lambda.min = cv.SFGAM$lambda.min
      }
  
  ## Return a list
  cv.SFGAM.output <- list(lambda=lambda,
                          cve=cve,
                          cvse=cvse,
                          lambda.min=lambda.min)
  # Return list
  return(cv.SFGAM.output)
  
}