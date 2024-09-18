######################################
######################################
## FUNCTION FOR IMPLEMENTING SPARSE ##
## FREQUENTIST GAM MODELS.          ##
######################################
######################################

# This function implements sparse frequentist generalized additive models in the exponential
# dispersion family with the group LASSO, group SCAD, and group MCP penalties.

# INPUTS:
# y = n x 1 vector of observations (y_1, ...., y_n)
# X = n x p design matrix, where ith row is (x_{i1},..., x_{ip})
# X.test = n.test x p design matrix for test data. If missing, then the program sets X.test=X
#          and computes in-sample predictions on training data X. X.test must have the same 
#          number of columns as X, but not necessarily the same number of rows. 
# df = number of basis functions to use. Default is d=6
# family = the exponential dispersion family.Allows for "gaussian", "binomial", "poisson",
#          "negativebinomial", or "gamma". 
# penalty = group regularization method to use on the groups of basis coefficients. The options 
#           are "gLASSO", "gSCAD", or "gMCP".
# nb.size = known size parameter for negative binomial regression. Default is nb.size=1
# gamma.shape = known shape parameter for gamma regression. Default is gamma.shape=1
# taper = tapering term in group SCAD and group MCP controlling how rapidly the penalty 
#         tapers off. Default is taper=4 for group SCAD and taper=3 for group MCP. This is ignored 
#         if "gLASSO" is specified as the penalty.
# nlambda = number of tuning parameters to use. Default is 100
# lambda = a grid of tuning parameters. If the user does not specify this, then the program
#          chooses a grid automatically
# max.iter = maximum number of iterations. Default is 10,000
# tol = convergence criteria. Default is 1e-4

# OUTPUT:
# lambda0 = grid of L lambda's in descending order.
# f.pred = list of L n.test x p matrices, where the kth matrix in the list corresponds to the kth spike 
#          hyperparameter in our lambda0 grid. The jth column in the kth matrix in f.pred is the 
#          estimate of the jth function evaluated on the test data in X.test for the jth covariate 
#          (or training data X if not argument was specified for X.test). 
# mu.pred = n.test x L matrix of predicted mean response values based on test data in X.test. If
#           X.test was left blank or X.test=X, then in-sample predictions on X.train are returned.
# classifications = p x L matrix of group classifications, where G is the number of groups. "1" indicates
#                   that the group was selected and "0" indicates that the group was not selected. 
#                   The lth column in this matrix corresponds to the lth entry in our lambda grid.
# beta0 = L x 1 vector of intercept estimates. The lth entry of beta0 corresponds to the lth entry in
#         our lambda grid.
# beta = dp x L matrix of regression coefficient estimates. The lth column of beta corresponds to the
#        lth entry in our lambda grid.
# loss = L x 1 vector of negative log-likelihoods for each fit. The lth entry in loss corresponds to 
#        the lth entry in our lambda grid.


SFGAM = function(y, X, X.test, df=6,
                 family=c("gaussian","binomial", "poisson", "negativebinomial","gamma"),
                 nb.size=1, gamma.shape=1, penalty=c("gLASSO","gMCP","gSCAD"), 
                 taper, nlambda=100, lambda, max.iter=10000, tol=1e-4) {
  
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
  if(missing(X.test)) X.test = X
  n.test = dim(X.test)[1]
  
  ## Check that X and X.test have the same number of columns
  if(dim(X.test)[2] != dim(X)[2])
    stop("X and X.test should have the same number of columns.")
  
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
  if(nlambda < 1)
    stop("The number of lambdas must be at least one.")
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
  
  if(family=="gaussian" || family=="binomial" || family=="poisson"){
    
    if(penalty=="gLASSO") penalty="grLasso"
    if(penalty=="gMCP") penalty="grMCP"
    if(penalty=="gSCAD") penalty="grSCAD"
    
    ## Fit sparse GAM
    if(!missing(lambda)){
      if(penalty=="grLasso"){
          gam.mod = grpreg::grpreg(X=X.tilde, y=y, group=groups, penalty=penalty, family=family,
                                   lambda=lambda, eps=tol, max.iter=max.iter)
      } else if(penalty=="grMCP" || penalty=="grSCAD") {
        gam.mod = grpreg::grpreg(X=X.tilde, y, group=groups, penalty=penalty, family=family,
                                 lambda=lambda, eps=tol, max.iter=max.iter, gamma=taper)
      }
    } else {
        if(penalty=="grLasso"){
          gam.mod = grpreg::grpreg(X=X.tilde, y=y, group=groups, penalty=penalty, family=family,
                                   nlambda=nlambda, eps=tol, max.iter=max.iter)
        } else if(penalty=="grMCP" || penalty=="grSCAD") {
          gam.mod = grpreg::grpreg(X=X.tilde, y=y, group=groups, penalty=penalty, family=family,
                                   nlambda=nlambda, eps=tol, max.iter=max.iter, gamma=taper)
        }
    }
    
    ## Lambdas
    lambda = gam.mod$lambda
    L = length(lambda)
    ## Estimates of regression coefficients
    beta0 = gam.mod$beta[1,]
    beta = gam.mod$beta[-1,]
    ## Estimate of loss function
    loss = gam.mod$loss
    ## Predictions for mu.pred
    mu.pred = stats::predict(gam.mod, X.tilde.test, type="response")

    ## Compute classifications
    classifications = matrix(0, nrow=p, ncol=L)
    if(L>1){
      for(l in 1:L){
        for (j in 1:p) {
          active = which(groups == j)
            
          ## Update classifications
          if(!identical(as.numeric(beta[active,l]), rep(0,length(active))))
              classifications[j,l] = 1   
        }
      }
    } else if(L==1){
        for(j in 1:p){
          active = which(groups == j)
          
          ## Update classifications
          ## Update classifications
          if(!identical(as.numeric(beta[active]), rep(0,length(active))))
            classifications[j,1] = 1 
      }
    }
    ## Row names for classifications
    row.names(classifications) = unique(groups)
        
  } else if(family=="negativebinomial"){
    
    ## Fit sparse GAM
    if(!missing(lambda)){
      gam.mod = grpreg.nb(y=y, X=X.tilde, groups=groups, X.test=X.tilde.test, 
                          nb.size=nb.size, penalty=penalty, taper=taper, 
                          nlambda=nlambda, lambda=lambda, max.iter=max.iter, tol=tol)
    } else {
      gam.mod = grpreg.nb(y=y, X=X.tilde, groups=groups, X.test=X.tilde.test, 
                          nb.size=nb.size, penalty=penalty, taper=taper, 
                          nlambda=nlambda, max.iter=max.iter, tol=tol)
    } 
      
    ## Lambdas
    lambda = gam.mod$lambda 
    L = length(lambda)
    ## Estimates of regression coefficients
    beta0 = gam.mod$beta0
    beta = gam.mod$beta
    ## Estimate of loss function
    loss = gam.mod$loss
    ## Predictions for mu.pred
    mu.pred = gam.mod$mu.pred
    ## Classifications
    classifications = gam.mod$classifications
    
  } else if (family=="gamma") {
    
    ## Fit sparse GAM
    if(!missing(lambda)){
      gam.mod = grpreg.gamma(y=y, X=X.tilde, groups=groups, X.test=X.tilde.test, 
                             gamma.shape=gamma.shape, penalty=penalty, taper=taper, 
                             nlambda=nlambda, lambda=lambda, max.iter=max.iter, tol=tol)
    } else {
      gam.mod = grpreg.gamma(y=y, X=X.tilde, groups=groups, X.test=X.tilde.test, 
                             gamma.shape=gamma.shape, penalty=penalty, taper=taper, 
                             nlambda=nlambda, max.iter=max.iter, tol=tol)
    } 
    
    ## Lambdas
    lambda = gam.mod$lambda
    ## Estimates of regression coefficients
    beta0 = gam.mod$beta0
    beta = gam.mod$beta
    ## Estimate of loss function
    loss = gam.mod$loss
    ## Predictions for mu.pred
    mu.pred = gam.mod$mu.pred
    ## Classifications
    classifications = gam.mod$classifications
  
  } 

  ## Compute the function evaluations for the individual functions
  L = length(lambda)
  ## For individual predictions
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
  SFGAM.output <- list(lambda = lambda,
                       f.pred = f.pred,
                       mu.pred = mu.pred,
                       classifications = classifications,
                       beta0 = beta0,
                       beta = beta,
                       loss = loss) 
  # Return list
  return(SFGAM.output)
}