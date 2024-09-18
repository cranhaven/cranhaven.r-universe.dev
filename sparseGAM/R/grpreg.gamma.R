#################################################
#################################################
## FUNCTION FOR IMPLEMENTING GROUP-REGULARIZED ##
## GAMMA REGRESSION MODELS                     ##
#################################################
#################################################

# This function implements group-regularized gamma regression models. This program
# employs the least squares approximation approach of Wang and Leng (2007) and
# hence does not allow for the number of columns of X to be greater than sample size.
# The size parameter alpha is automatically computed using the MLE of the non-penalized
# problem.

# INPUTS:
# y = n x 1 vector of responses (y_1, ...., y_n) for training data
# X = n x p design matrix for training data, where J is the total number of coefficients
# X.test = n.test x p design matrix for test data. If missing, then the program sets X.test=X
#          and computes in-sample predictions on training data X. X.test must have the same 
#          number of columns as X, but not necessarily the same number of rows.
# groups = p x 1 vector of group indices or factor level names for each of the individual covariates.
# gamma.shape = known shape parameter nu in Gamma(mu_i, nu) distribution for the responses.
#           Default is gamma.shape=1.
# penalty = group regularization method to apply. Options are "gLASSO" for group lasso,
#           "gSCAD" for group SCAD, and "gMCP" for group MCP. Negative binomial regression for
#           the SSGL penalty is available in the stand-alone SSGL function.
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
# beta0 = L x 1 vector of intercept estimates. The lth entry of beta0 corresponds to the lth entry in
#         our lambda grid.
# beta = p x L matrix of regression coefficient estimates. The lth column of beta corresponds to the
#        lth entry in our lambda grid.
# mu.pred = n.test x L matrix of predicted mean response values based on test data in X.test. If
#           X.test was left blank or X.test=X, then in-sample predictions on X.train are returned.
# classifications = G x L matrix of group classifications, where G is the number of groups. "1" indicates
#                   that the group was selected and "0" indicates that the group was not selected. 
#                   The lth column in this matrix corresponds to the lth entry in our lambda grid.
# loss = L x 1 vector of negative log-likelihoods for each fit. The lth entry in loss corresponds to 
#        the lth entry in our lambda grid.

grpreg.gamma = function(y, X, X.test, groups, gamma.shape=1, penalty=c("gLASSO","gSCAD","gMCP"),
                        weights, taper, nlambda=100, lambda, max.iter=10000, tol=1e-4) {
  
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
  
  ## Check that J is less than or equal to n
  if(J > n) {
    stop("For group-regularized gamma regression, we require the total
          number of covariates to be less than or equal to sample size. 
          Consider reducing the number of covariates.")
  }
  
  ## Set test data as training data if test data not provided.
  if(missing(X.test)) X.test = X
  n.test = dim(X.test)[1]
  
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
  ## Check that X and X.test have the same number of columns
  if(dim(X.test)[2] != J)
    stop("X and X.test should have the same number of columns.")
  
  ## Check that taper parameter is greater than 2 for gSCAD and greater than 1 for gMCP
  if(penalty=="gSCAD"){
    if(taper<=2)
      stop("The taper parameter must be greater than 2 for the group SCAD penalty.")
  }
  if(penalty=="gMCP"){
    if(taper<=1)
      stop("The taper parameter must be greater than 1 for the group MCP penalty.")
  }
  
  ## Check that gamma.shape is strictly positive
  if (gamma.shape<=0)
    stop("Shape parameter for gamma density must be strictly positive.")
  ## Check that the response variables are strictly positive
  if(any(y<=0))
    stop("All responses y must be strictly positive.")
  ## Check that weights are all greater than or equal to 0
  if(!missing(weights)){
    if(any(weights<0))
      stop("All group-specific weights should be nonnegative.")
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
  
  ## If lambda is not specified
  if(missing(lambda)) {
    max.lambda = 1
    eps = 0.05
    
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
  
  # Find MLE of regression coefficients
  glm.mod = stats::glm(y~X, family=stats::Gamma(link="log"))
  gamma.MLE = glm.mod$coefficients # MLE of regression coefficients 
  
  # Find asymptotic variance-covariances
  MLE.hessian = stats::vcov(glm.mod, type="hessian")
  
  # Square root of Sigma.inv
  sqrt.Sigma.inv = pracma::sqrtm(solve(MLE.hessian))$B
  
  # New parameter y
  y.check = sqrt.Sigma.inv %*% gamma.MLE
  
  # Fit group regression. Do not penalize the intercept
  groups.MLE = group.numbers + 1
  groups.MLE = c(1, groups.MLE)
  weights.MLE = c(0, weights)
  
  ## Solve for beta
  if(penalty=="gLASSO"){
      ## LSA approach
      if(nlambda > 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grLasso", 
                                     nlambda=nlambda, lambda=lambda, eps=tol, max.iter=max.iter,
                                     group.multiplier = weights.MLE)
      } else if (nlambda == 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grLasso", 
                                     lambda=lambda, eps=tol, max.iter=max.iter,
                                     group.multiplier = weights.MLE)
      }
    
  } else if(penalty=="gSCAD"){
    
      ## LSA approach
      if(nlambda > 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grSCAD", 
                                     nlambda=nlambda, lambda=lambda, eps=tol, max.iter=max.iter,
                                     gamma=taper, group.multiplier = weights.MLE)
      } else if (nlambda == 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grSCAD", 
                                     lambda=lambda, eps=tol, max.iter=max.iter,
                                     gamma=taper, group.multiplier = weights.MLE)
      }
    
  } else if(penalty=="gMCP"){

      ## LSA approach
      if(nlambda > 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grMCP", 
                                     nlambda=nlambda, lambda=lambda, eps=tol, max.iter=max.iter,
                                     gamma=taper, group.multiplier = weights.MLE)
      } else if (nlambda == 1){
        gamma.group = grpreg::grpreg(X=sqrt.Sigma.inv, y=y.check, group=groups.MLE, penalty="grMCP", 
                                     lambda=lambda, eps=tol, max.iter=max.iter,
                                     gamma=taper, group.multiplier = weights.MLE)
      }
  }
  
  ## Estimated intercepts
  beta0 = as.numeric(gamma.group$beta[2,]) 
  
  ## Estimated regression coefficients
  beta = as.matrix(gamma.group$beta[-c(1,2),])
  colnames(beta) = NULL
  rownames(beta) = groups
  
  ## Lambda
  lambda = lambda
  L = length(lambda)
  
  ## Compute loss function (i.e. the negative log-likelihood
  loss.ind = matrix(0, n, L) # To compute loss for a single observation
  
  if(L>1){
    for(l in 1:L){
      for(m in 1:n){
        eta.pred.ind = t(as.matrix(X[m,])) %*% beta[,l] + beta0[l]
        mu.pred.ind = exp(eta.pred.ind)
        loss.ind[m,l] = lgamma(gamma.shape)+gamma.shape*log(gamma.shape/mu.pred.ind)+(gamma.shape/mu.pred.ind)*y[m]-(gamma.shape-1)*log(y[m])
      }
    }
    loss = colSums(loss.ind)
  } else if(L==1){
      for(m in 1:n){
        eta.pred.ind = t(as.matrix(X[m,])) %*% beta + beta0
        mu.pred.ind = exp(eta.pred.ind)
        loss.ind[m,1] = lgamma(gamma.shape)+gamma.shape*log(gamma.shape/mu.pred.ind)+(gamma.shape/mu.pred.ind)*y[m]-(gamma.shape-1)*log(y[m])
      }  
    loss = sum(loss.ind)
  }
  
  ## Compute predictions matrix on test data
  mu.pred = matrix(0, n.test, L) # To compute predictions on test set
  if(L>1){
    for(l in 1:L){
      mu.pred[,l] = exp(rep(beta0[l],dim(X.test)[1])+X.test%*%beta[,l])
    }
  } else if(L==1){
    mu.pred[,1] = exp(rep(beta0,dim(X.test)[1])+X.test%*%beta)
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
  } else if(L==1){
    for (g in 1:G) {
      active = which(group.numbers == g)
      
      ## Update classifications
      if(!identical(as.numeric(beta[active]), rep(0,length(active))))
        classifications[g,1] = 1   
    }
  }
  
  ## Row names for classifications
  row.names(classifications) = unique(groups)
  
  ## Return a list
 grpreg.gamma.output <- list(lambda=lambda,
                             beta0=beta0,
                             beta=beta,
                             mu.pred=mu.pred,
                             classifications = classifications,
                             loss = loss)
  # Return list
  return(grpreg.gamma.output)
}