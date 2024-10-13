RAP <-
function(X, y, r=.95, eps=.01, l0=.1, Approx=FALSE){
  # function to initialize RAP object
  #
  # INPUT:
  #     - X: matrix of predictors for burn in (can be just 1 row)
  #     - y: response for burn in (can be just 1 entry)
  #     - r: fixed forgetting factor
  #     - eps: fixed step size in SGD tuning of reg parameter
  #     - l0: initial regularization parameter
  #     - Approx: indicates if approximate method should be used
  #
  #
    
  RAPobj = structure(list(r=r, 
                          eps=eps, 
                          regParam=l0, # current regularization parameter
                          w = 1, # weight for fixed forgetting factor
                          xbar = matrix(rep(0, ncol(X)+1), nrow=1), # forgetting factor mean
                          l1Track = c(l0), # vector to track values for regularization parameter
                          beta = matrix( lassoshooting(XtX = t(X)%*%X, Xty = t(X)%*%y, lambda = l0)$coef, ncol=ncol(X) ), # initial estimate of regression coefficients
                          St = 0, 
                          Approx=Approx), class="RAP")
  
  if (nrow(X)==1){
    RAPobj$St = diag(ncol(X)+1) # start with a diagonal approximation
  } else {
    RAPobj$St = cov(cbind(y, X))
  }
  
  # note that the covariance, St, is the joint covariance across response and predictors!
  
  return(RAPobj)
}
