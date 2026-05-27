#
# OtherSPCAMethods.R
#
# OtherSPCAMethods.R: Implementation of TPower and cross-validation support for
# both TPower and rifle using Witten et al. PMA cross-validation approach.
#
# Author: rob.frost@dartmouth.edu
#

#
# Implementation of Yuan and Zhang TPower method (JMLR, 2013).
#
# Inputs:
# 
#   X: Matrix for which the principal eigenvector/value will be computed, i.e., the 
#      the sample covariance matrix for PCA.
#   k: Truncation parameter. Must be between 1 and p. If specified,
#      the estimated eigenvector will have all loadings except for those with the k
#      largest absolute values set to 0 each iteration.
#   v1.init: Initial value of PC loadings. If not specified, will be initialized
#      using powerIteration() with k=p.
#   max.iter: The maximum number of iterations
#   lambda.diff.threshold: If the change in the estimated eigenvalue is less than this threshold
#      between iterations, the algorithm will exit.
#   trace: If true, debugging messages will be output.
#   
# Outputs: Estimated sparse principal eigenvector.
#
tpower = function(X, k, v1.init, max.iter=10, lambda.diff.threshold=1e-6, trace=FALSE) {
  p = nrow(X)
  if (missing(v1.init)) {
    v1.init = powerIteration(X=X, k=p, max.iter=10, 
        lambda.diff.threshold=lambda.diff.threshold, trace=trace)$v1
  }
  power.iter.out = powerIteration(X=X, k=k, v1.init=v1.init, max.iter=max.iter, 
      lambda.diff.threshold=lambda.diff.threshold, trace=trace)
  return (power.iter.out$v1)     
}

#
# Sparsity parameter selection for PCA-based TPower using the cross-validation approach of Witten et al.
#
# Inputs:
# 
#   X: n-by-p data matrix being evaluated via PCA.
#   k.values: Set of truncation parameter values to evaluate via cross-validation. 
#      Values must be between 1 and p. 
#   nfolds: Number of folds for cross-validation.
#   
# Outputs: k value that generated the smallest cross-validation error.
#
tpowerPCACV = function(X, k.values, nfolds=5) {
  tpower.cv = sPCA.cv(X, sparsity.params=k.values, nfolds=nfolds, method = "tpower")
  return (tpower.cv$best.sparsity)
}

#
# Sparsity parameter selection for PCA-based rifle using the cross-validation approach of Witten et al.
#
# Inputs:
# 
#   X: n-by-p data matrix being evaluated via PCA.
#   k.values: Set of truncation parameter values to evaluate via cross-validation. 
#      Values must be between 1 and p. 
#   nfolds: Number of folds for cross-validation.
#   
# Outputs: k value that generated the smallest cross-validation error.
#
riflePCACV = function(X, k.values, nfolds=5) {
  rifle.cv = sPCA.cv(X, sparsity.params=k.values, nfolds=nfolds, method="rifle")  
  return (rifle.cv$best.sparsity)
}

#
# Computes the initial eigenvector for the rifle method using the initial.convex() 
# method with lambda=sqrt(log(p)/n) and K=1.
# 
# Inputs:
#
#   X: n-by-p data matrix being evaluated via PCA.
#
# Output: Initial eigenvector.
#
rifleInit = function(X) {
  p = ncol(X)
  n = nrow(X)
  cov.X = cov(X)
  lambda = sqrt(log(p)/n)
  init.cov.out = initial.convex(A=cov.X, B=diag(p), lambda=lambda, K=1)
  return (eigen(init.cov.out$Pi)$vectors[,1])
}

#
# Performs cross-validation of sparse PCA methods to determine the optimal 
# sparsity parameter value. Selection is based on the minimization of reconstruction error.
# Based on cv logic in PMA package.
#
sPCA.cv = function(x, # matrix for sparse PCA analysis
    sparsity.params, # vector of candidate values for the sparsity parameter
    nfolds = 5, # number of cross-validation folds
    method = "rifle" # sparse PCA method to use; must be one of "rifle" or "tpower" 
) {
  if (missing(x)) {
    stop("Must specify x!")
  }
  p = ncol(x)
  n = nrow(x)
  if (missing(sparsity.params)) {
    stop("Must specify sparsity.params!")
  }    
  if (length(sparsity.params) < 2) {
    stop("Must specify at least 2 potential sparsity param values!")
  }
  if (nfolds < 2) { 
    stop("Must run at least 2 cross-validation folds.")
  }
  supported.methods =c("rifle", "tpower")
  if (!(method %in% supported.methods)) {
    stop("Method ", method, " not one of ", paste(supported.methods, collapse=", "))
  }
  percentRemove = min(0.25, 1/nfolds)
  missing = is.na(x)    
  errs = matrix(NA, nrow = nfolds, ncol = length(sparsity.params))
  nonzeros = matrix(NA, nrow = nfolds, ncol = length(sparsity.params))
  rands = matrix(runif(n * p), ncol = p)
  
  if (method=="tpower") {
    # Use power iteration to generate initial eigenvector
    v.init = powerIteration(X=cov(x))$v1
  } else if (method=="rifle") {
    # Use initial.convex method with recommended lambda and K values to generated
    # initial eigenvector
    v.init = rifleInit(x)
  }
  
  for (i in 1:nfolds) {
    rm = ((i - 1) * percentRemove < rands) & (rands < i * 
          percentRemove)
    xrm = x
    xrm[rm] = NA
    for (j in 1:length(sparsity.params)) {
      sparsity.param = sparsity.params[j]
      sparse.v = NA
      cov.xrm = cov(xrm, use="pairwise.complete.obs")
      if (method == "rifle") {
        sparse.v = rifle(A=cov.xrm, B=diag(p), init=v.init, k=sparsity.param)
      } else if (method == "tpower") {
        sparse.v = powerIteration(X=cov.xrm, max.iter=100, k=sparsity.param, v1.init=v.init)$v1
#      } else if (method == "spc") {
#        spc.out = SPC(xrm, sumabsv=sparsity.param, niter=10, trace=F)
#        sparse.v = spc.out$v
      } else {
        stop("Method ", method, " not supported!")
      }
      X.resid = computeResidualMatrix(x,sparse.v)
      errs[i,j] = sum((X.resid[rm & !missing])^2)
      nonzeros[i, j] = sum(sparse.v != 0)
    }
  }
  err.means = apply(errs, 2, mean)
  err.sds = apply(errs, 2, sd)/sqrt(nfolds)
  nonzeros.mean = apply(nonzeros, 2, mean)    
  best.sparsity = sparsity.params[which.min(err.means)]
  best.sparsity.1se = sparsity.params[min(which(err.means < min(err.means) + 
                  err.sds[which.min(err.means)]))]
  object = list(cv = err.means, 
      cv.error = err.sds, 
      best.sparsity = best.sparsity,
      best.sparsity.1se = best.sparsity.1se,                  
      nonzerovs = nonzeros.mean, 
      sparsity.params = sparsity.params,
      nfolds = nfolds)
  return(object)
}




