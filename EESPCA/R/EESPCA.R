#
# EESPCA.R
#
# Implementation of the Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA)
# method.
#
# Author: rob.frost@dartmouth.edu
#

#
# Computes the first sparse principal component of the specified data matrix using 
# the Eigenvectors from Eigenvalues Sparse Principal Component Analysis (EESPCA) method.
#
# Inputs:
#
#   X: n-by-p data matrix for which the first sparse PC will be computed.
#   max.iter: Maximum number of iterations for power iteration method.
#   sparse.threshold: Threshold on loadings used to induce sparsity, i.e, loadings below this
#      value are set to 0. If not specified, defaults to 1/sqrt(p).
#   lambda.diff.threshold: Threshold for exiting the power iteration calculation.
#      If the absolute relative difference in lambda is less than this threshold between     
#      subsequent iterations, the power iteration method is terminated.
#   compute.sparse.lambda: If true, will use the sparse loadings to compute the sparse eigenvalue.
#   sub.mat.max.iter: Maximum iterations for computation of sub-matrix eigenvalues using
#      the power iteration method. To maximize performance, set to 1. Uses the same lambda.diff.threshold.
#   trace: True if debugging messages should be displayed during execution.
# 
# Outputs: List with the following elements:
#
#   v1: The first non-sparse PC as calculated via power iteration.
#   lambda1: The variance of the first non-sparse PC as calculated via power iteration.
#   v1.sparse: First sparse PC.
#   lambda1.sparse: Variance of the first sparse PC. NA if compute.sparse.lambda=F.
#   ratio: Vector of ratios of the sparse to non-sparse PC loadings.
#
eespca = function(X, max.iter=20, sparse.threshold, lambda.diff.threshold=1e-6, 
    compute.sparse.lambda=FALSE, sub.mat.max.iter=5, trace=FALSE) {
    
  # Compute covariance matrix of X (or X^T)
  p = ncol(X)
  n = nrow(X)
  var.names = colnames(X)
  
  # The default threshold, 1/sqrt(p), is the loading value for a normalized vector with all
  # equal values  
  if (missing(sparse.threshold)) {
    sparse.threshold = 1/sqrt(p)
  }
    
  cov.X = cov(X, use="pairwise.complete.obs")    
  
  # Compute principal eigenvalue and eigenvector using power iteration
  power.iter.results = powerIteration(X=cov.X, max.iter=max.iter,
      lambda.diff.threshold=lambda.diff.threshold, trace=trace)
  v1 = power.iter.results$v1
  lambda1 = power.iter.results$lambda1    
  if (trace) {
    message("Finished computing first PC, real loadings: ", paste(head(v1), collapse=", "), "...") 
  }  
  
  # Use sub-matrix eigenvalues to approximate the normed squared principal eigenvector loadings   
  approx.v1.sq = computeApproxNormSquaredEigenvector(cov.X, v1, lambda1, 
      max.iter=sub.mat.max.iter, lambda.diff.threshold=lambda.diff.threshold, trace=trace)                     
    
  # Need to check if all approximate loadings are set to 0; this makes it impossible to get a unit length eigenvector.
  if (sum(approx.v1.sq) == 0) {
    warning("All approximate squared loadings are 0! Not scaling the loadings.")
    v1.adj = v1
    ratio = rep(1, p)
  } else {
    # Compute the ratio of approximate to real eigenvector loadings. These should all be 
    # less than 1. The ratio will be larger for variables with a true loading 
    # given the nature of the eigenvector loading approximation
    ratio = sqrt(approx.v1.sq)/abs(v1)
    
    # Check for NaN elements in ratio and set to 0
    nan.indices = which(is.nan(ratio))
    if (length(nan.indices) > 0) {
      warning("Some eigenvector loading ratios are NaN, setting to 0.")
      ratio[nan.indices] = 0
    }
  
    if (trace) {
      message("Approx loadings: ", paste(sqrt(head(approx.v1.sq)), collapse=", "), "...")
      message("Ratio of approx to real ev loadings: ", paste(head(ratio), collapse=", "), "...")
    }
    
    # Scale the loadings by the ratio
    v1.adj = v1 * ratio
    
    # Renormalize
    v1.adj = v1.adj/sqrt(sum(v1.adj^2))
  }
  
  results = list()   
  results$v1 = v1 
  results$lambda1 = lambda1  
  results$v1.sparse = v1.adj
  results$ratio = ratio
    
  # Which scaled loadings are below the threshold?
  below.threshold = which(abs(v1.adj) < sparse.threshold)
  
  if (trace) {
    message("Scaled loadings: ", paste(head(v1.adj), collapse=", "), "...")
    message("Number below threshold: ", length(below.threshold), ", threshold: ", sparse.threshold)
  }

  # Set any adjusted loadings that are below the specified sparse.threshold to 0. 
  if (length(below.threshold) > 0) {
    results$v1.sparse[below.threshold] = 0
    l2 = sqrt(sum(results$v1.sparse^2))
    if (l2 > 0) {
     results$v1.sparse = results$v1.sparse/l2
    }
  }
  
  # If requested, compute lambda1 for sparse v1
  if (compute.sparse.lambda) {
    lambda1.sparse = t(results$v1.sparse) %*% cov.X %*% results$v1.sparse
    results$lambda1.sparse = lambda1.sparse    
  }

  # Set names to var names
  names(results$v1) = var.names
  names(results$v1.sparse) = var.names
  names(results$ratio) = var.names  
    
    
  return (results)
}

#
# Performs cross-validation of EESPCA to determine the optimal 
# sparsity threshold. Selection is based on the minimization of reconstruction error.
# Based on cv logic in PMA package.
#
eespcaCV = function(X, max.iter=20, 
    sparse.threshold.values,
    nfolds = 5, # number of cross-validation folds
    lambda.diff.threshold=1e-6, 
    compute.sparse.lambda=FALSE, sub.mat.max.iter=5, trace=FALSE) {
  
  p = ncol(X)
  n = nrow(X)
  if (missing(sparse.threshold.values)) {
    stop("Must specify sparse.threshold.values!")
  }    
  if (length(sparse.threshold.values) < 2) {
    stop("Must specify at least 2 potential sparse threshold values!")
  }
  if (nfolds < 2) { 
    stop("Must run at least 2 cross-validation folds.")
  }
  percentRemove = min(0.25, 1/nfolds)
  missing = is.na(X)    
  errs = matrix(NA, nrow = nfolds, ncol = length(sparse.threshold.values))
  nonzeros = matrix(NA, nrow = nfolds, ncol = length(sparse.threshold.values))
  rands = matrix(runif(n * p), ncol = p)
  
  falsepos = matrix(NA, nrow = nfolds, ncol = length(sparse.threshold.values))  
  falseneg = matrix(NA, nrow = nfolds, ncol = length(sparse.threshold.values))      
  
  for (i in 1:nfolds) {
    rm = ((i - 1) * percentRemove < rands) & (rands < i * 
          percentRemove)
    Xrm = X
    Xrm[rm] = NA
    for (j in 1:length(sparse.threshold.values)) {
      sparse.threshold = sparse.threshold.values[j]
      eespca.results = eespca(X=X, max.iter=max.iter,
            sparse.threshold = sparse.threshold,
            lambda.diff.threshold=lambda.diff.threshold,          
            compute.sparse.lambda=compute.sparse.lambda, 
            sub.mat.max.iter=sub.mat.max.iter, trace=trace)
      sparse.v = eespca.results$v1.sparse
      X.resid = computeResidualMatrix(X,sparse.v)
      errs[i,j] = sum((X.resid[rm & !missing])^2)
      nonzeros[i, j] = sum(sparse.v != 0)
      zero.vals = which(sparse.v == 0)      
      nonzero.vals = which(sparse.v != 0)            
    }
  }
  err.means = apply(errs, 2, mean)
  err.sds = apply(errs, 2, sd)/sqrt(nfolds)
  nonzeros.mean = apply(nonzeros, 2, mean)  
  min.err.index = which.min(err.means)
  min.err = err.means[min.err.index]
  min.err.sd = err.sds[min.err.index]
  best.sparsity = sparse.threshold.values[min.err.index]
  min.err.1se.index = max(which(err.means < min.err + min.err.sd))
  best.sparsity.1se = sparse.threshold.values[min.err.1se.index]
  object = list(cv = err.means, 
      cv.error = err.sds, 
      best.sparsity = best.sparsity,
      best.sparsity.1se = best.sparsity.1se,                  
      nonzerovs = nonzeros.mean, 
      sparse.threshold.values = sparse.threshold.values,
      nfolds = nfolds)
  
  return(object)
}

#
# Computes multiple sparse principal components of the specified data matrix via sequential application of
# the "Eigenvector-from-Eigenvalue Sparse PCA" (EESPCA) method. After computing the first sparse PC, 
# subsequent PCs are computing by repeatedly applying the eespca() method to the residual matrix formed
# by subtracting the reconstruction of X from the original X. Multiple sparse PCs are not guaranteed to be
# orthogonal. 
#
# Note that the accuracy of the sparse approximation declines substantially for PCs with very small
# variances. To avoid this issue, k should not be set higher than the number of statistically significant PCs
# according to a Tracey-Widom test.
#
# Inputs:
#
#   X: n-by-p data matrix for which the sparse PCs will be computed.
#   k: The number of sparse PCs to compute. The specified k must be 2 or greater (for k=1, use
#      the eespca() method). A check is made that k is not greater than the maximum theoretical
#      rank of X but, for performance reasons, a check is NOT made that
#      k is less than or equal to the actual rank of X. 
#   max.iter: Maximum number of iterations for power iteration method.
#   sparse.threshold: Threshold on loadings used to induce sparsity, i.e, loadings below this
#      value are set to 0. If not specified, defaults to 1/sqrt(p).
#   lambda.diff.threshold: Threshold for exiting the power iteration calculation.
#      If the absolute relative difference in lambda is less than this threshold between     
#      subsequent iterations, the power iteration method is terminated.
#   compute.sparse.lambda: If true, will use the sparse loadings to compute the sparse eigenvalues.
#   sub.mat.max.iter: Maximum iterations for computation of sub-matrix eigenvalues using
#      the power iteration method. To maximize performance, set to 1. Uses the same lambda.diff.threshold.
#   trace: True if debugging messages should be displayed during execution.
# 
# Outputs: List with the following elements:
#
#   V: Matrix of sparse loadings for the first k PCs. 
#   lambdas: Vector of variances of the first k sparse PCs. Will be NA unless 
#      compute.sparse.lambda=T.
#
eespcaForK = function(X, k=2, max.iter=20, sparse.threshold, lambda.diff.threshold=1e-6, 
    compute.sparse.lambda=FALSE, sub.mat.max.iter=5, trace=FALSE) {
  
  if (k < 2) {
    stop("k must be 2 or greater! To compute just the first sparse PC, use eespca().")
  }
  
  p = ncol(X)
  n= nrow(X)
  if (k > min(c(p,n))) {
    stop("k cannot be greater than the max theoretical rank of X!")
  }
  
  results = list()
  results$V = matrix(0, nrow=p, ncol=k)
  colnames(results$V) = paste("PC", 1:k, sep="_")
  rownames(results$V) = colnames(X)
  results$lambdas = rep(NA, k)
  
  for (i in 1:k) {
    if (trace) {
      message("Computing sparse loadings for PC ", i)
    }
    # Get the first sparse PC
    eespca.results = eespca(X=X, max.iter=max.iter, 
        sparse.threshold=sparse.threshold,
        lambda.diff.threshold=lambda.diff.threshold, 
        compute.sparse.lambda=compute.sparse.lambda, 
        sub.mat.max.iter=sub.mat.max.iter, trace=trace)
    
    # Save the sparse loadings and PC variance
    results$V[,i] = eespca.results$v1.sparse
    if (compute.sparse.lambda) {
      results$lambdas[i] = eespca.results$lambda1.sparse
    }
    
    # If there are more to compute, need to update X with residual matrix
    if (k > i) {
      X = computeResidualMatrix(X, eespca.results$v1.sparse)
    }
  }
  
  return (results)
  
}

#
# Implementation of power iteration to compute the first eigenvector/eigenvalue.
# Includes support for truncating to k non-zero entries on each iteration, i.e., 
# the TPower sparse PCA method of Yuan & Zhang (JMLR, 2013).
#
# Inputs:
# 
#   X: Matrix for which the principal eigenvector/value will be computed.
#   k: Optional truncation parameter. Must be between 1 and p. If specified,
#      the estimated eigenvector will have all loadings except for those with the k
#      largest absolute values set to 0 each iteration.
#   v1.init: Initial value of PC loadings. If not specified, will be initialized to
#      rep(1/sqrt(p), p).
#   max.iter: The maximum number of iterations
#   lambda.diff.threshold: If the change in the estimated eigenvalue is less than this threshold
#      between iterations, the algorithm will exit.
#   trace: If true, debugging messages will be output.
#   
# Outputs: List with the following elements:
#
#   v1: Estimated principal eigenvector.
#   lambda: Estimated principal eigenvalue.
#   num.iter: Number of iterations completed.
#
powerIteration = function(X, k, v1.init, max.iter=10, lambda.diff.threshold=1e-6, trace=FALSE) {
  
  p = ncol(X)
  
  # If not specified, initialize principal eigenvector to unit vector with equal values
  if (missing(v1.init)) {
    v1 = rep(1/sqrt(p), p)        
  } else {
    v1 = v1.init
  }
  
  if (missing(k)) {
    k = p
  } else if (k < 1 | k > p) {
    stop("If specified, k must be between 1 and ", p)
  }
  
  # Initialize principal eigenvalue
  lambda1 = NA
  
  # Update using modified power iteration
  for (i in 1:max.iter) {
    
    if (trace) {
      message("Iteration ", i)
    }
    
    # Standard power iteration update of v1 and lambda1
    v1 = X %*% v1
    v1 = as.vector(v1/sqrt(sum(v1^2)))
    
    if (k != p) {
      # Find k loadings with largest absolute values; set all others to 0
      abs.v1 = abs(v1)
      vars.to.truncate = order(abs.v1, decreasing=F)[1:(p-k)]     
      v1[vars.to.truncate] = 0
      
      # Renormalize the eigenvector
      v1 = as.vector(v1/sqrt(sum(v1^2)))
    }
    
    # Compute principal eigenvalue
    lambda1.est = t(v1) %*% X %*% v1
    
#    if (trace) {
#      message("Updated v1 from power iteration: ", paste(v1, collapse=", "))
#      message("Updated lambda1 from power iteration: ", lambda1.est)      
#    }    
    
    # Check if the difference is less than the threshold, exit
    if (i > 1) {
      if (abs(lambda1.est - lambda1)/lambda1.est <= lambda.diff.threshold){
        if (trace) {
          message("Lambda1 diff less than threshold at iteration ", i, ", exiting.")
        }
        break
      }
    }    
    lambda1=lambda1.est
  }    
  
  results = list()
  results$v1 = as.vector(v1)
  results$lambda1 = lambda1
  results$num.iter = i
  
  return (results)
}

#
# Approximates the normed squared eigenvector loadings using a simplified version of the formula
# associating normed squared eigenvector loadings with the eigenvalues of the full matrix 
# and sub-matrices. 
#
# Inputs:
#   cov.X: Covariance matrix.
#   v1: Principal eigenvector of cov.X, i.e, the loadings of the first PC.
#   lambda1: Largest eigenvalue of cov.X.
#   max.iter: Maximum number of iterations for power iteration method when computing 
#      sub-matrix eigenvalues. See powerIteration().
#   lambda.diff.threshold: Threshold for exiting the power iteration calculation. 
#      See powerIteration().
#   trace: True if debugging messages should be displayed during execution.
#
# Output: Vector of approximate normed squared eigenvector loadings.
#
computeApproxNormSquaredEigenvector = function(cov.X, v1, lambda1, max.iter=5,
    lambda.diff.threshold=1e-6, trace=FALSE) {
  p = nrow(cov.X)  
  sub.evals = rep(0, p)
  
  # Compute all of the approximate sub-matrix principal eigenvalues  
  for (i in 1:p) {  
    
    if (trace) {
      if ((i %% 50) == 0) {
        message("Computing sub-matrix eigenvalues for variable ", i, " of ", p)
      }
    }
    
    # Get the covariance submatrix      
    indices = which(!(1:p %in% i))     
    sub.cov.X = cov.X[indices,indices]
    
    # Get the approximate eigenvalue using either power iteration
    v1.sub = v1[indices]
    sub.evals[i] = powerIteration(X=sub.cov.X, v1.init=v1.sub, 
        lambda.diff.threshold=lambda.diff.threshold, max.iter=max.iter)$lambda1
  }
  
  if (trace) {
    message("Lambda1: ", lambda1, ", submatrix eigenvalues: ",  
        paste(head(sub.evals), collapse=", "), "...")
    ratios = sub.evals/lambda1
    message("Ratios : ", paste(head(ratios), collapse=", "), "...")    
    
  }
  
  # Use the approximate sub-matrix eigenvalues to approximate the squared principal 
  # eigenvector loadings
  approx.v1.sq = 	1 - sub.evals/lambda1
  
  # if any are negative, set to 0
  approx.v1.sq[which(approx.v1.sq < 0)] = 0
  
  if (trace) {
    message("Approx PC loadings squared: ",  paste(head(approx.v1.sq), collapse=", "), "...")     
  }
  
  return (approx.v1.sq)
}

# 
# Creates a reduced rank reconstruction of X using the PC loadings in V.
#
# Inputs:
#
#   X: An n-by-p data matrix whose top k principal components are contained the 
#      p-by-k matrix V. 
#   V: A p-by-k matrix containing the loadings for the top k principal components of X.
#   center: If true (the default), X will be mean-centered before the reduced rank 
#      reconstruction is computed. If the PCs in V were computed via SVD on 
#      a mean-centered matrix or via eigen-decomposition of the sample covariance matrix, 
#      this should be set to true.
#
# Output: Reduced rank reconstruction of X.
#
reconstruct = function(X,V,center=TRUE) {
  if (center) {
    X = scale(X, center=TRUE, scale=FALSE)
  }
  X.recon = X %*% V %*% t(V) 
  return (X.recon)
}

#
# Utility function for computing the residual matrix formed by 
# subtracting from X a reduced rank approximation of matrix X generated from 
# the top k principal components contained in matrix V.
#
# Inputs:
#
#   X: An n-by-p data matrix whose top k principal components are contained the 
#      p-by-k matrix V.
#   V: A p-by-k matrix containing the loadings for the top k principal components of X.
#   center: If true (the default), X will be mean-centered before the reduced rank 
#      reconstruction is computed. If the PCs in V were computed via SVD on 
#      a mean-centered matrix or via eigen-decomposition of the sample covariance matrix, 
#      this should be set to true.
#
# Output: Computed residual matrix.
#
computeResidualMatrix = function(X,V,center=TRUE) {
  if (center) {
    X = scale(X, center=TRUE, scale=FALSE)    
  }
  X.residual = X-reconstruct(X,V, center=F) # don't need to center again
  return (X.residual)
}

#
# Calculates the squared Frobenius norm of the residual matrix.
#
# Inputs:
#
#   X: An n-by-p data matrix whose top k principal components are contained the 
#      p-by-k matrix V.
#   V: A p-by-k matrix containing the loadings for the top k principal components of X.
#   center: If true (the default), X will be mean-centered before the reduced rank 
#      reconstruction is computed. If the PCs in V were computed via SVD on 
#      a mean-centered matrix or via eigen-decomposition of the sample covariance matrix, 
#      this should be set to true.
#
# Output: Squared Frobenius norm of residual matrix.
#
reconstructionError = function(X,V,center=TRUE) {
  X.resid = computeResidualMatrix(X,V,center=center)
  return (sum((X.resid)^2))
}

