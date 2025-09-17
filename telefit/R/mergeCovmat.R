#' Combine sample covariance matrices from two samples
#'
#' This function combines the sample covariance information from two samples (of 
#' the same phenomena) to return the sample covariance matric of the union of 
#' the two samples.
#' 
#' This function assumes the data is normalized by n (the MLE estimator) 
#' instead of n-1 (the unbiased estimator).
#'
#' @param A.cov.xy sample covariance matrix from the first sample, 'A'
#' @param B.cov.xy sample covariance matrix from the second sample, 'B'
#' @param A.mean.x sample mean from the first sample, 'A'
#' @param B.mean.x sample mean from the second sample, 'B'
#' @param A.mean.y sample mean from the first sample, 'A'
#' @param B.mean.y sample mean from the second sample, 'B'
#' @param A.n sample size from the first sample, 'A'
#' @param B.n sample size from the second sample, 'B'
#'
#' @references Pebay, P., 2008, Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments: Sandia Report.
#' 

mergeCovmat = function(A.cov.xy, B.cov.xy, A.mean.x, A.mean.y, 
                       B.mean.x, B.mean.y, A.n, B.n) {
  
  A.mean.x = matrix(A.mean.x, ncol=1)
  A.mean.y = matrix(A.mean.y, ncol=1)
  B.mean.x = matrix(B.mean.x, ncol=1)
  B.mean.y = matrix(B.mean.y, ncol=1)
  
  N = A.n + B.n

  (A.cov.xy*A.n + B.cov.xy*B.n + 
     (A.mean.x-B.mean.x) %*% t(A.mean.y-B.mean.y) * A.n*B.n/N)/N
}