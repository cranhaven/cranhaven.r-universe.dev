#' Estimate sigma
#'
#' This function estimates the standard deviation sigma of the noise of the model where the data are generated from a signal of rank k corrupted by homoscedastic Gaussian noise. 
#' Two estimators are implemented. The first one, named LN, is asymptotically unbiased for sigma in the asymptotic framework where both the number of rows and the number of columns are fixed while the noise variance tends to zero (Low Noise).
#' It is calculated by computing the residuals sum of squares (using the truncated SVD at order k as an estimator) divided by the number of data minus the number of estimated parameters. Thus, it requires as an input the rank k.
#' The second one, MAD (mean absolute deviation) is a robust estimator defined as the ratio of the median of the singular values of X over the square root of the median of the Marcenko-Pastur distribution. It can  be useful when the signal can be considered of low-rank (the rank is very small in comparison to the matrix size).
#' 
#' @param X a data frame or a matrix with numeric entries
#' @param method LN for the low noise asymptotic estimate (it requires to specify the rank k) or MAD for mean absolute deviation
#' @param k integer specifying the rank of the signal only if method = "LN". By default k is estimated using the estim_ncp function of the FactoMineR package 
#' @param  center boolean, to center the data. By default "TRUE". 
#' @return sigma the estimated value
#' @details In the low noise (LN) asymptotic framework,  the estimator requires providing the rank k. Different methods are available in the litterature and if by default the user does not provide any value, we use of the function estim_ncp of the FactoMineR package with the option GCV (see ?estim_ncp).
#' @references Josse, J & Husson, F. (2012). Selecting the number of components in principal component analysis using cross-validation approximations. Computational Statistics & Data Analysis, 6 (56).
#' @references Gavish, M & Donoho, D. L. Optimal Shrinkage of Singular Values.
#' @references Gavish, M & Donoho, D. L. (2014). The Optimal Hard Threshold for Singular Values is 4/sqrt(3). IEEE Transactions on Information Theory, 60 (8), 5040-5053.
#' @references Josse, J. & Husson, F. (2011). Selecting the number of components in PCA using cross-validation approximations.Computational Statististics and Data Analysis. 56 (6), pp. 1869-1879.
#' @seealso \code{\link{estim_ncp}}
#' @seealso \code{\link{LRsim}}
#' @examples 
#' Xsim <-  LRsim(100, 30, 2, 4)
#' res.sig <- estim_sigma(Xsim$X, k = 2)


## estim_sigma
estim_sigma <- function(X,
                        k = NA,
                        method = c("LN", "MAD"),
                        center = "TRUE") {
  
  method <- match.arg(method, c("LN","MAD","ln","mad", "Ln","Mad"), several.ok = T)[1]
  method <- tolower(method)
  
  if(inherits(X, "data.frame")){
    X <- as.matrix(X)
  }
  
  if(sum(sapply(X, is.numeric)) < ncol(X)){
    stop("all the variables must be numeric")
  }
  
  if(center == "TRUE"){
    X <- scale(X,scale=F)
  }
  
  n = nrow(X) 
  p = ncol(X)
  svdX = svd(X)
  
  # infer unspecified choices
  if(method == "ln" & is.na(k)){
    warning("Since you did not specify k, k was estimated using the FactoMineR estim_ncp function")
    k <- estim_ncp(X, scale = F)$ncp
    print(paste("k = ", k))    
  } 
  
  if(center == "TRUE") {
    N <- (n-1)
  } else {
    N <- n
  }
  
  if((k >= min(N, p))&(method == "ln")){   
    stop("the number k specified has to be smaller than the minimum of the number of rows or columns")
  }
  
  # Begin
  if (method == "ln"){
    
    if(k == 0){
      sigma = sqrt(sum(svdX$d^2)/(N*p))
    } else {                          
      sigma <- sqrt(sum(svdX$d[-c(1:k)]^2)/(N*p  - N*k - p*k + k^2))
    }
  } else {
    beta <- min(n,p)/max(n,p)
    lambdastar <- sqrt( 2*(beta + 1) + 8*beta/((beta + 1 + (sqrt(beta^2 + 14*beta + 1)))))
    wbstar <- 0.56*beta^3 - 0.95*beta^2 + 1.82*beta + 1.43
    sigma <-  median(svdX$d)/(sqrt(max(n,p)) *(lambdastar/wbstar))
  }
  
  return(sigma)
}