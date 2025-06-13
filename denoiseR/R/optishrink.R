#' Optimal Shrinkage
#'
#' This function estimates a low-rank signal from Gaussian noisy data using the Optimal Shrinker of the singular values. 
#' More precisely, in an asymptotic framework, the estimator which applies a non-linear transformation of the singular values is the closest to the underlying signal in term of mean squared error. 
#' Two asymptotic frameworks are considered: one where both the number of rows and the number of columns are fixed while the noise variance tends to zero (Low Noise) and one where both the number of rows and of columns tend to infinity (ASYMPT) while the rank of the matrix stays fixed. In this latter, an optimal shrinker is given according to different norm losses (Frobenius, Operator, Nuclear). 
#' 
#' @param X a data frame or a matrix with numeric entries
#' @param sigma integer, standard deviation of the Gaussian noise. By default sigma is estimated using the estim_sigma function 
#' @param method asymptotic framework used either low noise LN or ASYMPT. By default ASYMPT
#' @param k integer, specifying the rank of the signal only if method = "LN". By default k is estimated using the estim_ncp function of the FactoMineR package 
#' @param loss by default Frobenius only if method = "ASYMPT"
#' @param  center boolean, to center the data. By default "TRUE"
#' @return mu.hat the estimator of the signal
#' @return nb.eigen the number of non-zero singular values
#' @return singval the singular values of the estimator
#' @return low.rank the results of the SVD of the estimator
#' @details In the low noise (LN) asymptotic framework, the estimator applies the following transformation on the first k singular values dl  = dl *(dl^2-sigma^2)/dl^2. Thus, it requires providing both the rank k and a value for sigma.  Concerning the rank k, different methods are available in the litterature and if by default the user does not provide any value, we use of the function estim_ncp of the FactoMineR package with the option GCV (see ?estim_ncp).
#' The other asymptotic framework (ASYMPT) only requires providing sigma. optishrink automatically estimates the rank of the signal. Its value is given in the output nb.eigen corresponding to the number of non-zero eigenvalues. 
#' The estimated low rank matrix is given in the output mu.hat.
#' @references Gavish, M & Donoho, D. L. (2014). Optimal Shrinkage of Singular Values.
#' @references Verbanck, M., Husson, F. & Josse, J. (2015). Regularised PCA to denoise and visualise data. Statistics & Computing. 25 (2), 471-486
#' @seealso \code{\link{estim_sigma}}
#' @seealso \code{\link{LRsim}}
#' @examples 
#' Xsim <- LRsim(200, 500, 10, 2)
#' opti.ln <- optishrink(Xsim$X, method = "LN", k = 10)
#' opti.asympt <- optishrink(Xsim$X, method = "ASYMPT")
#' 
#' Xsim <- LRsim(200, 500, 100, 1)
#' truesigma <- 1/(1*sqrt(200*500))
#' opti.asympt <- optishrink(Xsim$X, method = "ASYMPT", sigma = truesigma)
#' opti.asympt$nb.eigen

## optishrink
optishrink <- function(X,
                       sigma = NA,
                       center = "TRUE", 
                       method = c("ASYMPT","LN"),
                       loss = c("Frobenius", "Operator","Nuclear"),
                       k = NA){
  # housekeeping
  
  if(inherits(X, "data.frame")){
    X <- as.matrix(X)
  }
  
  if(sum(sapply(X, is.numeric)) < ncol(X)){
    stop("all the variables are not numeric")
  }
  
  if(!is.na(sigma) & sigma <= 0){
    stop("sigma must be positive")
  }
  
  method <- match.arg(method, c("ASYMPT", "Asympt", "asympt", "LN", "ln", "Ln"), several.ok = T)[1]
  method <- tolower(method)
  
  loss <- match.arg(loss, c("Frobenius", "Operator", "Nuclear", "frobenius", "operator", "nuclear", "Frob", "Op", "Nuc", "frob", "op", "nuc"), several.ok = T)[1]
  loss <- tolower(loss)
  
  if(center == "TRUE"){
    moy <- apply(X, 2, mean)
    X <- scale(X, scale = F)
  }
  
  # infer unspecified choices
  
  if(method == "ln" & is.na(k)){
    warning("Since you did not specify k, k was estimated using the FactoMineR estim_ncp function")
    k <- estim_ncp(X, scale = F)$ncp
    print(paste("k = ", k))    
  }  
  
  if(method == "ln" & is.na(sigma)){   
    sigma <- estim_sigma(X, method = "LN", center = center, k = k) 
    warning(paste("sigma estimated by LN:", round(sigma,6)))   
  }
  
  if(method == "asympt" & is.na(sigma)){   
    sigma <- estim_sigma(X, method = "MAD", center = center)
    warning(paste("sigma estimated by MAD:", round(sigma,6)))  
  }
  
  
  #
  # Solve the Optimal Shrinker
  #
  
  svdX <- svd(X)
  n <- nrow(X)
  p <- ncol(X)
  
  if(center == "TRUE") {
    N <- (n-1)
  } else {
    N <- n
  }
  
  if(method == "ln"){
    
    if(k == 0){
      lambda.shrink <- 0
      X.shrinkopt <- matrix(0, n, p)
    } else {
      lambda.shrink <- (svdX$d[1:k]^2 - n*(p/min(p,N))*sigma^2)/svdX$d[1:k]
      X.shrinkopt <- (svdX$u[, 1:k, drop = F] %*% diag(lambda.shrink, k, k)%*%t(svdX$v[, 1:k, drop = F]))
    }
    nb.eigen <- k
    
  } else {
    
    beta <-  min(n, p)/max(n, p)
    svdXajust <- svd(X/(sqrt(max(n, p))*sigma) )
    ind <- which((svdXajust$d - (1+sqrt(beta)) >= 0))
    if (length(ind) == 0){
      X.shrinkopt <- matrix(0, n, p)
      nb.eigen <- 0
    } else {   
      
      if(loss == "frobenius") {
        lambda.shrink <- ((1/(svdXajust$d[1:length(ind)])) * sqrt((svdXajust$d[1:length(ind)]^2 - beta -1)^2  - 4*beta)) 
      } else {     
        lambda.shrink <- 1/(sqrt(2)) * sqrt((svdXajust$d[1:length(ind)]^2 - beta -1)  + sqrt((svdXajust$d[1:length(ind)]^2 - beta -1)^2 - 4*beta))
      } 
      
      X.shrinkopt <- (sqrt(max(n, p))*sigma) *(svdXajust$u[, 1:length(ind), drop = F]%*%diag(lambda.shrink, length(ind), length(ind))%*%t(svdXajust$v[, 1:length(ind), drop = F]))
      nb.eigen  <- length(ind) 
      
      if (loss == "nuclear") {   
        ind <-  which((lambda.shrink^4 - beta - sqrt(beta)*lambda.shrink*svdXajust$d[1:length(ind)]) >= 0)
        if (length(ind) == 0){
          X.shrinkopt <- matrix(0, n, p)
          nb.eigen <- 0
        } else {
          lambda.shrink <- 1/(svdXajust$d[1:length(ind)]*lambda.shrink^2)*(svdXajust$d[1:length(ind)]^4 - beta - sqrt(beta)*lambda.shrink*svdXajust$d[1:length(ind)])
          X.shrinkopt <- (sqrt(max(n, p))*sigma)*(svdXajust$u[, 1:length(ind), drop=F]%*% diag(lambda.shrink, length(ind), length(ind))%*%t(svdXajust$v[, 1:length(ind), drop=F]))
          nb.eigen <- length(ind)      
        }
      } 
    }
  }
  
  if(center == "TRUE"){ 
    X.shrinkopt <-  X.shrinkopt + matrix(moy, nrow(X), ncol(X), byrow = T)
  }
  
  row.names(X.shrinkopt) <- row.names(X)
  colnames(X.shrinkopt) <- colnames(X)
  
  return(list(mu.hat = X.shrinkopt,
              nb.eigen = nb.eigen,
              singval = lambda.shrink,
              low.rank = svd(X.shrinkopt)))
}
