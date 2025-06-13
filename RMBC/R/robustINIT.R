#' robustINIT
#'
#' Robust Initializer for RMBC algorithm, it depends on the package \code{ktaucenters}
#' @param Y A matrix of size n x p.
#' @param K The number of groups
#' @param nstart the number of starting points to the algorithm, defaults to 10 
#' @return A list including the initial parameters of the mixture distribution, namely
#' \itemize{
#' \item{\code{alphaINIT}}{: K numeric values representing the convex combination coefficients.}
#' \item{\code{muINIT}}{: a list of length K with the location initial estimators.}
#' \item{\code{sigmaINIT}}{: a list of length K with the scatter matrix estimators.}
#' \item{\code{indicesINIT}}{: indices with initial clusters}
#' }
#'   
robustINIT=function(Y,K,nstart=10){
  # applying  initial estimator 
  sal1 <- ktaucenters::improvedktaucenters(X=Y, K=K, nstart = nstart)
  # giving list format to the estimator  
  thetaOld.mu <- lapply(1:K, function(i){sal1$centers[i,]})
  thetaOld.sigma <- sal1$sigmas
  # compute alpha among non outliers 
  if(length(-sal1$outliers)>0){
    alpha <- table(sal1$cluster[-sal1$outliers])/sum(table(sal1$cluster[-sal1$outliers]))
  }
  # the above if gave error (alpha=numeric(0) if there are not outliers detected, 
  # (fixed with the below if)
  if(length(-sal1$outliers)==0){
    alpha <- table(sal1$cluster)/sum(table(sal1$cluster))
  }
  alpha <- as.numeric(alpha)
  list(alphaINIT=alpha,muINIT=thetaOld.mu,sigmaINIT=thetaOld.sigma,indicesINIT=sal1$cluster)
}

