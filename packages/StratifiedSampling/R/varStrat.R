#' @title Approximated variance for balanced sampling
#' @name varApp
#' @param X A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
#' @param strata A vector of integers that represents the categories.
#' @param pik A vector of inclusion probabilities.
#' @param y A variable of interest.
#'
#' @return a scalar, the value of the approximated variance.
#' @export
#' 
#' @details
#' 
#' This function gives an approximation of the variance of the Horvitz-Thompson total estimator presented by Hasler and Tillé (2014). 
#'
#' @references 
#' Hasler, C. and Tillé, Y. (2014). Fast balanced sampling for highly stratified population. \emph{Computational Statistics and Data Analysis}, 74:81-94.
#' 
#' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
#'
#' @seealso \code{\link{varEst}} 
#' 
#' @examples
#' 
#' N <- 1000
#' n <- 400
#' x1 <- rgamma(N,4,25)
#' x2 <- rgamma(N,4,25)
#' 
#' strata <- as.matrix(rep(1:40,each = 25)) # 25 strata
#' Xcat <- disjMatrix(strata)
#' pik <- rep(n/N,N)
#' X <- as.matrix(matrix(c(x1,x2),ncol = 2))
#'  
#' s <- stratifiedcube(X,strata,pik)
#'  
#' y <- 20*strata + rnorm(1000,120) # variable of interest
#' # y_ht <- sum(y[which(s==1)]/pik[which(s == 1)]) # Horvitz-Thompson estimator
#' # (sum(y_ht) - sum(y))^2 # true variance
#' varEst(X,strata,pik,s,y)
#' varApp(X,strata,pik,y)
varApp <- function(X,strata,pik,y){
  
  Xcat <- disj(strata)
  X_tmp <- cbind(Xcat,X)
  N <- length(pik)
  H <- ncol(Xcat)
  q <- ncol(X)
  
  beta <- matrix(rep(0,(H+q)^2),ncol = (H+q),nrow = (H+q))
  
  #compute beta
  for(k in 1:N){
    z_k <- X_tmp[k,]
    b_k <- pik[k]*(1-pik[k])*(N/(N-(H+q)))
    beta <- beta + b_k*(z_k/pik[k])%*%t(z_k/pik[k])
  } 
  beta <- solve(beta)
  
  #compute tmp
  tmp <- rep(0,H+q)
  for(k in 1:N){
    z_k <- X_tmp[k,]
    b_k <- pik[k]*(1-pik[k])*(N/(N-(H+q)))
    tmp <- tmp + b_k*(y[k]/pik[k])*(z_k/pik[k])
  } 
  
  beta <- beta%*%tmp
  
  #compute v
  v <- 0
  for(k in 1:N){
    z_k <- X_tmp[k,]
    b_k <- pik[k]*(1-pik[k])*(N/(N-(H+q)))
    
    v <- v + b_k*( (y[k]/pik[k]) - t(beta)%*%(z_k/pik[k]) )^2
  } 
  
  v
  return(v)
  
  
}


#' @title Estimator of the approximated variance for balanced sampling
#' @name varEst
#' @param X A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
#' @param strata A vector of integers that represents the categories.
#' @param pik A vector of inclusion probabilities.
#' @param s A sample (vector of 0 and 1, if rejected or selected).
#' @param y A variable of interest.
#'
#'
#' @return a scalar, the value of the estimated variance.
#'
#' @details
#' 
#' This function gives an estimator of the approximated variance of the Horvitz-Thompson total estimator presented by Hasler C. and Tillé Y. (2014). 
#' 
#' @seealso \code{\link{varApp}}
#' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
#' @references 
#' Hasler, C. and Tillé, Y. (2014). Fast balanced sampling for highly stratified population. \emph{Computational Statistics and Data Analysis}, 74:81-94.
#'
#' @examples
#' 
#' N <- 1000
#' n <- 400
#' x1 <- rgamma(N,4,25)
#' x2 <- rgamma(N,4,25)
#' 
#' strata <- as.matrix(rep(1:40,each = 25)) # 25 strata
#' Xcat <- disjMatrix(strata)
#' pik <- rep(n/N,N)
#' X <- as.matrix(matrix(c(x1,x2),ncol = 2))
#'  
#' s <- stratifiedcube(X,strata,pik)
#'  
#' y <- 20*strata + rnorm(1000,120) # variable of interest
#' # y_ht <- sum(y[which(s==1)]/pik[which(s == 1)]) # Horvitz-Thompson estimator
#' # (sum(y_ht) - sum(y))^2 # true variance
#' varEst(X,strata,pik,s,y)
#' varApp(X,strata,pik,y)
#' @export 
varEst <- function(X,strata,pik,s,y){
  
  
  Xcat <- pik*disj(strata)
  X_tmp <- cbind(Xcat,X)
  # N <- length(pik)
  n <- sum(s)
  index <- which(s == 1)
  H <- ncol(Xcat)
  q <- ncol(X)
  
  if( (H+ q) > n){
    stop("H + q is greater than n.")
  }
  
  beta <- matrix(rep(0,(H+q)^2),ncol = (H+q),nrow = (H+q))
  
  #compute beta
  for(k in index){
    # print(k)
    z_k <- X_tmp[k,]
    c_k <- (1-pik[k])*(n/(n-(H+q)))
    beta <- beta + c_k*(z_k/pik[k])%*%t(z_k/pik[k])
  } 
  beta <- solve(beta)
  
  #compute tmp
  tmp <- rep(0,H+q)
  for(k in index){
    z_k <- X_tmp[k,]
    c_k <- (1-pik[k])*(n/(n-(H+q)))
    tmp <- tmp + c_k*y[k]/pik[k]*(z_k/pik[k])
  } 
  
  beta <- beta%*%tmp
  
  #compute v
  v <- 0
  for(k in index){
    z_k <- X_tmp[k,]
    c_k <- (1-pik[k])*(n/(n-(H+q)))
    v <- v + c_k*( (y[k]/pik[k]) - t(beta)%*%(z_k/pik[k]) )^2
  } 
  
  v
  return(v)
  
  
}
