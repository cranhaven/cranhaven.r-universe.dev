##########################################################################################
####            Rank estimation fractional cointegration                           #######
####               Zhang, Robinson, Yao (2018)                                     #######                     
##########################################################################################

#' Correlation in ZRY18
#' @keywords internal
corr_ZRY <- function(x, lags)
{
T<-length(x)
corr <- numeric(length=lags)
for(k in 1:lags){
  help<-numeric(length=(T-k))
  x_quer <- mean(x) 
    for(t in 1:(T-k))
    {
      help[t] <- (x[t+k]-x_quer)*(x[t]-x_quer)
    }
    corr[k]<-sum(help)/(T-k)
  }
   variance <- sum((x-x_quer)^2)/T
  return(corr/variance)
}


#' @title Rank estimation in fractionally cointegrated systems (Zhang, Robinson, Yao (2018))
#' @description \code{FCI_CH06} SRank estimation in fractionally cointegrated systems (Zhang, Robinson, Yao (2018)).
#'  Returns estimated cointegrating rank, r=0,...,dim-1.
# #' @details add details here.
#' @param X data matrix.
#' @param lag_max number of lags in autocovariance matrix of data for eigenvector estimation.
#' @param lag_max2 number of residual autocorrelations that are averaged, default is \code{m=20} as recommended by Zhang, Robinson, Yao (2018).
#' @param c0 threshold to compare averaged residual autocorrelation to, default is \code{c0=0.3} as recommended by Zhang, Robinson, Yao (2018).
#' @references  Zhang, R., Robinson, P. and Yao, Q. (2018): Identifying cointegration by eigenanalysis. Journal of the American Statistical Association (forthcoming).
#' @author Michelle Voges
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=3, rho=0.4, d=c(0.2,0.2,1), B=rbind(c(1,0,-1),c(0,1,-1),c(0,0,1)))
#' FCI_ZRY18(series, lag_max=5, lag_max2=20, c0=0.3) 
#' series<-FI.sim(T=T, q=3, rho=0.4, d=c(1,1,1))
#' FCI_ZRY18(series, lag_max=5, lag_max2=20, c0=0.3) 
#' @export

FCI_ZRY18 <- function(X, lag_max, lag_max2=20, c0=0.3)
{
 if (which.max(dim(X)) != 1) {X <- t(X)}        # change dimension of data if necessary
 T<-nrow(X)                                     # number of observations
 nbr <- ncol(X)                                 # number of time series
 
 ### construct autocovariance function up to lag "lag_max"
 sigma <- acf(X, plot=F, lag.max=lag_max, type="covariance")$acf
 ### rearrange autocovariance function (matrix for each lag)
 sigma_sorted <- array(dim=c(nbr,nbr,(lag_max+1)))
 for(i in 1:(lag_max+1))
 for(j in 1:nbr){
 {
   sigma_sorted[j,,i] <- sigma[i,,j]
 }
 }
 ### calculate sum of product of autocovariance function (W in paper)
 cov_sum <- matrix(0,nrow=nbr,ncol=nbr)
 for(i in 1:(lag_max+1))
 {
   helper <- sigma_sorted[,,i]%*%sigma_sorted[,,i]
   cov_sum <- cov_sum+helper
 }
  
  ### determine eigenvectors from covariance sum (descending eigenvalues) 
  betta <- eigen(cov_sum)$vectors
  
  ### construct residuals and calculate autocorrelation function of residual (without 1)
  residual <- matrix(nrow=T, ncol=nbr)
  correlation <- matrix(nrow=lag_max2, ncol=nbr)
  for(i in 1:nbr)
  {
  residual[,i] <- X%*%betta[,i]
  correlation[,i] <- corr_ZRY(residual[,i], lags=lag_max2)
  }
 
  r <- sum(Re(colSums(correlation)/lag_max2)<c0)
  return(r)
 }
