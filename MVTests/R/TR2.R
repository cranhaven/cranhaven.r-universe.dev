#' @title  Robust Hotelling T^2 Test Statistic
#'
#' @description
#' Robust Hotelling T^2 Test Statistic for Two Independent Samples in high Dimensional Data
#' @details
#' \code{TR2} function calculates the robust Hotelling T^2 test statistic for two independent samples in high dimensional data based on the minimum regularized covariance determinant estimators.
#' 
#' @importFrom  mvtnorm rmvnorm
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param x1 the data matrix for the first group. It must be matrix or data.frame.
#' @param x2 the data matrix for the first group. It must be matrix or data.frame.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized. 
#'              Allowed values are between 0.5 and 1 and the default is 0.75.  
#' 
#' @export
#'
#' @return a list with 2 elements:
#' \item{TR2}{The calculated value of Robust Hotelling T^2 statistic based on MRCD estimations}
#' @references Bulut et al. (2024). A Robust High-Dimensional Test for Two-Sample Comparisons, Axioms
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' if (requireNamespace("rrcov", quietly=TRUE)) {
#' x<-mvtnorm::rmvnorm(n=10,sigma=diag(20),mean=rep(0,20))
#' y<-mvtnorm::rmvnorm(n=10,sigma=diag(20),mean=rep(1,20))
#' TR2(x1=x,x2=y)}

TR2<-function(x1,x2,alpha=0.75){
  if (!requireNamespace("rrcov", quietly=TRUE))
    stop("Package 'rrcov' is required for TR2 (CovMrcd). Please install it.", call.=FALSE)
  
  
  x1<-as.matrix(x1)   ;  x2<-as.matrix(x2)
  n1<-nrow(x1); n2<-nrow(x2); n<-n1+n2 ;  p<-ncol(x1) 
  
  mrcd.est1<-rrcov::CovMrcd(x = x1,alpha = alpha)
  mrcd.est2<-rrcov::CovMrcd(x = x2,alpha = alpha)
  
  mu.mrcd1<-as.matrix(mrcd.est1@center) 
  S1.mrcd<-as.matrix(mrcd.est1@cov)
  
  mu.mrcd2<-as.matrix(mrcd.est2@center)
  S2.mrcd<-as.matrix(mrcd.est2@cov)
  
  Spool<-(((n1-1)*S1.mrcd)+((n2-1)*S2.mrcd))/(n1+n2-2)
  
  T2.mrcd<-((n1*n2)/(n))*t(mu.mrcd1-mu.mrcd2)%*%solve(Spool)%*%(mu.mrcd1-mu.mrcd2)
  return(list=c(TR2=T2.mrcd))
}
