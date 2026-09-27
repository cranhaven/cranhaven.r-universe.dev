#' @title  Robust Permutation Test for Covariance Matrices
#'
#' @description
#' Robust Permutation Test for Covariance Matrices in High Dimensional Data
#' @details
#' \code{RobPer_CovTest} function calculates directly p-value based on the calculated value of test statistics and the permutational distribution of test statistics for covariance matrices of two or more independent samples in high dimensional data based on the minimum regularized covariance determinant estimators.
#' 
#' 
#' @importFrom  mvtnorm rmvnorm
#' @param x the data matrix
#' @param group the grouping vector. It must be factor.
#' @param N the permutation number and the default value is 100.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized. 
#'              Allowed values are between 0.5 and 1 and the default is 0.75.  
#' 
#' @export
#'
#' @return a list with 3 elements:
#' \item{pval}{p-value of the robust permutation test process}
#' \item{TM}{The calculated value of test statistics based on raw data}
#' \item{Permutations_TM}{The calculated values of test statistics based on each permutational data}
#' @references Bulut, H (2024). A robust permutational test to compare covariance matrices in high dimensional data. (Unpublished)
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' if (requireNamespace("rrcov", quietly=TRUE)) {
#' x1<-mvtnorm::rmvnorm(n = 10,mean = rep(0,20),sigma = diag(20))
#' x2<-mvtnorm::rmvnorm(n = 10,mean = rep(0,20),sigma = 2*diag(20))
#' x3<-mvtnorm::rmvnorm(n = 10,mean = rep(0,20),sigma = 3*diag(20))
#' data<-rbind(x1,x2,x3)
#' group_label<-c(rep(1,10),rep(2,10),rep(3,10))
#' RobPer_CovTest(x=data, group=group_label)}

RobPer_CovTest<-function(x, group,N=100,alpha=0.75){
  if (!requireNamespace("rrcov", quietly=TRUE))
    stop("Package 'rrcov' is required for Rob_CovTest (CovMrcd). Please install it.", call.=FALSE)
  
  nh<-table(group) ; n<-sum(nh)
  k<-length(nh)
  
  # step-1
  TM<-Rob_CovTest(x,group,alpha=alpha)
  test.value<-rep(TM,N)
  
  # step-2: N times step-2 and step-3
  TMi <- rep(NA, N)  # create a vector to store Tis
  for(i in 1:N) {
    obs<-sample(n)  # gözlemleri random karıştır
    # Store Ti in Tis vector
    TMi[i] <- Rob_CovTest(x[obs,],group,alpha=alpha)
  }
  
  # p- values
  pval<-length(which(TMi>test.value))/N
  return(list(pval=pval,TM=TM,Permutations_TM=TMi))
}
