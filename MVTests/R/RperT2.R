#' @title  Robust Permutation Hotelling T^2 Test in High Dimensional Data  
#'
#' @description
#' Robust Permutation Hotelling T^2 Test for Two Independent Samples in high Dimensional Data
#' @details
#' \code{RperT2} function performs a robust permutation Hotelling T^2 test for two independent samples in high dimensional test based on the minimum regularized covariance determinant estimators.
#' 
#' 
#' @importFrom  mvtnorm rmvnorm
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param X1 the data matrix for the first group. It must be matrix or data.frame.
#' @param X2 the data matrix for the first group. It must be matrix or data.frame.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized. 
#'              Allowed values are between 0.5 and 1 and the default is 0.75.  
#' @param N the permutation number
#' 
#' @export
#'
#' @return a list with 2 elements:
#' \item{T2}{The calculated value of Robust Hotelling T^2 statistic based on MRCD estimations}
#' \item{p.value}{p value obtained from test process}
#' @references Bulut et al. (2024). A Robust High-Dimensional Test for Two-Sample Comparisons, Axioms.
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' if (requireNamespace("rrcov", quietly=TRUE)) {
#' x<-mvtnorm::rmvnorm(n=10,sigma=diag(20),mean=rep(0,20))
#' y<-mvtnorm::rmvnorm(n=10,sigma=diag(20),mean=rep(1,20))
#' RperT2(X1=x,X2=y)$p.value}

RperT2<-function(X1,X2,alpha=0.75,N=100){
  if (!requireNamespace("rrcov", quietly=TRUE))
    stop("Package 'rrcov' is required for RperT2 (CovMrcd). Please install it.", call.=FALSE)
  
  X1<-as.matrix(X1)   ;  X2<-as.matrix(X2)
  n1<-nrow(X1); n2<-nrow(X2); n<-n1+n2 ;  p<-ncol(X1) 
  data<-rbind(X1,X2)
  
  # step-1
  T2<-TR2(x1 = X1,x2 = X2,alpha = alpha)
  
  # step-4: N times step-2 and step-3
  TRis <- rep(NA, N)  # create a vector to store Tis
  for(i in 1:N) {
    g1 <- sample(n, size = n1, replace = FALSE) 
    group1 <- data[g1,]  # assign n1 observations to group1
    group2 <- data[-g1,] # assign n2 observations to group2
    # Store Ti in Tis vector
    TRis[i] <- TR2(x1 = group1,x2 = group2,alpha = alpha)
  }
  test<-rep(T2,N)
  # p- values
  pval<-length(which(TRis>test))/N
  return(list(T2=T2,p.value=pval))
}
