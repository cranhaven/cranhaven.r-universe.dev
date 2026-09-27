#' @title Robust Hotelling T^2 Test for One Sample in High Dimensional Data  
#'
#' @description
#' Robust Hotelling T^2 Test for One Sample in high Dimensional Data
#' @details
#' \code{RHT2} function performs a robust Hotelling T^2 test in high dimensional test based on the minimum regularized covariance determinant estimators.
#' This function needs the q and d values. These values can be obtained \code{simRHT2} function.  
#' For more detailed information, you can see the study by Bulut (2021).
#' 
#' @param data the data. It must be matrix or data.frame.
#' @param mu0 the mean vector which will be used to test the null hypothesis.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized. 
#'              Allowed values are between 0.5 and 1 and the default is 0.75.  
#' @param d the constant in Equation (11) in the study by Bulut (2021).
#' @param q the second degree of freedom value of the approximate F distribution in Equation (11) in the study by Bulut (2021).

#' @export
#'
#' @return a list with 3 elements:
#' \item{T2}{The Robust Hotelling T^2 value in high dimensional data}
#' \item{Fval}{The F value based on T2}
#' \item{pval}{The p value based on the approximate F distribution}
#' @references Bulut, H (2021). A robust Hotelling test statistic for one sample case in high dimensional data,
#' Communication in Statistics: Theory and Methods.
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' if (requireNamespace("rrcov", quietly = TRUE)) {
#' utils::data("octane", package = "rrcov")
#' mu.clean <- colMeans(octane[-c(25,26,36,37,38,39), ])
#' RHT2(data = octane, mu0 = mu.clean, alpha = 0.84, d = 1396.59, q = 1132.99)}


RHT2<-function(data,mu0,alpha=0.75,d,q){
  if (!requireNamespace("rrcov", quietly=TRUE))
    stop("Package 'rrcov' is required for RHT2 (CovMrcd). Please install it.", call.=FALSE)
  
  data<-as.matrix(data)
  n<-nrow(data); p<-ncol(data)
  mu0<-as.matrix(mu0)
  mrcd.est<-rrcov::CovMrcd(x = data,alpha = alpha)
  mean.mrcd<-as.matrix(mrcd.est@center)
  sigma.mrcd<-as.matrix(mrcd.est@cov)
  
  T2.mrcd<-n*t(mean.mrcd-mu0)%*%solve(sigma.mrcd)%*%(mean.mrcd-mu0)
  
  Fh<-T2.mrcd/d
  pval<-pf(q=Fh,df1=p,df2=q,lower.tail = FALSE)
  return(list(T2=T2.mrcd,Fval=Fh,pval=pval))
}
