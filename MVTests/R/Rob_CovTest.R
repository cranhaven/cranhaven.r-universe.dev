#' @title  Robust Test for Covariance Matrices
#'
#' @description
#' Robust Test for Covariance Matrices in High Dimensional Data
#' @details
#' \code{Rob_CovTest} function computes the calculated value of the test statistic for covariance matrices of two or more independent samples in high dimensional data based on the minimum regularized covariance determinant estimators.
#' 
#' 
#' @importFrom  mvtnorm rmvnorm
#' @param x the data matrix
#' @param group the grouping vector. It must be factor.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized. 
#'              Allowed values are between 0.5 and 1 and the default is 0.75.  
#' 
#' @export
#'
#' @return a list with 1 elements:
#' \item{TM}{The calculated value of test statistics based on raw data}
#' @references Bulut, H (2024). A robust permutational test to compare covariance matrices in high dimensional data. (Unpublished)
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' if (requireNamespace("rrcov", quietly=TRUE)) {
#' x1<-mvtnorm::rmvnorm(n = 8,mean = rep(0,10),sigma = diag(10))
#' x2<-mvtnorm::rmvnorm(n = 8,mean = rep(0,10),sigma = 2*diag(10))
#' x3<-mvtnorm::rmvnorm(n = 8,mean = rep(0,10),sigma = 3*diag(10))
#' data<-rbind(x1,x2,x3)
#' group_label<-c(rep(1,8),rep(2,8),rep(3,8))
#' Rob_CovTest(x=data, group=group_label)}

Rob_CovTest<- function(x, group,alpha=0.75) {
  if (!requireNamespace("rrcov", quietly=TRUE))
    stop("Package 'rrcov' is required for Rob_CovTest (CovMrcd). Please install it.", call.=FALSE)
  
  n <- nrow(x)
  p <- ncol(x)
  nk <- table(group)
  g <- length(nk)
  Levels <- unique(group)
  
  # Calculation Si matrices
  Si.matrices <- lapply(1:g, function(i) rrcov::CovMrcd(x[(group==Levels[i]),],alpha=alpha)@cov)
  
  # Calculation S pooled
  Spool <- Reduce("+", Map("*", nk, Si.matrices)) / n
  
  # Calculation sum of Mhg values
  Mhg.T <- sum(sapply(1:g, function(h) {
    sum(sapply(1:h, function(k) {
      Mhg(Sh = Si.matrices[[h]], Sg = Si.matrices[[k]], 
          S = Spool, nh = nk[h], ng = nk[k], n = n)
    }))
  }))
  
  # Calculation of TM value
  TM <- 2 / (g * (g - 1)) * Mhg.T
  
  return(TM)
}

