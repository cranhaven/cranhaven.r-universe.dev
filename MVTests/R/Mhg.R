#' @title  Pair-Wise comparison between hth and gth sample
#'
#' @description
#' Pair-Wise comparison of covariance matrices between hth and gth sample 
#' @details
#' \code{Mhg} function computes proposed Mgh values as defined in the paper.
#' 
#' @importFrom  mvtnorm rmvnorm
#' @param Sh the robust covariance matrix of the hth sample
#' @param Sg the robust covariance matrix of the gth sample
#' @param S the robust pooled covariance matrix.
#' @param nh the sample size of the hth sample
#' @param ng the sample size of the gth sample
#' @param n the sample size of the full data
#' 
#' @export
#'
#' @return a list with 1 elements:
#' \item{Mhg}{Mgh value}
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
#' n <- nrow(data)
#' p <- ncol(data)
#' nk <- table(group_label)
#' g <- length(nk)
#' Levels <- unique(group_label)
#' Si.matrices<-lapply(1:g, function(i) rrcov::CovMrcd(data[(group_label==Levels[i]),],
#' alpha=0.9)@cov)
#' Spool <- Reduce("+", Map("*", nk, Si.matrices)) / n
#' #for the first and second groups
#' Mhg(Sh = Si.matrices[[1]], Sg = Si.matrices[[2]],S = Spool, nh = nk[1], ng = nk[2], n = n)}

Mhg<-function(Sh,Sg,S,nh,ng,n){
  eigenS<-eigen(S)
  lamda<-diag(eigenS$values)
  P<-eigenS$vector
  S.root<-P%*%sqrt(lamda)%*%t(P)
  a<-solve(S.root)
  matrix.result<-sqrt(nh*ng/n)*a%*%(Sh-Sg)%*%a
  return(Mhg=max(abs(eigen(matrix.result)$values)))
}

