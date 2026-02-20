#' Wald Chi-Square Test
#'
#' This function implements the Wald \emph{chi-square} test on a \eqn{K}x\eqn{1} parameter vector \strong{theta}. The test assumes that \strong{thetaHat}, a consistent estimator of \strong{theta} such as MLE, is asymptotically normal with mean \strong{theta} and covariance matrix \strong{Sigma}. The function can implement 1 test on \strong{theta} as well as multiple, \strong{Q}, tests jointly on \strong{theta}.
#'
#' @param R A \eqn{Q}x\eqn{K} matrix of known coefficients depending on how the test is to be carried out.
#' @param thetaHat An estimate of \strong{theta}.
#' @param Sigma An estimated covariance matrix for \code{thetaHat}.
#' @param r A \eqn{Q}x\eqn{1} matrix of hypothesized values.
#' @param L A character string to be used as a name of the test. When NULL, "L" will be used.
#' 
#' @importFrom stats pchisq
#' 
#' @details
#' 
#' Suppose that Q tests are to be performed jointly on the K by 1 parameter vector \strong{theta}. Let R be a \eqn{Q}x\eqn{K} matrix of known coefficients such as 0, 1, and -1, and r be a \eqn{Q}x\eqn{1} matrix of hypothesized values. The hypotheses are \eqn{H0:} \eqn{R}\eqn{\theta} = \eqn{r} vs. \eqn{H1}: \eqn{R}\eqn{\theta} != \eqn{r}. The test statistic has a chi-square distribution with Q degrees of freedom (Buse, 1982; Agresti, 2002).
#' 
#' @return A data.frame object of the Wald test results.
#'
#' @export
#' 
#' @references
#' 
#' Agresti A. (2002). Categorical Data Analysis (2nd ed.). Wiley. ISBN 0471360937.
#'
#' Buse A. (1982). The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician}, 36:153-157.
#'
#' @examples
#' 
#' library(groupTesting)
#' 
#' ## Example 1
#' # Parameter: p (proportion)
#' MLE <- 0.42
#' Var <- 0.016
#' # (a) Test  H0: p = 0.50  vs. H1: p != 0.50
#' R <- matrix(1, nrow=1, ncol=1)
#' p0 <- 0.50
#' waldTest( R=R, thetaHat=MLE, r=p0, Sigma=Var ) 
#' 
#' ## Example 2
#' # Parameter: beta = (beta1, beta2), regression coefficients
#' MLE <- c(1.09, 2.95)
#' Cov <- rbind(c(0.21, -0.27), 
#'              c(-0.27, 0.66))
#' # (a) Test  H0: beta1 = beta2  vs. H1: beta1 != beta2
#' R <- rbind(c(1,-1))
#' waldTest( R=R, thetaHat=MLE, r=0, Sigma=Cov, L="1 vs 2" )
#' 
#' # (b) Test  H0: beta1 = 0  vs. H1: beta1 != 0
#' R <- rbind(c(1,0))
#' waldTest( R=R, thetaHat=MLE, r=0, Sigma=Cov )
#' 
#' ## Example 3
#' # Parameter: beta = (beta0, beta1, beta2)
#' MLE <- c(-3.05, 1.99, 0.93)
#' Cov <- rbind(c( 0.045, -0.022, -0.034),
#'              c(-0.022,  0.032,  0.008),
#'              c(-0.034,  0.008,  0.048))
#'
#' # Performing simultaneous test:
#' # H0: beta0  = -3, H0: beta1  = 2, H0: beta2  = 1
#' # H1: beta0 != -3, H1: beta1 != 2, H1: beta2 != 1
#' R <- rbind(c(1,0,0), 
#'            c(0,1,0), 
#'            c(0,0,1))
#' r <- matrix( c(-3,2,1), nrow=3 )
#' waldTest( R=R, thetaHat=MLE, r=r, Sigma=Cov)
#' 
waldTest <- function(R,thetaHat,Sigma,r=0,L=NULL){
  Lhat <- R%*%thetaHat - r
  # Wald test statistic, W:
  W <- t(Lhat)%*%solve(R%*%Sigma%*%t(R))%*%Lhat
  DF <- nrow(R)
  p.value <- 1 - stats::pchisq(as.numeric(W),DF)
  res <- data.frame(round(W,2), round(DF), round(p.value,5))
  res <- noquote(res)
  colnames(res) <- c("ChiSq","DF","Pr > ChiSq")
  if( is.null(L) ){ L <- "L" }
  rownames(res) <- L
  return(res)
}
