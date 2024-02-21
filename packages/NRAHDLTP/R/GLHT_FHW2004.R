#' @title Test for proposed by Fujikoshi et al. (2004)
#' @description
#' Fujikoshi et al. (2004)'s test for general linear hypothesis testing (GLHT) problem for high-dimensional data with assuming that underlying covariance matrices are the same.
#'
#' @usage glht_fhw2004(Y,X,C)
#' @param Y An \eqn{n\times p} response matrix obtained by independently observing a \eqn{p}-dimensional response variable for \eqn{n} subjects.
#' @param X A known \eqn{n\times k} full-rank design matrix with \eqn{\operatorname{rank}(\boldsymbol{G})=k<n}.
#' @param C A known matrix of size \eqn{q\times k} with \eqn{\operatorname{rank}(\boldsymbol{C})=q<k}.

#'
#' @details
#' A high-dimensional linear regression model can be expressed as
#' \deqn{\boldsymbol{Y}=\boldsymbol{X\Theta}+\boldsymbol{\epsilon},}
#' where \eqn{\Theta} is a \eqn{k\times p} unknown parameter matrix and \eqn{\boldsymbol{\epsilon}} is an \eqn{n\times p} error matrix.
#'
#' It is of interest to test the following GLHT problem
#' \deqn{H_0: \boldsymbol{C\Theta}=\boldsymbol{0}, \quad \text { vs. } \quad H_1: \boldsymbol{C\Theta} \neq \boldsymbol{0}.}
#'
#' Fujikoshi et al. (2004) proposed the following test statistic:
#' \deqn{T_{FHW}=\sqrt{p}\left[(n-k)\frac{\operatorname{tr}(\boldsymbol{S}_h)}{\operatorname{tr}(\boldsymbol{S}_e)}-q\right],}
#' where \eqn{\boldsymbol{S}_h} and \eqn{\boldsymbol{S}_e} are the matrices of sums of squares and products due to the hypothesis and the error, respecitively.
#'
#' They showed that under the null hypothesis, \eqn{T_{FHW}} is asymptotically normally distributed.


#' @references
#' \insertRef{fujikoshi_2004_asymptotic}{NRAHDLTP}
#'
#' @return A  (list) object of  \code{S3} class \code{htest}  containing the following elements:
#' \describe{
#' \item{statistic}{the test statistic proposed by Fujikoshi et al. (2004).}
#' \item{p.value}{the \eqn{p}-value of the test proposed by Fujikoshi et al. (2004).}
#' }

#' @examples

#' set.seed(1234)
#' k <- 3
#' q <- k-1
#' p <- 50
#' n <- c(25,30,40)
#' rho <- 0.01
#' Theta <- matrix(rep(0,k*p),nrow=k)
#' X <- matrix(c(rep(1,n[1]),rep(0,sum(n)),rep(1,n[2]),rep(0,sum(n)),rep(1,n[3])),ncol=k,nrow=sum(n))
#' y <- (-2*sqrt(1-rho)+sqrt(4*(1-rho)+4*p*rho))/(2*p)
#' x <- y+sqrt((1-rho))
#' Gamma <- matrix(rep(y,p*p),nrow=p)
#' diag(Gamma) <- rep(x,p)
#' U <- matrix(ncol = sum(n),nrow=p)
#' for(i in 1:sum(n)){
#' U[,i] <- rnorm(p,0,1)
#' }
#' Y <- X%*%Theta+t(U)%*%Gamma
#' C <- cbind(diag(q),-rep(1,q))
#' glht_fhw2004(Y,X,C)


#' @export
glht_fhw2004 <- function(Y, X, C) {
  stats <- glht_fhw2004_cpp(Y, X, C)
  pvalue <- pnorm(stats, 0, 1, lower.tail = FALSE, log.p = FALSE)
  names(stats) <- "statistic"
  res <- list(statistic = stats, p.value = pvalue)
  class(res) <- "htest"
  return(res)
}
