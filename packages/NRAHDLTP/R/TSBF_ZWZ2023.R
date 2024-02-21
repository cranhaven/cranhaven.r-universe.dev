#' @title
#' Test proposed by Zhu et al. (2023)
#' @description
#' Zhu et al. (2023)'s test for testing equality of two-sample high-dimensional mean vectors without assuming that two covariance matrices are the same.

#' @usage tsbf_zwz2023(y1, y2)
#' @param y1 The data matrix (p by n1) from the first population. Each column represents a \eqn{p}-dimensional sample.
#' @param y2 The data matrix (p by n2) from the first population. Each column represents a \eqn{p}-dimensional sample.
#
#' @details
#' Suppose we have two independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma}_i,\; i=1,2.
#' }
#' The primary object is to test
#' \deqn{H_{0}: \boldsymbol{\mu}_1 = \boldsymbol{\mu}_2\; \operatorname{versus}\; H_{1}: \boldsymbol{\mu}_1 \neq \boldsymbol{\mu}_2.}
#' Zhu et al. (2023) proposed the following test statistic:
#' \deqn{T_{ZWZ}=\frac{n_1n_2n^{-1}\|\bar{\boldsymbol{y}}_1-\bar{\boldsymbol{y}}_2\|^2}{\operatorname{tr}(\hat{\boldsymbol{\Omega}}_n)},}
#' where  \eqn{\bar{\boldsymbol{y}}_{i},i=1,2} are the sample mean vectors and \eqn{\hat{\boldsymbol{\Omega}}_n} is the estimator of \eqn{\operatorname{Cov}[(n_1n_2/n)^{1/2}(\bar{\boldsymbol{y}}_1-\bar{\boldsymbol{y}}_2)]}.
#' They showed that under the null hypothesis, \eqn{T_{ZWZ}} and an F-type mixture have the same normal or non-normal limiting distribution.

#' @references
#' \insertRef{zhu2022two}{NRAHDLTP}
#'
#' @return A  (list) object of  \code{S3} class \code{htest}  containing the following elements:
#' \describe{
#' \item{p.value}{the p-value of the test proposed by  Zhu et al. (2023).}
#' \item{statistic}{the test statistic proposed by Zhu et al. (2023).}
#' \item{df1}{estimated approximate degrees of freedom \eqn{d_1} of Zhu et al. (2023)'s test.}
#' \item{df2}{estimated approximate degrees of freedom \eqn{d_2} of Zhu et al. (2023)'s test.}
#' }
#'
#' @examples
#' set.seed(1234)
#' n1 <- 20
#' n2 <- 30
#' p <- 50
#' mu1 <- t(t(rep(0, p)))
#' mu2 <- mu1
#' rho1 <- 0.1
#' rho2 <- 0.2
#' a1 <- 1
#' a2 <- 2
#' w1 <- (-2 * sqrt(a1 * (1 - rho1)) + sqrt(4 * a1 * (1 - rho1) + 4 * p * a1 * rho1)) / (2 * p)
#' x1 <- w1 + sqrt(a1 * (1 - rho1))
#' Gamma1 <- matrix(rep(w1, p * p), nrow = p)
#' diag(Gamma1) <- rep(x1, p)
#' w2 <- (-2 * sqrt(a2 * (1 - rho2)) + sqrt(4 * a2 * (1 - rho2) + 4 * p * a2 * rho2)) / (2 * p)
#' x2 <- w2 + sqrt(a2 * (1 - rho2))
#' Gamma2 <- matrix(rep(w2, p * p), nrow = p)
#' diag(Gamma2) <- rep(x2, p)

#' Z1 <- matrix(rnorm(n1*p,mean = 0,sd = 1), p, n1)
#' Z2 <- matrix(rnorm(n2*p,mean = 0,sd = 1), p, n2)
#' y1 <- Gamma1 %*% Z1 + mu1%*%(rep(1,n1))
#' y2 <- Gamma2 %*% Z2 + mu2%*%(rep(1,n2))
#' tsbf_zwz2023(y1, y2)

#' @export
tsbf_zwz2023 <- function(y1, y2) {
  if (nrow(y1) != nrow(y2)) {
    stop("y1 and y2 must have same dimension!")
  } else {
    stats <- tsbf_zwz2023_cpp(y1, y2)
    stat <- stats[1]
    df1 <- stats[2]
    df2 <- stats[3]
    pvalue <- pf(stat, df1, df2, ncp = 0, lower.tail = FALSE, log.p = FALSE)
  }
  names(stat) <- "statistic"
  names(df1) <- "df1"
  names(df2) <- "df2"
  res <- list(statistic = stat, p.value = pvalue, parameters = c(df1, df2))
  class(res) <- "htest"
  return(res)
}
