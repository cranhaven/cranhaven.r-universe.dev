#' @title Test proposed by Schott (2007)
#' @description
#' Schott, J. R. (2007)'s test for one-way MANOVA problem for high-dimensional data with assuming that underlying covariance matrices are the same.
#' @usage ks_s2007(Y,n,p)
#' @param Y A list of \eqn{k} data matrices.  The \eqn{i}th element represents the data matrix (\eqn{p\times n_i}) from the \eqn{i}th population with each column representing a \eqn{p}-dimensional sample.
#' @param n A vector of \eqn{k} sample sizes. The \eqn{i}th element represents the sample size of group \eqn{i}, \eqn{n_i}.
#' @param p The dimension of data.

#'
#' @details
#' Suppose we have the following \eqn{k} independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma},i=1,\ldots,k.
#' }
#' It is of interest to test the following one-way MANOVA problem:
#' \deqn{H_0: \boldsymbol{\mu}_1=\cdots=\boldsymbol{\mu}_k, \quad \text { vs. }\; H_1: H_0 \;\operatorname{is \; not\; ture}.}
#' Schott (2007) proposed the following test statistic:
#' \deqn{
#'  T_{S}=[\operatorname{tr}(\boldsymbol{H})/h-\operatorname{tr}(\boldsymbol{E})/e]/\sqrt{N-1},
#'  }
#'  where \eqn{\boldsymbol{H}=\sum_{i=1}^kn_i(\bar{\boldsymbol{y}}_i-\bar{\boldsymbol{y}})(\bar{\boldsymbol{y}}_i-\bar{\boldsymbol{y}})^\top}, \eqn{\boldsymbol{E}=\sum_{i=1}^k\sum_{j=1}^{n_i}(\boldsymbol{y}_{ij}-\bar{\boldsymbol{y}}_{i})(\boldsymbol{y}_{ij}-\bar{\boldsymbol{y}}_{i})^\top}, \eqn{h=k-1}, and \eqn{e=N-k}, with \eqn{N=n_1+\cdots+n_k}.
#' They showed that under the null hypothesis, \eqn{T_{S}} is asymptotically normally distributed.
#'
#' @references
#' \insertRef{schott2007some}{NRAHDLTP}
#'
#' @return A  (list) object of  \code{S3} class \code{htest}  containing the following elements:
#' \describe{
#' \item{statistic}{the test statistic proposed by Schott (2007).}
#' \item{p.value}{the \eqn{p}-value of the test proposed by Schott (2007).}
#' }
#'
#' @examples
#' set.seed(1234)
#' k <- 3
#' p <- 50
#' n <- c(25, 30, 40)
#' rho <- 0.1
#' M <- matrix(rep(0, k * p), nrow = k, ncol = p)
#' y <- (-2 * sqrt(1 - rho) + sqrt(4 * (1 - rho) + 4 * p * rho)) / (2 * p)
#' x <- y + sqrt((1 - rho))
#' Gamma <- matrix(rep(y, p * p), nrow = p)
#' diag(Gamma) <- rep(x, p)
#' Y <- list()
#' for (g in 1:k) {
#'   Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
#'   Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
#' }
#' ks_s2007(Y, n, p)
#' @export
ks_s2007 <- function(Y, n, p) {
  stats <- ks_s2007_cpp(Y, n, p)
  stat <- stats[1]
  sigma <- stats[2]
  statstd <- stat / sigma
  pvalue <- pnorm(
    q = stat / sigma, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE
  )
  names(statstd) <- "statistic"
  res <- list(statistic = statstd, p.value = pvalue)
  class(res) <- "htest"
  return(res)
}
