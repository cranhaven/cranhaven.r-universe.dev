#' @title Test proposed by Zhang and Zhu (2022)
#' @description
#' Zhang and Zhu (2022)'s test for general lineral hypothesis testing (GLHT) problem for high-dimensional data under heteroscedasticity.
#' @usage glhtbf_zz2022(Y,G,n,p)
#' @param Y A list of \eqn{k} data matrices.  The \eqn{i}th element represents the data matrix (\eqn{p\times n_i}) from the \eqn{i}th population with each column representing a \eqn{p}-dimensional sample.
#' @param G A known full-rank coefficient matrix (\eqn{q\times k}) with \eqn{\operatorname{rank}(\boldsymbol{G})< k}.
#' @param n A vector of \eqn{k} sample sizes. The \eqn{i}th element represents the sample size of group \eqn{i}, \eqn{n_i}.
#' @param p The dimension of data.
#'
#' @details
#' Suppose we have the following \eqn{k} independent high-dimensional samples:
#' \deqn{
#' \boldsymbol{y}_{i1},\ldots,\boldsymbol{y}_{in_i}, \;\operatorname{are \; i.i.d. \; with}\; \operatorname{E}(\boldsymbol{y}_{i1})=\boldsymbol{\mu}_i,\; \operatorname{Cov}(\boldsymbol{y}_{i1})=\boldsymbol{\Sigma}_i,i=1,\ldots,k.
#' }
#' It is of interest to test the following GLHT problem:
#' \deqn{H_0: \boldsymbol{G M}=\boldsymbol{0}, \quad \text { vs. } H_1: \boldsymbol{G M} \neq \boldsymbol{0},}
#' where
#' \eqn{\boldsymbol{M}=(\boldsymbol{\mu}_1,\ldots,\boldsymbol{\mu}_k)^\top} is a \eqn{k\times p} matrix collecting \eqn{k} mean vectors and \eqn{\boldsymbol{G}:q\times k} is a known full-rank coefficient matrix with \eqn{\operatorname{rank}(\boldsymbol{G})<k}.
#'
#' Let  \eqn{\bar{\boldsymbol{y}}_{i},i=1,\ldots,k} be the sample mean vectors and \eqn{\hat{\boldsymbol{\Sigma}}_i,i=1,\ldots,k} be the sample covariance matrices.
#'
#' Zhang and Zhu (2022) proposed the following U-statistic based test statistic:
#' \deqn{
#' T_{ZZ}=\|\boldsymbol{C \hat{\mu}}\|^2-\sum_{i=1}^kh_{ii}\operatorname{tr}(\hat{\boldsymbol{\Sigma}}_i)/n_i,
#' }
#' where \eqn{\boldsymbol{C}=[(\boldsymbol{G D G}^\top)^{-1/2}\boldsymbol{G}]\otimes\boldsymbol{I}_p}, \eqn{\boldsymbol{D}=\operatorname{diag}(1/n_1,\ldots,1/n_k)}, and \eqn{h_{ij}} is the \eqn{(i,j)}th entry of the \eqn{k\times k} matrix \eqn{\boldsymbol{H}=\boldsymbol{G}^\top\boldsymbol{G}}.
#'
#'
#' @references
#' \insertRef{Zhang_2022heteroscedastic}{NRAHDLTP}
#'
#' @return A  (list) object of  \code{S3} class \code{htest}  containing the following elements:
#' \describe{
#' \item{p.value}{the \eqn{p}-value of the test proposed by Zhang and Zhu (2022).}
#' \item{statistic}{the test statistic proposed by Zhang and Zhu (2022).}
#' \item{beta0}{the parameter used in Zhang and Zhu (2022)'s test.}
#' \item{beta1}{the parameter used in Zhang and Zhu (2022)'s test.}
#' \item{df}{estimated approximate degrees of freedom of Zhang and Zhu (2022)'s test.}
#' }
#'
#' @examples
#' set.seed(1234)
#' k <- 3
#' p <- 50
#' n <- c(25, 30, 40)
#' rho <- 0.1
#' M <- matrix(rep(0, k * p), nrow = k, ncol = p)
#' avec <- seq(1, k)
#' Y <- list()
#' for (g in 1:k) {
#'   a <- avec[g]
#'   y <- (-2 * sqrt(a * (1 - rho)) + sqrt(4 * a * (1 - rho) + 4 * p * a * rho)) / (2 * p)
#'   x <- y + sqrt(a * (1 - rho))
#'   Gamma <- matrix(rep(y, p * p), nrow = p)
#'   diag(Gamma) <- rep(x, p)
#'   Z <- matrix(rnorm(n[g] * p, mean = 0, sd = 1), p, n[g])
#'   Y[[g]] <- Gamma %*% Z + t(t(M[g, ])) %*% (rep(1, n[g]))
#' }
#' G <- cbind(diag(k - 1), rep(-1, k - 1))
#' glhtbf_zz2022(Y, G, n, p)
#' @export
glhtbf_zz2022 <- function(Y, G, n, p) {
  stats <- glhtbf_zz2022_cpp(Y, G, n, p)
  stat <- stats[1]
  beta0 <- stats[2]
  beta1 <- stats[3]
  df <- stats[4]
  statn <- (stat - beta0) / beta1
  pvalue <- pchisq(
    q = statn, df = df, ncp = 0, lower.tail = FALSE, log.p = FALSE
  )
  names(stat) <- "statistic"
  names(df) <- "df"
  names(beta1) <- "beta1"
  names(beta0) <- "beta0"
  res <- list(statistic = stat, p.value = pvalue, parameter = c(df, beta1, beta0))
  class(res) <- "htest"
  return(res)
}
