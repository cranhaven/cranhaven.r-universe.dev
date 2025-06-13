# ---------- Multivariate Logistic ---------------
#' Multivariate Logistic Distribution
#' @name MvtLogis
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for multivariate logistic distribution with vector parameter \code{parm1} and vector parameter \code{parm2}.
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param parm1 a vector of location parameters, see parameter \eqn{\mu_i} in \strong{Details}.
#' @param parm2 a vector of scale parameters, see parameters \eqn{\sigma_i} in \strong{Details}.
#' @param k dimension of data or number of variates.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @details
#' Bivariate logistic distribution was introduced by Gumbel (1961) and its multivariate generalization was given by Malik and Abraham (1973) as
#' \deqn{f(x_1, \cdots, x_k) = \frac{k! \exp{(-\sum_{i=1}^{k} \frac{x_i - \mu_i}{\sigma_i})}}{[\prod_{i=1}^{p} \sigma_i] [1 + \sum_{i=1}^{k} \exp{(-\frac{x_i - \mu_i}{\sigma_i})}]^{1+k}},}
#' where \eqn{-\infty<x_i, \mu_i<\infty, \sigma_i > 0, i=1,\cdots, k}.
#'
#' Cumulative distribution function \eqn{F(x_1, \dots, x_k)} is given as
#' \deqn{F(x_1, \cdots, x_k) = \left[1 + \sum_{i=1}^{k} \exp(-\frac{x_i-\mu_i}{\sigma_i})\right]^{-1}.}
#'
#' Equicoordinate quantile is obtained by solving the following equation for \eqn{q} through the built-in one dimension root finding function \code{\link{uniroot}}:
#' \deqn{\int_{-\infty}^{q} \cdots \int_{-\infty}^{q} f(x_1, \cdots, x_k) dx_k \cdots dx_1 = p,}
#' where \eqn{p} is the given cumulative probability.
#'
#' The survival function \eqn{\bar{F}(x_1, \cdots, x_k)} is obtained by the following formula related to cumulative distribution function \eqn{F(x_1, \dots, x_k)} (Joe, 1997)
#' \deqn{\bar{F}(x_1, \cdots, x_k) = 1 + \sum_{S \in \mathcal{S}} (-1)^{|S|} F_S(x_j, j \in S).}
#'
#' Random numbers \eqn{X_1, \cdots, X_k} from multivariate logistic distribution can be generated through transformation of multivariate Lomax random variables \eqn{Y_1, \cdots, Y_k} by letting \eqn{X_i=\mu_i-\sigma_i\ln(\theta_i Y_i), i = 1, \cdots, k}; see Nayak (1987).
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Gumbel, E.J. (1961). Bivariate logistic distribution. \emph{J. Am. Stat. Assoc.}, 56, 335-349.
#'
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Malik, H. J. and Abraham, B. (1973). Multivariate logistic distributions. \emph{Ann. Statist.} 3, 588-590.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#'
#' @return \code{dmvlogis} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the multivariate logistic distribution with parameters:
#' # mu1 = 0.5, mu2 = 1, mu3 = 2, sigma1 = 1, sigma2 = 2 and sigma3 = 3
#' # Vector of quantiles: c(3, 2, 1)
#'
#' dmvlogis(x = c(3, 2, 1), parm1 = c(0.5, 1, 2), parm2 = c(1, 2, 3)) # Density
#'
#' @export
#'
dmvlogis <- function(x, parm1 = rep(1, k), parm2 = rep(1, k), log = FALSE) {
  if (!is.vector(x, mode = "numeric") && !is.matrix(x)) {
    stop(sQuote("x"), " must be a vector or matrix of quantiles")
  }
  if (is.vector(x)) {
    x <- matrix(x, ncol = length(x))
  }
  k <- ncol(x)
  if (!is.vector(parm1, mode = "numeric")) {
    stop(sQuote("parm1"), " must be a vector of numbers")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm1)) {
    stop("x and parm1 have non-conforming size")
  }
  if (k != length(parm2)) {
    stop("x and parm2 have non-conforming size")
  }

  y <- sweep(x, 2, parm1, FUN = "-")
  y <- as.matrix(sweep(y, 2, parm2, FUN = "/"))

  logretval <- as.vector(lgamma(k + 1) - y %*% rep(1, k) - sum(log(parm2)) -
    (k + 1) * log(1 + exp(-y) %*% rep(1, k)))

  if (log)
    logretval
  else exp(logretval)
}

#' @rdname MvtLogis
#' @param q a vector of quantiles.
#' @return
#' \code{pmvlogis} gives the cumulative probability.
#' @importFrom utils combn
#' @examples
#' pmvlogis(q = c(3, 2, 1), parm1 = c(0.5, 1, 2), parm2 = c(1, 2, 3)) # Cumulative Probability
#'
#' @export
#'
pmvlogis <- function(q, parm1 = rep(1, k), parm2 = rep(1, k)){
  if (!is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of quantiles")
  }
  k <- length(q)
  if (!is.vector(parm1, mode = "numeric")) {
    stop(sQuote("parm1"), " must be a vector of numbers")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm1)) {
    stop("q and parm1 have non-conforming size")
  }
  if (k != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }

  y <- (q - parm1) / parm2
  CDF <- (1 + sum(exp(-y)))^(-1)
  return(CDF)
}

#' @rdname MvtLogis
#' @param p a scalar value corresponding to probability.
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(0, 1e8)}.
#' @return
#' \code{qmvlogis} gives the equicoordinate quantile.
#' @examples
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvlogis(p = 0.5, parm1 = c(0.5, 1, 2), parm2 = c(1, 2, 3))
#'
#' @importFrom stats uniroot
#' @export
qmvlogis <- function(p, parm1 = rep(1, k), parm2 = rep(1, k), interval = c(0, 1e8)) {

  if (length(p) != 1 || p <= 0 || p >= 1) {
    stop(sQuote("p"), " is not a double between zero and one")
  }
  if (missing(parm1)) {
    stop(sQuote("parm1"), " is a required argument")
  }
  if (!is.vector(parm1, mode = "numeric")) {
    stop(sQuote("parm1"), " must be a vector of numbers")
  }
  k <- length(parm1)
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("parm1 and parm2 have non-conforming size")
  }
  if (!is.vector(interval) || length(interval) != 2 || interval[1] > interval[2]) {
    stop(sQuote("interval"), " must be a vector of valid range")
  }

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvlogis(equi.x, parm1 = parm1, parm2 = parm2) - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      stop("No quantile was found in provided interval")
  }

  return(find.root$root)
}

#' @rdname MvtLogis
#' @param n number of observations.
#' @return \code{rmvlogis} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvlogis(n = 100, parm1 = c(0.5, 1, 2), parm2 = c(1, 2, 3)) 
#'
#' @export
rmvlogis<- function(n, parm1 = rep(1, k), parm2 = rep(1, k)) {
  if (!is.numeric(n) || n <= 0 || n %% 1 != 0 || length(n) != 1 ) {
    stop("sample size n must be a positive integer")
  }
  if (missing(parm1)) {
    stop(sQuote("parm1"), " is a required argument")
  }
  if (!is.vector(parm1, mode = "numeric")) {
    stop(sQuote("parm1"), " must be a vector of numbers")
  }
  k <- length(parm1)
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (length(parm1) != length(parm2)) {
    stop("parm1 and parm2 have non-conforming size")
  }

  mvlomax <- rmvlomax(n, parm1 = 1, parm2 = rep(1, k))
  y <- sweep(-log(mvlomax), 2, parm2, FUN = "*")
  retval <- sweep(y, 2, parm1, FUN = "+")
  return(retval)
}

#' @rdname MvtLogis
#' @return
#' \code{smvlogis} gives the value of survival function
#' @examples
#' smvlogis(q = c(3, 2, 1), parm1 = c(0.5, 1, 2), parm2 = c(1, 2, 3)) # Survival function
#'
#' @export
#'
smvlogis <- function(q, parm1 = rep(1, k), parm2 = rep(1, k)) {
  if (!is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of quantiles")
  }
  k <- length(q)
  if (!is.vector(parm1, mode = "numeric")) {
    stop(sQuote("parm1"), " must be a vector of numbers")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm1)) {
    stop("q and parm1 have non-conforming size")
  }
  if (k != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }

  SF <- 1
  Index <- seq(1, k)
  for (i in 1:k) {
    combn.Index <- as.matrix(combn(Index, i), length(combn(Index, i)))
    card <- ncol(combn.Index)
    coef.One <- rep((-1)^i, card)
    sub.cdf <- rep(0, card)
    for (j in 1:card) {
      sub.index <- combn.Index[,j]
      sub.q <- as.vector(q[sub.index])
      sub.parm1 <- parm1[sub.index]
      sub.parm2 <- parm2[sub.index]
      sub.cdf[j] <- pmvlogis(sub.q, parm1 = sub.parm1, parm2 = sub.parm2)
    }
    SF <- SF + sum(coef.One * sub.cdf)
  }
  return(SF)
}
