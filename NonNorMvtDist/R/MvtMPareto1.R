# ---------- Mardia's Multivariate Pareto Type I ---------------
#' Mardia's Multivariate Pareto Type I Distribution
#' @name MvtMardiaPareto1
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for Mardia's multivariate Pareto Type I distribution with a scalar parameter \code{parm1} and a vector of parameters \code{parm2}.
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param parm1 a scalar parameter, see parameter \eqn{a} in \strong{Details}.
#' @param parm2 a vector of parameters, see parameters \eqn{\theta_i} in \strong{Details}.
#' @param k dimension of data or number of variates.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @details
#' Multivariate Pareto type I distribution was introduced by Mardia (1962) as a joint probability distribution of several nonnegative random variables \eqn{X_1, \cdots, X_k}. Its probability density function is given by
#' \deqn{f(x_1, \cdots, x_k) = \frac{[ \prod_{i=1}^{k} \theta_i] a(a+1) \cdots (a+k-1)}{(\sum_{i=1}^{k} \theta_i x_i - k + 1)^{a+k}},}
#' where \eqn{x_i > 1 / \theta_i, a >0, \theta_i>0, i=1,\cdots, k}.
#'
#' Cumulative distribution function \eqn{F(x_1, \dots, x_k)} is obtained by the following formula related to survival function \eqn{\bar{F}(x_1, \dots, x_k)} (Joe, 1997)
#' \deqn{F(x_1, \dots, x_k) = 1 + \sum_{S \in \mathcal{S}} (-1)^{|S|} \bar{F}_S(x_j, j \in S),}
#' where the survival function is given by
#' \deqn{\bar{F}(x_1, \cdots, x_k) = \left( \sum_{i=1}^{k} \theta_i x_i - k + 1 \right)^{-a}.}
#'
#' Equicoordinate quantile is obtained by solving the following equation for \eqn{q} through the built-in one dimension root finding function \code{\link{uniroot}}:
#' \deqn{\int_{0}^{q} \cdots \int_{0}^{q} f(x_1, \cdots, x_k) dx_k \cdots dx_1 = p,}
#' where \eqn{p} is the given cumulative probability.
#'
#' Random numbers \eqn{X_1, \cdots, X_k} from Mardia's multivariate Pareto type I distribution can be generated through linear transformation of multivariate Lomax random variables \eqn{Y_1, \cdots, Y_k} by letting \eqn{X_i = Y_i + 1/\theta_i, i = 1, \cdots, k}; see Nayak (1987).
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Mardia, K. V. (1962). Multivariate Pareto distributions. \emph{Ann. Math. Statist.} 33, 1008-1015.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#'
#' @return \code{dmvmpareto1} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the Mardia's multivariate Pareto Type I with parameters:
#' # a = 5, theta1 = 1, theta2 = 2, theta3 = 3
#' # Vector of quantiles: c(2, 1, 1)
#'
#' dmvmpareto1(x = c(2, 1, 1), parm1 = 5, parm2 = c(1, 2, 3)) # Density
#'
#' @export
dmvmpareto1<- function(x, parm1 = 1, parm2 = rep(1, k), log = FALSE) {
  dfun <- function(x) {
    if (any(x < 1 / parm2)) {
      return(-Inf)
    }
    else {
      dlog <- sum(log(parm2)) + log(prod(parm1:(parm1 + k - 1))) -
        (parm1 + k) * log(x %*% parm2 - k + 1)
      return(dlog)
    }
  }
  if (!is.vector(x, mode = "numeric") && !is.matrix(x)) {
    stop(sQuote("x"), " must be a vector or matrix of quantiles")
  }
  if (is.vector(x)) {
    x <- matrix(x, ncol = length(x))
  }

  k <- ncol(x)
  if (length(parm1) != 1) {
    stop(sQuote("parm1"), " must be a scalar")
  }
  if (parm1 <= 0) {
    stop(sQuote("parm1"), " must be a positive number")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("x and parm2 have non-conforming size")
  }

  logretval <- apply(x, 1, dfun)

  if (log)
    logretval
  else exp(logretval)
}

#' @rdname MvtMardiaPareto1
#' @param q a vector of quantiles.
#' @return
#' \code{pmvmpareto1} gives the cumulative probability.
#' @importFrom utils combn
#' @examples
#' pmvmpareto1(q = c(2, 1, 1), parm1 = 5, parm2 = c(1, 2, 3)) # Cumulative Probability
#'
#' @export
#'
pmvmpareto1 <- function(q, parm1 = 1, parm2 = rep(1, k)){
  if (any(q < 0) ||  !is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of non-negative quantiles")
  }
  k <- length(q)
  if (length(parm1) != 1) {
    stop(sQuote("parm1"), " must be a scalar")
  }
  if (parm1 <= 0) {
    stop(sQuote("parm1"), " must be a positive number")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }
  if (any(q < 1 / parm2)) {
    stop(sQuote("q"), " must be greater than reciprocal of parm2")
  }

  CDF <- 1
  Index <- seq(1,k)
  for (i in 1:k) {
    combn.Index <- as.matrix(combn(Index, i), length(combn(Index, i)))
    card <- ncol(combn.Index)
    coef.One <- rep((-1)^i, card)
    sub.surv <- rep(0, card)
    for (j in 1:card) {
      sub.index <- combn.Index[,j]
      sub.q <- as.vector(q[sub.index])
      sub.parm2 <- parm2[sub.index]
      sub.surv[j] <- smvmpareto1(sub.q, parm1 = parm1, parm2 = sub.parm2)
    }
    CDF <- CDF + sum(coef.One * sub.surv)
  }
  return(CDF)
}

#' @rdname MvtMardiaPareto1
#' @param p a scalar value corresponding to probability.
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(max(1 / parm2) + 1e-8, 1e8)} according to \eqn{x_i > 1 / \theta_i, \theta_i>0, i=1,\cdots, k}.
#' @return
#' \code{qmvmpareto1} gives the equicoordinate quantile.
#' @examples
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvmpareto1(p = 0.5, parm1 = 5, parm2 =  c(1, 2, 3))
#'
#' @importFrom stats uniroot
#' @export
qmvmpareto1 <- function(p, parm1 = 1, parm2 = rep(1, k), interval = c(max(1 / parm2) + 1e-8, 1e8)) {

  if (length(p) != 1 || p <= 0 || p >= 1) {
    stop(sQuote("p"), " is not a double between zero and one")
  }
  if (length(parm1) != 1) {
    stop(sQuote("parm1"), " must be a scalar")
  }
  if (parm1 <= 0) {
    stop(sQuote("parm1"), " must be a positive number")
  }
  if (missing(parm2)) {
    stop(sQuote("parm2"), " is a required argument")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  k <- length(parm2)
  if (!is.vector(interval) || length(interval) != 2 || interval[1] > interval[2]) {
    stop(sQuote("interval"), " must be a vector of valid range")
  }

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvmpareto1(equi.x, parm1 = parm1, parm2 = parm2) - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      stop("No quantile was found in provided interval")
  }

  return(find.root$root)
}

#' @rdname MvtMardiaPareto1
#' @param n number of observations.
#' @return \code{rmvmpareto1} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvmpareto1(n = 100, parm1 = 5, parm2 = c(1, 2, 3)) 
#'
#' @export
rmvmpareto1<- function(n, parm1 = 1, parm2 = rep(1, k)) {
  if (!is.numeric(n) || n <= 0 || n %% 1 != 0 || length(n) != 1 ) {
    stop("sample size n must be a positive integer")
  }
  if (length(parm1) != 1) {
    stop(sQuote("parm1"), " must be a scalar")
  }
  if (parm1 <= 0) {
    stop(sQuote("parm1"), " must be a positive number")
  }
  if (missing(parm2)) {
    stop(sQuote("parm2"), " is a required argument")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }

  k <- length(parm2)
  mvtlomax <- rmvlomax(n, parm1 = parm1, parm2 = parm2)
  retval <- sweep(mvtlomax, 2, 1 / parm2, FUN = "+")
  return(retval)
}


#' @rdname MvtMardiaPareto1
#' @return
#' \code{smvmpareto1} gives the value of survival function.
#' @examples
#' smvmpareto1(q = c(2, 1, 1), parm1 = 5, parm2 = c(1, 2, 3)) # Survival function
#'
#' @export
#'
smvmpareto1 <- function(q, parm1 = 1, parm2 = rep(1, k)) {
  if (any(q < 0) ||  !is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of non-negative quantiles")
  }
  if (is.vector(q)) {
    q <- matrix(q, ncol = length(q))
  }
  k <- ncol(q)
  if (length(parm1) != 1) {
    stop(sQuote("parm1"), " must be a scalar")
  }
  if (parm1 <= 0) {
    stop(sQuote("parm1"), " must be a positive number")
  }
  if (any(parm2 <= 0) || !is.vector(parm2, mode = "numeric")) {
    stop(sQuote("parm2"), " must be a vector of positive numbers")
  }
  if (length(q) != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }
  if (any(q < 1 / parm2)) {
    stop(sQuote("q"), " must be greater than reciprocal of parm2")
  }
  
  retval <- as.vector((1 + q %*% parm2 - k)^(-parm1))
  return(retval)
}
