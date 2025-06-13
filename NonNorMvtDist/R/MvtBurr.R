# ---------- Multivariate Burr ---------------
#' Multivariate Burr Distribution
#' @name MvtBurr
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for multivariate Burr distribution with a scalar parameter \code{parm1} and vectors of parameters \code{parm2} and \code{parm3}.
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param parm1 a scalar parameter, see parameter \eqn{a} in \strong{Details}.
#' @param parm2 a vector of parameters, see parameters \eqn{d_i} in \strong{Details}.
#' @param parm3 a vector of parameters, see parameters \eqn{c_i} in \strong{Details}.
#' @param k dimension of data or number of variates.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @details
#' Multivariate Burr distribution (Johnson and Kotz, 1972) is a joint distribution of positive random variables \eqn{X_1, \cdots, X_k}. Its probability density is given as
#' \deqn{f(x_1, \cdots, x_k) = \frac{[ \prod_{i=1}^{k} c_i d_i] a(a+1) \cdots (a+k-1) [ \prod_{i=1}^{k} x_i^{c_i-1}]}{(1 + \sum_{i=1}^{k} d_i x_i^{c_i})^{a+k}},}
#' where \eqn{x_i >0, a,c_i,d_i>0, i=1,\cdots, k}.
#'
#' Cumulative distribution function \eqn{F(x_1, \dots, x_k)} is obtained by the following formula related to survival function \eqn{\bar{F}(x_1, \dots, x_k)} (Joe, 1997)
#' \deqn{F(x_1, \dots, x_k) = 1 + \sum_{S \in \mathcal{S}} (-1)^{|S|} \bar{F}_S(x_j, j \in S),}
#' where the survival function is given by
#' \deqn{\bar{F}(x_1, \cdots, x_k) = \left( 1+\sum_{i=1}^{k} d_i x_i^{c_i} \right)^{-a}.}
#'
#' Equicoordinate quantile is obtained by solving the following equation for \eqn{q} through the built-in one dimension root finding function \code{\link{uniroot}}:
#' \deqn{\int_{0}^{q} \cdots \int_{0}^{q} f(x_1, \cdots, x_k) dx_k \cdots dx_1 = p,}
#' where \eqn{p} is the given cumulative probability.
#'
#' Random numbers \eqn{X_1, \cdots, X_k} from multivariate Burr distribution can be generated through transformation of multivariate Lomax random variables \eqn{Y_1, \cdots, Y_k} by letting \eqn{X_i=(\theta_i Y_i/d_i)^{1/c_i}, i = 1, \cdots, k}; see Nayak (1987).
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Johnson, N. L. and Kotz, S. (1972). \emph{Distribution in Statistics: Continuous Multivariate Distributions}. New York: John Wiley & Sons, INC.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#'
#' @return \code{dmvburr} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the multivariate Burr with parameters:
#' # a = 3, d1 = 1, d2 = 3, d3 = 5, c1 = 2, c2 = 4, c3 = 6
#' # Vector of quantiles: c(3, 2, 1)
#'
#' dmvburr(x = c(3, 2, 1), parm1 = 3, parm2 = c(1, 3, 5), parm3 = c(2, 4, 6)) # Density
#'
#' @export
#'
dmvburr<- function(x, parm1 = 1, parm2 = rep(1, k), parm3 = rep(1, k), log = FALSE) {
  dfun <- function(x) {
    if (any(x < 0)) {
      return(-Inf)
    }
    else {
      dlog <- log(prod(parm2) * prod(parm3)) + log(prod(parm1:(parm1 + k - 1))) +
              sum(log(x) * (parm3 - 1)) - (parm1 + k) * log(1 +  sum((x ^ parm3) * parm2))
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

  if (any(parm3 <= 0) || !is.vector(parm3, mode = "numeric")) {
    stop(sQuote("parm3"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("x and parm2 have non-conforming size")
  }
  if (k != length(parm3)) {
    stop("x and parm3 have non-conforming size")
  }

  logretval <- apply(x, 1, dfun)

  if (log)
    logretval
  else exp(logretval)
}

#' @rdname MvtBurr
#' @param q a vector of quantiles.
#' @return
#' \code{pmvburr} gives the cumulative probability.
#' @importFrom utils combn
#' @examples
#' pmvburr(q = c(3, 2, 1), parm1 = 3, parm2 = c(1, 3, 5), parm3 = c(2, 4, 6)) # Cumulative Probability
#'
#' @export
#'
pmvburr <- function(q, parm1 = 1, parm2 = rep(1, k), parm3 = rep(1, k)){
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
  if (any(parm3 <= 0) || !is.vector(parm3, mode = "numeric")) {
    stop(sQuote("parm3"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }
  if (k != length(parm3)) {
    stop("q and parm3 have non-conforming size")
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
      sub.parm3 <- parm3[sub.index]
      sub.surv[j] <- smvburr(sub.q, parm1 = parm1, parm2 = sub.parm2, parm3 = sub.parm3)
    }
    CDF <- CDF + sum(coef.One * sub.surv)
  }
  return(CDF)
}

#' @rdname MvtBurr
#' @param p a scalar value corresponding to probability.
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(0, 1e8)}.
#' @return
#' \code{qmvburr} gives the equicoordinate quantile.
#' @examples
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvburr(p = 0.5, parm1 = 3, parm2 = c(1, 3, 5), parm3 = c(2, 4, 6))
#'
#' @importFrom stats uniroot
#' @export
qmvburr <- function(p, parm1 = 1, parm2 = rep(1, k), parm3 = rep(1, k), interval = c(0, 1e8)) {

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
  if (any(parm3 <= 0) || !is.vector(parm3, mode = "numeric")) {
    stop(sQuote("parm3"), " must be a vector of positive numbers")
  }
  if (k != length(parm3)) {
    stop("parm2 and parm3 have non-conforming size")
  }
  if (!is.vector(interval) || length(interval) != 2 || interval[1] > interval[2]) {
    stop(sQuote("interval"), " must be a vector of valid range")
  }

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvburr(equi.x, parm1 = parm1, parm2 = parm2, parm3 = parm3) - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      stop("No quantile was found in provided interval")
  }

  return(find.root$root)
}

#' @rdname MvtBurr
#' @param n number of observations.
#' @return \code{rmvburr} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvburr(n = 100, parm1 = 3, parm2 = c(1, 3, 5), parm3 = c(2, 4, 6))
#'
#' @export
rmvburr<- function(n, parm1 = 1, parm2 = rep(1, k), parm3 = rep(1, k)) {
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
  if (any(parm3 <= 0) || !is.vector(parm3, mode = "numeric")) {
    stop(sQuote("parm3"), " must be a vector of positive numbers")
  }
  if (k != length(parm3)) {
    stop("parm2 and parm3 have non-conforming size")
  }

  retval <- rmvlomax(n, parm1 = parm1, parm2 = rep(1, k))
  retval <- sweep(retval, 2, parm2, FUN = "/")
  retval <- sweep(retval, 2, 1 / parm3, FUN = "^")
  return(retval)
}

#' @rdname MvtBurr
#' @return \code{smvburr} gives the value of survival function.
#' @examples
#' smvburr(q = c(3, 2, 1), parm1 = 3, parm2 = c(1, 3, 5), parm3 = c(2, 4, 6)) # Survival function
#'
#' @export
#'
smvburr <- function(q, parm1 = 1, parm2 = rep(1, k), parm3 = rep(1, k)) {
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
  if (any(parm3 <= 0) || !is.vector(parm3, mode = "numeric")) {
    stop(sQuote("parm3"), " must be a vector of positive numbers")
  }
  if (k != length(parm2)) {
    stop("q and parm2 have non-conforming size")
  }
  if (k != length(parm3)) {
    stop("q and parm3 have non-conforming size")
  }

  retval <- (1 + sum(parm2 * (q ^ parm3)))^(-parm1)
  return(retval)
}
