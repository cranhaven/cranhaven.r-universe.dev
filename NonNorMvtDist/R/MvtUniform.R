# ---------- Cook-Johnson's Multivariate Uniform ---------------
#' Cook-Johnson's Multivariate Uniform Distribution
#' @name MvtUniform
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for Cook-Johnson’s multivariate uniform distribution with a scalar parameter \code{parm}.
#'
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param parm a scalar parameter, see parameter \eqn{a} in \strong{Details}.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @return \code{dmvunif} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the Cook-Johnson's multivariate uniform distribution with parameters:
#' # a = 2, dim = 3
#' # Vector of quantiles: c(0.8, 0.5, 0.2)
#'
#' dmvunif(x = c(0.8, 0.5, 0.2), parm = 2) # Density
#'
#' @details
#' Multivariate uniform distribution of Cook and Johnson (1981) is a joint distribution of uniform variables over \eqn{(0,1]} and its probability density is given as
#' \deqn{f(x_1, \cdots, x_k) = \frac{\Gamma(a+k)}{\Gamma(a)a^k}\prod_{i=1}^{k} x_i^{(-1/a)-1} \left[\sum_{i=1}^{k} x_i^{-1/a} - k +1 \right]^{-(a+k)},}
#' where \eqn{0 < x_i <=1, a>0, i=1,\cdots, k}. In fact, Cook-Johnson's uniform distribution is also called Clayton copula (Nelsen, 2006).
#'
#' Cumulative distribution function \eqn{F(x_1, \dots, x_k)} is given as
#' \deqn{F(x_1, \cdots, x_k) = \left[ \sum_{i=1}^{k} x_i^{-1/a} - k + 1 \right]^{-a}.}
#'
#' Equicoordinate quantile is obtained by solving the following equation for \eqn{q} through the built-in one dimension root finding function \code{\link{uniroot}}:
#' \deqn{\int_{0}^{q} \cdots \int_{0}^{q} f(x_1, \cdots, x_k) dx_k \cdots dx_1 = p,}
#' where \eqn{p} is the given cumulative probability.
#'
#' The survival function \eqn{\bar{F}(x_1, \cdots, x_k)} is obtained by the following formula related to cumulative distribution function \eqn{F(x_1, \dots, x_k)} (Joe, 1997)
#' \deqn{\bar{F}(x_1, \cdots, x_k) = 1 + \sum_{S \in \mathcal{S}} (-1)^{|S|} F_S(x_j, j \in S).}
#'
#' Random numbers \eqn{X_1, \cdots, X_k} from Cook-Johnson’s multivariate uniform distribution can be generated through transformation of multivariate Lomax random variables \eqn{Y_1, \cdots, Y_k} by letting \eqn{X_i = (1+\theta_i Y_i)^{-a}, i = 1, \cdots, k}; see Nayak (1987).
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Cook, R. E. and Johnson, M. E. (1981). A family of distributions for modeling non-elliptically symmetric multivariate data. \emph{J.R. Statist. Soc}. B 43, No. 2, 210-218.
#'
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#'
#' Nelsen, R. B. (2006). \emph{An Introduction to Copulas, Second Edition}. New York: Springer.
#'
#' @export
dmvunif <- function(x, parm = 1, log = FALSE) {
  dfun <- function(x) {
    if (any(x < 0) || any(x > 1)) {
      return(-Inf)
    }
    else {
      dlog <- lgamma(parm + k) -lgamma(parm) - k * log(parm) -
              log(x) %*% rep((1 / parm + 1), k)  - (parm + k) * log((x ^ ( -1 / parm)) %*% rep(1, k) - k + 1)
      return(dlog)
    }
  }
  if (!is.vector(x, mode = "numeric") && !is.matrix(x)) {
    stop(sQuote("x"), " must be a vector or matrix of quantiles")
  }
  if (length(parm) != 1) {
    stop(sQuote("parm"), " must be a scalar")
  }
  if (parm <= 0) {
    stop(sQuote("parm"), " must be a positive number")
  }

  if (is.vector(x)) {
    x <- matrix(x, ncol = length(x))
  }

  k <- ncol(x)

  logretval <- apply(x, 1, dfun)

  if (log)
    logretval
  else exp(logretval)
}


#' @rdname MvtUniform
#' @param q a vector of quantiles.
#' @return
#' \code{pmvunif} gives the cumulative probability.
#' @importFrom utils combn
#' @examples
#' pmvunif(q = c(0.8, 0.5, 0.2), parm = 2) # Cumulative Probability
#'
#' @export
#'
pmvunif <- function(q, parm = 1){
  if (!is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of quantiles")
  }
  if (any(q < 0) || any(q > 1)) {
    stop(sQuote("q"), " must be a vector that each element in [0, 1]")
  }
  if (length(parm) != 1) {
    stop(sQuote("parm"), " must be a scalar")
  }
  if (parm <= 0) {
    stop(sQuote("parm"), " must be a positive number")
  }

  k <- length(q)
  CDF <- (sum(q ^ (-1 / parm)) - k + 1)^(-parm)
  return(CDF)
}

#' @rdname MvtUniform
#' @param p a scalar value corresponding to probability.
#' @param dim dimension of data or number of variates (k).
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(0, 1)}.
#' @return
#' \code{qmvunif} gives the equicoordinate quantile.
#' @examples
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvunif(p = 0.5, parm = 2, dim = 3)
#'
#' @importFrom stats uniroot
#' @export
qmvunif <- function(p, parm = 1, dim = k, interval = c(0, 1)) {

  if (length(p) != 1 || p <= 0 || p >= 1) {
    stop(sQuote("p"), " is not a double between zero and one")
  }
  if (length(parm) != 1) {
    stop(sQuote("parm"), " must be a scalar")
  }
  if (parm <= 0) {
    stop(sQuote("parm"), " must be a positive number")
  }
  if (missing(dim)) {
    stop(sQuote("dim"), " is a required argument")
  }
  if (length(dim) != 1) {
    stop(sQuote("dim"), " must be a scalar")
  }
  if (dim %% 1 != 0 || dim <= 0) {
    stop(sQuote("dim"), " must be a positive integer")
  }
  if (!is.vector(interval) || length(interval) != 2 || interval[1] > interval[2]) {
    stop(sQuote("interval"), " must be a vector of valid range")
  }

  k <- dim

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvunif(equi.x, parm = parm) - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      stop("No quantile was found in provided interval")
  }

  return(find.root$root)
}

#' @rdname MvtUniform
#' @param n number of observations.
#' @return \code{rmvunif} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvunif(n = 100, parm = 2, dim = 3)
#'
#' @export
rmvunif<- function(n, parm = 1, dim = 1) {
  if (!is.numeric(n) || n <= 0 || n %% 1 != 0 || length(n) != 1 ) {
    stop("sample size n must be a positive integer")
  }
  if (length(parm) != 1) {
    stop(sQuote("parm"), " must be a scalar")
  }
  if (parm <= 0) {
    stop(sQuote("parm"), " must be a positive number")
  }
  if (missing(dim)) {
    stop(sQuote("dim"), " is a required argument")
  }
  if (length(dim) != 1) {
    stop(sQuote("dim"), " must be a scalar")
  }
  if (dim %% 1 != 0 || dim <= 0) {
    stop(sQuote("dim"), " must be a positive integer")
  }

  mvlomax <- rmvlomax(n, parm1 = parm, parm2 = rep(1, dim))
  retval <- (1 + mvlomax) ^ ( - parm)
  return(retval)
}

#' @rdname MvtUniform
#' @return
#' \code{smvunif} gives the value of survival function.
#' @examples
#' smvunif(q = c(0.8, 0.5, 0.2), parm = 3) # Survival function
#'
#' @export
#'
smvunif <- function(q, parm = 1) {
  if (!is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of quantiles")
  }
  if (any(q < 0) || any(q > 1)) {
    stop(sQuote("q"), " must be a vector that each element in [0, 1]")
  }
  if (length(parm) != 1) {
    stop(sQuote("parm"), " must be a scalar")
  }
  if (parm <= 0) {
    stop(sQuote("parm"), " must be a positive number")
  }

  k <- length(q)
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
      sub.cdf[j] <- pmvunif(sub.q, parm = parm)
    }
    SF <- SF + sum(coef.One * sub.cdf)
  }
  return(SF)
}
