# ---------- Multivariate Inverted Beta ---------------
#' Multivariate Inverted Beta Distribution
#' @name MvtInvBeta
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for multivariate inverted beta distribution with a scalar parameter \code{parm1} and a vector of parameters \code{parm2}.
#'
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param parm1 a scalar parameter, see parameter \eqn{a} in \strong{Details}.
#' @param parm2 a vector of parameters, see parameter \eqn{l_i} in \strong{Details}.
#' @param k dimension of data or number of variates.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @return \code{dmvinvbeta} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the multivariate inverted beta with parameters:
#' # a = 7, l1 = 1, l2 = 3
#' # Vector of quantiles: c(2, 4)
#'
#' dmvinvbeta(x = c(2, 4), parm1 = 7, parm2 = c(1, 3)) # Density
#'
#' @details
#' Multivariate inverted beta distribution is an alternative expression of multivariate F distribution and is a special case of multivariate Lomax distribution (Balakrishnan and Lai, 2009). Its probability density is given as
#' \deqn{f(x_1, \cdots, x_p) = \frac{\Gamma(\sum_{i=1}^{p} l_i + a) \prod_{i=1}^{p} x_i^{l_i-1}}{\Gamma(a) [\prod_{i=1}^{p} \Gamma(l_i)] (1+\sum_{i=1}^{p} x_i)^{\sum_{i=1}^{p} l_i + a}},}
#' where \eqn{x_i>0, a>0, l_i>0, i=1,\cdots, p}.
#'
#' Cumulative distribution function \eqn{F(x_1, \dots, x_k)} is obtained by multiple integral
#' \deqn{F(x_1, \dots, x_k) = \int_{0}^{x_1} \cdots  \int_{0}^{x_k} f(y_1, \cdots, y_k) dy_k \cdots dy_1.}
#' This multiple integral is calculated by either adaptive multivariate integration using \code{\link{hcubature}} in package \strong{\link{cubature}} (Narasimhan et al., 2018) or via Monte Carlo method.
#'
#' Equicoordinate quantile is obtained by solving the following equation for \eqn{q} through the built-in one dimension root finding function \code{\link{uniroot}}:
#' \deqn{\int_{0}^{q} \cdots \int_{0}^{q} f(x_1, \cdots, x_k) dx_k \cdots dx_1 = p,}
#' where \eqn{p} is the given cumulative probability.
#'
#' The survival function \eqn{\bar{F}(x_1, \cdots, x_k)} is obtained either by the following formula related to cumulative distribution function \eqn{F(x_1, \dots, x_k)} (Joe, 1997)
#' \deqn{\bar{F}(x_1, \cdots, x_k) = 1 + \sum_{S \in \mathcal{S}} (-1)^{|S|} F_S(x_j, j \in S),}
#' or via Monte Carlo method.
#'
#' Random numbers \eqn{X_1, \cdots, X_k} from multivariate inverted beta distribution can be generated through parameter substitutions in simulation of generalized multivariate Lomax distribution by letting \eqn{\theta_i = 1, i = 1, \cdots, k}.
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Balakrishnan, N. and Lai, C. (2009). \emph{Continuous Bivariate Distributions. 2nd Edition.} New York: Springer.
#'
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Narasimhan, B.,  Koller, M., Johnson, S. G., Hahn, T., Bouvier, A., Kiêu, K. and Gaure, S. (2018). cubature: Adaptive Multivariate Integration over Hypercubes. R package version 2.0.3.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#'
#' @export
dmvinvbeta <- function(x, parm1 = 1, parm2 = rep(1, k), log = FALSE) {
  dfun <- function(x) {
    if (any(x < 0)) {
      return(-Inf)
    }
    else {
      dlog <- lgamma(sum(parm2) + parm1) + log(x) %*% (parm2 - 1)  - lgamma(parm1) - sum(lgamma(parm2)) -
                           (sum(parm2) + parm1) * log(1 + x %*% rep(1, k))
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

#' @rdname MvtInvBeta
#' @param q a vector of quantiles.
#' @param algorithm method to be used for calculating cumulative probability. Two options are provided as (i) \code{numerical} using adaptive multivariate integral and (ii) \code{MC} using Monte Carlo method. Recommend algorithm \code{numerical} for \eqn{(k <= 4)} dimension and \code{MC} for \eqn{(k > 4)} dimension based on running time consumption. Default option is set as \code{numerical}.
#' @param nsim number of simulations used in algorithm \code{MC}.
#' @return \code{pmvinvbeta} gives a list of two items:
#'
#' \eqn{\quad} \code{value} cumulative probability
#'
#' \eqn{\quad} \code{error} the estimated relative error by \code{algorithm = "numerical"} or the estimated standard error by \code{algorithm = "MC"}
#'
#' @importFrom cubature hcubature
#' @importFrom stats sd
#'
#' @examples
#' # Cumulative Probability using adaptive multivariate integral
#' pmvinvbeta(q = c(2, 4), parm1 = 7, parm2 = c(1, 3))
#'
#' \donttest{
#' # Cumulative Probability using Monte Carlo method
#' pmvinvbeta(q = c(2, 4), parm1 = 7, parm2 = c(1, 3), algorithm = "MC")}
#'
#' @export
pmvinvbeta <- function(q, parm1 = 1, parm2 = rep(1, k), algorithm = c("numerical", "MC"), nsim = 1e7) {
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

  method <- match.arg(algorithm, c("numerical", "MC"))

  if (method == "numerical") {
    CDF <- hcubature(dmvglomax, lowerLimit=rep(0, k), upperLimit = rep(1, k), parm1 = parm1, parm2 = q, parm3 = parm2)
    return(list(value = as.vector(CDF$integral), error=as.vector(CDF$error)))
  }

  if (method == "MC") {
  	if (nsim %% 1 != 0 || nsim <= 0){
  		stop(sQuote("nsim"), " must be a positive integer")
  	}
    mcSample <- rmvinvbeta(nsim, parm1 = parm1, parm2 = parm2)
    trueTable <- sweep(mcSample, 2, q, FUN = "<")
    numTable <- 1 * trueTable
    MC.vec <- rep(1, nsim)

    for (i in 1:k) {
      MC.vec <- MC.vec * as.vector(numTable[,i])
    }

    prob = mean(MC.vec)
  }

  return(list(value = prob, error = sd(MC.vec)/sqrt(nsim)))
}

#' @rdname MvtInvBeta
#' @param p a scalar value corresponding to probability.
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(1e-8, 1e8)}.
#' @return \code{qmvinvbeta} gives the equicoordinate quantile. \code{NaN} is returned for no solution found in the given interval. The result is seed dependent if Monte Carlo algorithm is chosen  (\code{algorithm = "MC"}).
#' @examples
#' \donttest{
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvinvbeta(p = 0.5, parm1 = 7, parm2 = c(1, 3))}
#'
#' @importFrom stats uniroot
#' @export
qmvinvbeta <- function(p, parm1 = 1, parm2 = rep(1, k), interval = c(1e-8, 1e8), algorithm = c("numerical", "MC"), nsim = 1e6) {

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

  method <- match.arg(algorithm, c("numerical", "MC"))

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvinvbeta(equi.x, parm1 = parm1, parm2 = parm2, algorithm = method, nsim = nsim)$value - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      print("No quantile was found in provided interval")
      return(NaN)
  }

  return(find.root$root)

}

#' @rdname MvtInvBeta
#' @param n number of observations.
#' @return \code{rmvinvbeta} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvinvbeta(n = 100, parm1 = 7, parm2 = c(1, 3))
#'
#' @export
rmvinvbeta <- function(n, parm1 = 1, parm2 = rep(1, k)) {
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

  retval <- rmvglomax(n, parm1 = parm1, parm2 = rep(1, k), parm3 = parm2)
  return(retval)
}

#' @rdname MvtInvBeta
#' @return \code{smvinvbeta} gives a list of two items:
#'
#' \eqn{\quad} \code{value} the value of survial function
#'
#' \eqn{\quad} \code{error} the estimated relative error by \code{algorithm = "numerical"} or the estimated standard error by \code{algorithm = "MC"}
#'
#' @examples
#' smvinvbeta(q = c(2, 4), parm1 = 7, parm2 = c(1, 3)) # Survival function
#'
#' @importFrom stats sd
#'
#' @export
smvinvbeta <- function(q, parm1 = 1, parm2 = rep(1, k), algorithm = c("numerical", "MC"), nsim = 1e7) {
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

  method <- match.arg(algorithm, c("numerical", "MC"))

  if (method == "numerical") {
    SF <- 1
    SF.error <- 0
    Index <- seq(1, k)
    for (i in 1:k) {
      combn.Index <- as.matrix(combn(Index, i), length(combn(Index, i)))
      card <- ncol(combn.Index)
      coef.One <- rep((-1)^i, card)
      sub.cdf <- rep(0, card)
      sub.error <- rep(0, card)
      for (j in 1:card) {
        sub.index <- combn.Index[,j]
        sub.q <- as.vector(q[sub.index])
        sub.parm2 <- parm2[sub.index]
        sub.calc <- pmvinvbeta(sub.q, parm1 = parm1, parm2 = sub.parm2, algorithm = method, nsim = nsim)
        sub.cdf[j] <- sub.calc$value
        sub.error[j] <- sub.calc$error
      }
      SF <- SF + sum(coef.One * sub.cdf)
      SF.error <- SF.error + sum(sub.error)
    }

    return(list(value = SF, error = SF.error))
  }

  if (method == "MC") {
    if (nsim %% 1 != 0 || nsim <= 0){
      stop(sQuote("nsim"), " must be a positive integer")
    }
    mcSample <- rmvinvbeta(n = nsim, parm1 = parm1, parm2 = parm2)
    trueTable <- sweep(mcSample, 2, q, FUN = ">")
    numTable <- 1 * trueTable
    MC.vec <- rep(1, nsim)

    for (i in 1:k) {
      MC.vec <- MC.vec * as.vector(numTable[,i])
    }

    SF = mean(MC.vec)
    return(list(value = SF, error = sd(MC.vec)/sqrt(nsim)))
  }
}
