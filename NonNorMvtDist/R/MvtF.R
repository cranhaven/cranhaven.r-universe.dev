# ---------- Multivariate F ---------------
#' Multivariate F Distribution
#' @name MvtF
#' @description Calculation of density function, cumulative distribution function, equicoordinate quantile function and survival function, and random numbers generation for multivariate \eqn{F} distribution with degrees of freedom \code{df}.
#'
#' @param x vector or matrix of quantiles. If \eqn{x} is a matrix, each row vector constitutes a vector of quantiles for which the density \eqn{f(x)} is calculated (for \eqn{i}-th row \eqn{x_i}, \eqn{f(x_i)} is reported).
#' @param df a vector of \eqn{k+1} degrees of freedom, see parameter \eqn{(2a, 2l_1, \ldots, 2l_k)} in \strong{Details}.
#' @param k dimension of data or number of variates.
#' @param log logical; if TRUE, probability densities \eqn{f} are given as \eqn{log(f)}.
#'
#' @return \code{dmvf} gives the numerical values of the probability density.
#'
#' @examples
#' # Calculations for the multivariate F with degrees of freedom:
#' # df = c(2, 4, 6)
#' # Vector of quantiles:  c(1, 2)
#'
#' dmvf(x = c(1, 2), df = c(2, 4, 6)) # Density
#'
#' @details
#' Multivariate \eqn{F} distribution (Johnson and Kotz, 1972) is a joint probability distribution of positive random variables and its probability density is given as
#' \deqn{f(x_1, \cdots, x_k) = \frac{[ \prod_{i=1}^{k} (l_i/a)^{l_i}] \Gamma(\sum_{i=1}^{k} l_i + a) \prod_{i=1}^{k} x_i^{l_i-1}}{\Gamma(a) [ \prod_{i=1}^{k} \Gamma(l_i)] (1+\sum_{i=1}^{k} \frac{l_i}{a}x_i )^{\sum_{i=1}^{k} l_i + a}},}
#' where \eqn{x_i>0, a>0, l_i>0, i=1,\cdots, k}. The degrees of freedom are \eqn{(2a, 2l_1,\dots,2l_k)}.
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
#' Random numbers \eqn{X_1, \cdots, X_k} from multivariate F distribution can be generated through parameter substitutions in simulation of generalized multivariate Lomax distribution by letting \eqn{\theta_i = l_i/a, i = 1, \cdots, k}; see Nayak (1987).
#'
#' @seealso \code{\link{uniroot}} for one dimensional root (zero) finding.
#'
#' @references
#' Joe, H. (1997). \emph{Multivariate Models and Dependence Concepts}. London: Chapman & Hall.
#'
#' Johnson, N. L. and Kotz, S. (1972). \emph{Distribution in Statistics: Continuous Multivariate Distributions}. New York: John Wiley & Sons, INC.
#'
#' Narasimhan, B.,  Koller, M., Johnson, S. G., Hahn, T., Bouvier, A., Kiêu, K. and Gaure, S. (2018). cubature: Adaptive Multivariate Integration over Hypercubes. R package version 2.0.3.
#'
#' Nayak, T. K. (1987). Multivariate Lomax Distribution: Properties and Usefulness in Reliability Theory. \emph{Journal of Applied Probability}, Vol. 24, No. 1, 170-177.
#' @export
dmvf <- function(x, df = rep(1, k + 1), log = FALSE) {
  dfun <- function(x) {
    if (any(x < 0)) {
      return(-Inf)
    }
    else {
      dlog <- sum(l * log(theta)) + lgamma(sum(l) + a) +
                  log(x) %*% (l - 1)  - lgamma(a) - sum(lgamma(l)) -
                  (sum(l) + a) * log(1 + x %*% theta)
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
  if (any(df <= 0) || !is.vector(df, mode = "numeric")) {
    stop(sQuote("df"), " must be a vector of positive numbers")
  }
  if (length(df) != (k + 1)) {
    stop("x and df have non-conforming size")
  }

  a <- df[1] / 2
  l <- df[ - 1] / 2
  theta <- l / a
  logretval <- apply(x, 1, dfun)

  if (log)
    logretval
  else exp(logretval)
}

#' @rdname MvtF
#' @param q a vector of quantiles.
#' @param algorithm method to be used for calculating cumulative probability. Two options are provided as (i) \code{numerical} using adaptive multivariate integral and (ii) \code{MC} using Monte Carlo method. Recommend algorithm \code{numerical} for \eqn{(k <= 4)} dimension and \code{MC} for \eqn{(k > 4)} dimension based on running time consumption. Default option is set as \code{numerical}.
#' @param nsim number of simulations used in algorithm \code{MC}.
#' @return \code{pmvf} gives a list of two items:
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
#' pmvf(q = c(1, 2), df = c(2, 4, 6), algorithm = "numerical")
#'
#' \donttest{
#' # Cumulative Probability using Monte Carlo method
#' pmvf(q = c(1, 2), df = c(2, 4, 6), algorithm = "MC")}
#'
#' @export
pmvf <- function(q, df = rep(1, k + 1), algorithm = c("numerical", "MC"), nsim = 1e7) {
  if (any(q < 0) ||  !is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of non-negative quantiles")
  }
  k <- length(q)
  if (any(df <= 0) || !is.vector(df, mode = "numeric")) {
    stop(sQuote("df"), " must be a vector of positive numbers")
  }
  if (length(df) != (k + 1)) {
    stop("q and df have non-conforming size")
  }

  method <- match.arg(algorithm, c("numerical", "MC"))

  if (method == "numerical") {
    a <- df[1] / 2
    l <- df[ - 1] / 2
    theta <- q * l / a
    CDF <- hcubature(dmvglomax, lowerLimit=rep(0, k), upperLimit = rep(1, k), parm1 = a, parm2 = theta, parm3 = l)
    return(list(value = as.vector(CDF$integral), error=as.vector(CDF$error)))
  }
  if (method == "MC") {
  	if (nsim %% 1 != 0 || nsim <= 0){
  		stop(sQuote("nsim"), " must be a positive integer")
  	}
    mcSample <- rmvf(n = nsim, df = df)
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

#' @rdname MvtF
#' @param p a scalar value corresponding to probability.
#' @param interval a vector containing the end-points of the interval to be searched. Default value is set as \code{c(1e-8, 1e8)}.
#' @return \code{qmvf} gives the equicoordinate quantile. \code{NaN} is returned for no solution found in the given interval. The result is seed dependent if Monte Carlo algorithm is chosen  (\code{algorithm = "MC"}).
#' @examples
#' \donttest{
#' # Equicoordinate quantile of cumulative probability 0.5
#' qmvf(p = 0.5, df = c(2, 4, 6))}
#'
#' @importFrom stats uniroot
#' @export
qmvf <- function(p, df = rep(1, k + 1), interval = c(1e-8, 1e8), algorithm = c("numerical", "MC"), nsim = 1e6) {

  if (length(p) != 1 || p <= 0 || p >= 1) {
    stop(sQuote("p"), " is not a double between zero and one")
  }
  if (missing(df)) {
    stop(sQuote("df"), " is a required argument")
  }
  if (any(df <= 0) || !is.vector(df, mode = "numeric")) {
    stop(sQuote("df"), " must be a vector of positive numbers")
  }
  if (!is.vector(interval) || length(interval) != 2 || interval[1] > interval[2]) {
    stop(sQuote("interval"), " must be a vector of valid range")
  }

  k <- length(df) - 1

  method <- match.arg(algorithm, c("numerical", "MC"))

  rootfun <- function(x) {
    equi.x <- rep(x, k)
    val <- pmvf(equi.x, df = df, algorithm = method, nsim = nsim)$value - p
  }

  find.root <- tryCatch(uniroot(rootfun, interval = interval), error = function(e) e)

  if (inherits(find.root, "error")) {
    if (find.root$message == "f() values at end points not of opposite sign")
      print("No quantile was found in provided interval")
      return(NaN)
  }

  return(find.root$root)

}

#' @rdname MvtF
#' @param n number of observations.
#' @return \code{rmvf} generates random numbers.
#' @examples
#' # Random numbers generation with sample size 100
#' rmvf(n = 100, df = c(2, 4, 6))
#'
#' @export
rmvf <- function(n, df = rep(1, k + 1)) {
  if (!is.numeric(n) || n <= 0 || n %% 1 != 0 || length(n) != 1 ) {
    stop("sample size n must be a positive integer")
  }
  if (missing(df)) {
    stop(sQuote("df"), " is a required argument")
  }
  if (any(df <= 0) || !is.vector(df, mode = "numeric")) {
    stop(sQuote("df"), " must be a vector of positive numbers")
  }

  k <- length(df) - 1
  a <- df[1] / 2
  l <- df[ - 1] / 2
  theta <- l / a
  retval <- rmvglomax(n, parm1 = a, parm2 = theta, parm3 = l)
  return(retval)
}

#' @rdname MvtF
#' @return \code{smvf} gives a list of two items:
#'
#' \eqn{\quad} \code{value} the value of survial function
#'
#' \eqn{\quad} \code{error} the estimated relative error by \code{algorithm = "numerical"} or the estimated standard error by \code{algorithm = "MC"}
#'
#' @examples
#' smvf(q = c(1, 2), df = c(2, 4, 6)) # Survival function
#'
#' @importFrom stats sd
#'
#' @export
smvf <- function(q, df = rep(1, k + 1), algorithm = c("numerical", "MC"), nsim = 1e7) {
  if (any(q < 0) ||  !is.vector(q, mode = "numeric")) {
    stop(sQuote("q"), " must be a vector of non-negative quantiles")
  }
  k <- length(q)
  if (any(df <= 0) || !is.vector(df, mode = "numeric")) {
    stop(sQuote("df"), " must be a vector of positive numbers")
  }
  if (length(df) != (k + 1)) {
    stop("q and df have non-conforming size")
  }

  v0 = df[1]
  vn = df[-1]

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
        sub.vn <- vn[sub.index]
        sub.calc <- pmvf(sub.q, df = c(v0, sub.vn), algorithm = method, nsim = nsim)
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
    mcSample <- rmvf(n = nsim, df = df)
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
