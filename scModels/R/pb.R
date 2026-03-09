#' Poisson-beta Distribution
#'
#' Density, distribution function, quantile function and random generation for the
#' Poisson-beta distribution: a Poisson distribution whose parameter itself follows
#' a beta distribution. Alpha and beta are the parameters of this specific beta
#' distribution which is scaled on (0, c) in contrast to the usual scaling of the
#' standard beta distribution on (0,1).
#'


#' @param x,q  Vector of (non-negative integer) quantiles
#' @param p  Vector of probabilities
#' @param n  Number of observations
#' @param alpha,beta  Non-negative parameters of the beta distribution (shape1 and shape2)
#' @param c  Numeric scaling parameter of the beta distribution. The standard beta is
#'     scaled on (0,1) (default) and can be transformed to (0,c).
#' @param log,log.p  Logical; if TRUE, probabilities p are given as log(p)
#' @param lower.tail  Logical; if TRUE (default), probabilities are \eqn{P[X \le x]}
#'                        otherwise, \eqn{P[X > x]}.
#' @keywords Poisson-beta distribution
#' @name Poisson-beta
#' @useDynLib scModels
#' @importFrom Rcpp evalCpp sourceCpp
#' @export
#' @examples
#'  X <- dpb(x=0:200, alpha=5, beta=3, c=20)
#'  plot(0:200, X, type='l')
#'  Y <- dpb(0:10, seq(10.0,11.0,by=0.1), seq(30.0,31.0,by=0.1), seq(10.2,11.2,by=0.1))
dpb <- function(x, alpha, beta, c = 1, log = FALSE) {
    cpp_dpb(x, alpha, beta, c, log)
}


#' @rdname Poisson-beta
#' @export
#' @examples
#'  Y <- ppb(q= 0 :200, alpha=5, beta= 3, c=20)
#'  plot(0:200, Y, type="l")
ppb <- function(q, alpha, beta, c = 1, lower.tail = TRUE, log.p = FALSE) {
    cpp_ppb(q, alpha, beta, c, lower.tail, log.p)
}


#' @rdname Poisson-beta
#' @export
#' @examples
#'  Z <- qpb(p= seq(0,1, by= 0.01), alpha=5, beta= 3, c=20)
#'  plot(seq(0,1, by= 0.01),Z, type="l")
qpb <- function(p, alpha, beta, c = 1, lower.tail = TRUE, log.p = FALSE) {
    cpp_qpb(p, alpha, beta, c, lower.tail, log.p)
}


#' @rdname Poisson-beta
#' @export
#' @examples
#'  RV <- rpb(n = 1000, alpha=5, beta= 3, c=20)
#'  plot(0 : 200, X, type="l")
#'  lines(density(RV), col="red")
#'  R2 <- rpb(11, seq(10.0,11.0,by=0.1), seq(30.0,31.0,by=0.1), seq(10.2,11.2,by=0.1))
rpb <- function(n, alpha, beta, c = 1) {
    cpp_rpb(n, alpha, beta, c)
}



#' Inverse Gaussian Distribution
#'
#' random generation function for the inverse Gaussian distribution:
#' Mu and lambda are the parameters of this distribution.
#'
#' @param n  Number of observations
#' @param mu,lambda Non-negative parameters of the inverse Gaussian distribution (mean and shape)
#' @keywords Inverse Gaussian distribution
#' @name Inverse Gaussian
#' @useDynLib scModels
#' @importFrom Rcpp evalCpp sourceCpp
#' @export
#' @examples
#'  RV <- rInvGaus(n = 100, mu = 10, lambda = 2)

rInvGaus <- function(n, mu, lambda) {
    cpp_rInvGaus(n, mu, lambda)
}
