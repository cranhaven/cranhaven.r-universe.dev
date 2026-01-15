#' Schur polynomial - C++ implementation
#'
#' @description Returns a Schur polynomial. The Schur polynomials are the
#'   Jack \eqn{P}-polynomials with Jack parameter \eqn{\alpha=1}.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{qspray} multivariate polynomial.
#'
#' @export
#' @importFrom qspray qspray_from_list qone qzero
#'
#' @examples
#' ( schur <- SchurPol(3, lambda = c(3, 1)) )
#' schur == JackPol(3, lambda = c(3, 1), alpha = "1", which = "P")
SchurPol <- function(n, lambda) {
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  lambda <- as.integer(removeTrailingZeros(lambda))
  if(length(lambda) == 0L) {
    qone()
  } else if(n == 0L){
    qzero()
  } else {
    qspray_from_list(SchurPolRcpp(as.integer(n), lambda))
  }
}

#' Evaluation of Schur polynomial - C++ implementation
#'
#' @description Evaluates a Schur polynomial. The Schur polynomials are the
#'   Jack \eqn{P}-polynomials with Jack parameter \eqn{\alpha=1}.
#'
#' @param x values of the variables, a vector of \code{bigq} numbers, or a
#'   vector that can be coerced as such (e.g. \code{c("2", "5/3")})
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{bigq} number.
#'
#' @export
#' @importFrom gmp as.bigq
#'
#' @examples
#' Schur(c("1", "3/2", "-2/3"), lambda = c(3, 1))
Schur <- function(x, lambda) {
  stopifnot(isPartition(lambda))
  lambda <- as.integer(removeTrailingZeros(lambda))
  if(is.numeric(x)) {
    if(anyNA(x)) {
      stop("Found missing values in `x`.")
    }
    SchurEvalRcpp_double(as.double(x), lambda)
  } else {
    x <- as.character(as.bigq(x))
    if(anyNA(x)) {
      stop("Found missing values in `x`.")
    }
    res <- SchurEvalRcpp_gmpq(x, lambda)
    as.bigq(res)
  }
}

#' Jack polynomial - C++ implementation
#'
#' Returns a Jack polynomial.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#' @param alpha rational number, given as a string such as
#'   \code{"2/3"} or as a \code{bigq} number
#' @param which which Jack polynomial, \code{"J"}, \code{"P"}, \code{"Q"},
#'   or \code{"C"}
#'
#' @return A \code{qspray} multivariate polynomial.
#'
#' @export
#' @importFrom gmp as.bigq factorialZ
#' @importFrom qspray qspray_from_list ESFpoly qone qzero
#'
#' @examples
#' JackPol(3, lambda = c(3, 1), alpha = "2/5")
JackPol <- function(n, lambda, alpha, which = "J") {
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  lambda <- as.integer(removeTrailingZeros(lambda))
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid `alpha`.")
  }
  if(length(lambda) == 0L) {
    return(qone())
  }
  if(n == 0L){
    return(qzero())
  }
  which <- match.arg(which, c("J", "P", "Q", "C"))
  alpha <- as.character(alpha)
  if(alpha == "0") {
    lambdaPrime <- dualPartition(lambda)
    f <- prod(factorialZ(lambdaPrime))
    JackPolynomial <- f * ESFpoly(n, lambdaPrime)
  } else {
    JackPolynomial <-
      qspray_from_list(JackPolRcpp(as.integer(n), lambda, alpha))
  }
  if(which != "J") {
    K <- switch(
      which,
      "P" = 1L / prod(hookLengths_gmp(lambda, alpha)[1L, ]),
      "Q" = 1L / prod(hookLengths_gmp(lambda, alpha)[2L, ]),
      "C" = JackCcoefficient(lambda, alpha)
    )
    JackPolynomial <- K * JackPolynomial
  }
  JackPolynomial
}

#' Evaluation of Jack polynomial - C++ implementation
#'
#' Evaluates a Jack polynomial.
#'
#' @param x values of the variables, a vector of \code{bigq} numbers, or a
#'   vector that can be coerced as such (e.g. \code{c("2", "5/3")})
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#' @param alpha rational number, given as a string such as
#'   \code{"2/3"} or as a \code{bigq} number
#'
#' @return A \code{bigq} number.
#'
#' @export
#' @importFrom gmp as.bigq
#'
#' @examples
#' Jack(c("1", "3/2", "-2/3"), lambda = c(3, 1), alpha = "1/4")
Jack <- function(x, lambda, alpha) {
  stopifnot(isPartition(lambda))
  lambda <- as.integer(removeTrailingZeros(lambda))
  if(is.numeric(x)) {
    x <- as.double(x)
    gmp <- FALSE
  } else {
    x <- as.bigq(x)
    gmp <- TRUE
  }
  if(anyNA(x)) {
    stop("Found missing values in `x`.")
  }
  if(alpha == 0) {
    lambdaPrime <- dualPartition(lambda)
    if(gmp){
      f <- prod(factorialZ(lambdaPrime))
    }else{
      f <- prod(factorial(lambdaPrime))
    }
    return(f * ESF(x, lambdaPrime))
  }
  if(is.numeric(x) && is.numeric(alpha)) {
    JackEvalRcpp_double(x, lambda, alpha)
  } else {
    alpha <- as.bigq(alpha)
    if(is.na(alpha)) {
      stop("Invalid `alpha`.")
    }
    alpha <- as.character(alpha)
    res <- JackEvalRcpp_gmpq(as.character(x), lambda, alpha)
    as.bigq(res)
  }
}

#' Zonal polynomial - C++ implementation
#'
#' @description Returns a zonal polynomial. The zonal polynomials are the
#'   Jack \eqn{C}-polynomials with Jack parameter \eqn{\alpha=Z}.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{qspray} multivariate polynomial.
#'
#' @export
#' @examples
#' ( zonal <- ZonalPol(3, lambda = c(3, 1)) )
#' zonal == JackPol(3, lambda = c(3, 1), alpha = "2", which = "C")
ZonalPol <- function(n, lambda){
  JackPol(n, lambda, alpha = "2", which = "C")
}

#' Evaluation of zonal polynomial - C++ implementation
#'
#' @description Evaluates a zonal polynomial. The zonal polynomials are the
#'   Jack \eqn{C}-polynomials with Jack parameter \eqn{\alpha=Z}.
#'
#' @param x values of the variables, a vector of \code{bigq} numbers, or a
#'   vector that can be coerced as such (e.g. \code{c("2", "5/3")})
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{bigq} number.
#'
#' @export
#' @importFrom gmp as.bigq factorialZ asNumeric
#'
#' @examples
#' Zonal(c("1", "3/2", "-2/3"), lambda = c(3, 1))
Zonal <- function(x, lambda){
  lambda <- as.integer(removeTrailingZeros(lambda))
  C <- JackCcoefficient(lambda, 2L)
  if(is.numeric(x)) {
    jack <- Jack(x, lambda, alpha = 2L)
    asNumeric(C) * jack
  } else {
    jack <- Jack(x, lambda, alpha = "2")
    C * jack
  }
}


#' Quaternionic zonal polynomial - C++ implementation
#'
#' @description Returns a quaternionic zonal polynomial. The quaternionic
#'   zonal polynomials are the Jack \eqn{C}-polynomials with Jack
#'   parameter \eqn{\alpha=1/Z}.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{qspray} multivariate polynomial.
#'
#' @export
#' @examples
#' ( zonalQ <- ZonalQPol(3, lambda = c(3, 1)) )
#' zonalQ == JackPol(3, lambda = c(3, 1), alpha = "1/2", which = "C")
ZonalQPol <- function(n, lambda){
  JackPol(n, lambda, alpha = "1/2", which = "C")
}

#' Evaluation of zonal quaternionic polynomial - C++ implementation
#'
#' @description Evaluates a zonal quaternionic polynomial. The quaternionic
#'   zonal polynomials are the Jack \eqn{C}-polynomials with Jack
#'   parameter \eqn{\alpha=1/Z}.
#'
#' @param x values of the variables, a vector of \code{bigq} numbers, or a
#'   vector that can be coerced as such (e.g. \code{c("2", "5/3")})
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#'
#' @return A \code{bigq} number.
#'
#' @export
#' @importFrom gmp asNumeric
#'
#' @examples
#' ZonalQ(c("1", "3/2", "-2/3"), lambda = c(3, 1))
ZonalQ <- function(x, lambda){
  lambda <- as.integer(removeTrailingZeros(lambda))
  C <- JackCcoefficient(lambda, "1/2")
  if(is.numeric(x)) {
    jack <- Jack(x, lambda, alpha = 0.5)
    asNumeric(C) * jack
  } else {
    jack <- Jack(x, lambda, alpha = "1/2")
    C * jack
  }
}
