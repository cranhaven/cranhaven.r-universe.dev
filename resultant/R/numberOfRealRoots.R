#' @title Number of real roots
#' @description Number of distinct real roots of a univariate polynomial.
#'
#' @param qspray a univariate \code{qspray} polynomial
#'
#' @return An integer, the number of real roots of the polynomial.
#' @note The roots are not counted with their multiplicity.
#' @export
#' @importFrom qspray numberOfVariables isQzero
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' P <- 2*x^4 + x^3 - 3*x^2 - x + 1
#' numberOfRealRoots(P)
numberOfRealRoots <- function(qspray) {
  n <- numberOfVariables(qspray)
  if(n == 0L) {
    if(isQzero(qspray)) {
      Inf
    } else {
      0L
    }
  } else if(n == 1L) {
    coeffs <- qspray@coeffs
    pows <- vapply(qspray@powers, function(pwrs) {
      out <- integer(1L)
      out[seq_along(pwrs)] <- pwrs
      out
    }, integer(1L))
    numberOfRealRootsCPP(pows, coeffs)
  } else {
    stop("The polynomial is not univariate.")
  }
}

signVariations <- function(x) {
  signs <- vapply(x, sign, integer(1L))
  l <- length(signs)
  chunks2 <- cbind(vapply(seq_len(l - 1L), function(i) {
    signs[c(i, i+1L)]
  }, integer(2L)))
  v <- sum(apply(chunks2, 2L, function(chunk) {
    identical(chunk, c(1L, -1L)) || identical(chunk, c(-1L, 1L))
  }))
  if(l >= 3L) {
    chunks3 <- cbind(vapply(seq_len(l - 2L), function(i) {
      signs[c(i, i+1L, i+2L)]
    }, integer(3L)))
    v <- v + sum(apply(chunks3, 2L, function(chunk) {
      identical(chunk, c(1L, 0L, -1L)) ||
        identical(chunk, c(-1L, 0L, 1L))
    }))
    if(l >= 4L) {
      chunks4 <- cbind(vapply(seq_len(l - 3L), function(i) {
        signs[c(i, i+1L, i+2L, i+3L)]
      }, integer(4L)))
      v <- v + sum(apply(chunks4, 2L, function(chunk) {
        identical(chunk, c(1L, 0L, 0L, -1L)) ||
          identical(chunk, c(-1L, 0L, 0L, 1L))
      })) + 2L * sum(apply(chunks4, 2L, function(chunk) {
        identical(chunk, c(1L, 0L, 0L, 1L)) ||
          identical(chunk, c(-1L, 0L, 0L, -1L))
      }))
    }
  }
  v
}

#' @title Number of real roots in an interval
#' @description Number of distinct real roots of a univariate polynomial in
#'   a given interval.
#'
#' @param qspray a univariate \code{qspray} polynomial
#' @param lower,upper the bounds of the interval, \code{bigq} numbers or
#'   objects coercible to \code{bigq} numbers, and it is also possible to set
#'   \code{lower = -Inf} and \code{upper = Inf}
#' @param closed Boolean, whether to consider the interval is closed or open
#'
#' @return An integer, the number of real roots of the polynomial in the
#'   interval.
#' @note The roots are not counted with their multiplicity.
#' @export
#' @importFrom qspray isUnivariate isQzero isConstant evalQspray leadingCoefficient
#' @importFrom gmp as.bigq c_bigq
#' @importFrom utils head
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' P <- 2*x^4 + x^3 - 3*x^2 - x + 1
#' numberOfRealRootsInInterval(P, 0, 1)
numberOfRealRootsInInterval <- function(qspray, lower, upper, closed = TRUE) {
  if(!isUnivariate(qspray)) {
    stop("The polynomial is not univariate.")
  }
  if(isMinusInfinity(lower) && isPlusInfinity(upper)) {
    numberOfRealRoots(qspray)
  } else if(isMinusInfinity(lower)) {
    numberOfRealRootsInLeftUnboundedInterval(qspray, upper, closed)
  } else if(isPlusInfinity(upper)) {
    numberOfRealRootsInRightUnboundedInterval(qspray, lower, closed)
  } else {
    numberOfRealRootsInBoundedInterval(qspray, lower, upper, closed)
  }
}

numberOfRealRootsInBoundedInterval <- function(qspray, lower, upper, closed) {
  interval <- range(c(as.bigq(lower), as.bigq(upper)))
  if(anyNA(interval)) {
    stop("Invalid bounds.")
  }
  alpha <- interval[1L]
  beta  <- interval[2L]
  equalBounds <- alpha == beta
  singleton <- equalBounds && closed
  empty     <- equalBounds && !closed
  if(empty) {
    0L
  } else if(isQzero(qspray)) {
    if(singleton) 1L else Inf
  } else if(isConstant(qspray)){
    0L
  } else {
    valueAtAlpha <- evalQspray(qspray, alpha)
    zeroAtAlpha <- valueAtAlpha == 0L
    if(singleton) {
      if(zeroAtAlpha) 1L else 0L
    } else {
      valueAtBeta <- evalQspray(qspray, beta)
      zeroAtBeta  <- valueAtBeta == 0L
      SHsequence <- head(SturmHabicht(qspray, 1L), -1L)
      SHsequence <- Filter(Negate(isQzero), SHsequence)
      valuesAtAlpha <- c(c_bigq(lapply(SHsequence, function(p) {
        evalQspray(p, alpha)
      })), valueAtAlpha)
      valuesAtBeta <- c(c_bigq(lapply(SHsequence, function(p) {
        evalQspray(p, beta)
      })), valueAtBeta)
      nroots <- signVariations(valuesAtAlpha) - signVariations(valuesAtBeta)
      if(zeroAtBeta) {
        nroots <- nroots - 1L
      }
      if(closed) {
        nroots <- nroots + zeroAtAlpha + zeroAtBeta
      }
      nroots
    }
  }
}

numberOfRealRootsInRightUnboundedInterval <- function(
    qspray, alpha, closed
) {
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid bound.")
  }
  if(isQzero(qspray)) {
    Inf
  } else if(isConstant(qspray)){
    0L
  } else {
    valueAtAlpha <- evalQspray(qspray, alpha)
    zeroAtAlpha <- valueAtAlpha == 0L
    valueAtInfinity <- leadingCoefficient(qspray)
    SHsequence <- head(SturmHabicht(qspray, 1L), -1L)
    SHsequence <- Filter(Negate(isQzero), SHsequence)
    valuesAtAlpha <- c(c_bigq(lapply(SHsequence, function(p) {
      evalQspray(p, alpha)
    })), valueAtAlpha)
    valuesAtInfinity <- c(c_bigq(lapply(SHsequence, function(p) {
      if(isQzero(p)) {
        as.bigq(0L)
      } else {
        leadingCoefficient(p)
      }
    })), valueAtInfinity)
    nroots <- signVariations(valuesAtAlpha) - signVariations(valuesAtInfinity)
    if(closed) {
      nroots <- nroots + zeroAtAlpha
    }
    nroots
  }
}

numberOfRealRootsInLeftUnboundedInterval <- function(
    qspray, beta, closed
) {
  if(isQzero(qspray)) {
    Inf
  } else {
    numberOfRealRoots(qspray) -
      numberOfRealRootsInRightUnboundedInterval(qspray, beta, !closed)
  }
}
