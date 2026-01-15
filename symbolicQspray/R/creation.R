#' @title The null 'symbolicQspray' polynomial
#' @description Returns the \code{symbolicQspray} polynomial
#'   identically equal to 0.
#' @return A \code{symbolicQspray} object.
#' @export
Qzero <- function() {
  new("symbolicQspray", powers = list(), coeffs = list())
}

#' @title The unit 'symbolicQspray' polynomial
#' @description Returns the \code{symbolicQspray} polynomial identically
#'   equal to 1.
#' @return A \code{symbolicQspray} object.
#' @export
Qone <- function() {
  unit_qspray <- new("qspray", powers = list(integer(0L)), coeffs = "1")
  unit_ratioOfQsprays <-
    new("ratioOfQsprays", numerator = unit_qspray, denominator = unit_qspray)
  new(
    "symbolicQspray",
    powers = list(integer(0L)),
    coeffs = list(unit_ratioOfQsprays)
  )
}

#' @title Polynomial variable
#' @description Creates a polynomial variable for a \code{symbolicQspray}.
#'
#' @param n positive integer, the index of the variable
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @examples
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' (X + Y)^2
#' Qlone(0) == 1
Qlone <- function(n) {
  stopifnot(isPositiveInteger(n))
  if(n == 0L) {
    return(Qone())
  }
  powers    <- integer(n)
  powers[n] <- 1L
  new(
    "symbolicQspray",
    powers = list(powers),
    coeffs = list(as.ratioOfQsprays(1L))
  )
}

#' @title Random 'symbolicQspray'
#' @description Generates a random \code{symbolicQspray} object.
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom qspray rQspray
#' @importFrom ratioOfQsprays rRatioOfQsprays
rSymbolicQspray <- function() {
  qspray <- rQspray()
  powers <- qspray@powers
  nterms <- length(powers)
  coeffs <- replicate(nterms, rRatioOfQsprays(allow.zero = FALSE))
  new(
    "symbolicQspray",
    powers = powers,
    coeffs = coeffs
  )
}

