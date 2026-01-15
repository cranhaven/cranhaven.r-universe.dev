#' @include symbolicQspray.R
NULL

setGeneric("compactSymmetricQspray")

#' @name compactSymmetricQspray
#' @aliases compactSymmetricQspray,symbolicQspray,logical-method compactSymmetricQspray,symbolicQspray,missing-method
#' @docType methods
#' @title Compact symmetric qspray
#' @description Prints a symmetric \code{symbolicQspray} polynomial as a linear
#'   combination of the monomial symmetric polynomials.
#'
#' @param qspray a \code{symbolicQspray} object which should correspond to a
#'   symmetric polynomial
#' @param check Boolean, whether to check the symmetry
#'
#' @return A character string.
#' @export
#' @importFrom qspray compactSymmetricQspray MSPcombination
#'
#' @seealso \code{\link[qspray]{MSPcombination}}
setMethod(
  "compactSymmetricQspray", c("symbolicQspray", "logical"),
  function(qspray, check) {
    combo <- MSPcombination(qspray, check = check)
    powers <- lapply(combo, `[[`, "lambda")
    coeffs <- lapply(combo, `[[`, "coeff")
    msp <- new("symbolicQspray", powers = powers, coeffs = coeffs)
    msp <- passShowAttributes(qspray, msp)
    showMonomial <- function(exponents) {
      sprintf("M[%s]", toString(exponents))
    }
    showSymbolicQsprayOption(msp, "showMonomial") <- showMonomial
    f <- getShowSymbolicQspray(msp)
    f(msp)
  }
)

#' @rdname compactSymmetricQspray
#' @export
setMethod(
  "compactSymmetricQspray", c("symbolicQspray", "missing"),
  function(qspray, check) {
    compactSymmetricQspray(qspray, FALSE)
  }
)
