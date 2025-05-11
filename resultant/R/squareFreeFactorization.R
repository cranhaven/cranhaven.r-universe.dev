#' @title Square-free factorization of a polynomial
#' @description Square-free factorization of a polynomial with rational
#'   coefficients.
#'
#' @param qspray a \code{qspray} polynomial having at most nine variables
#'
#' @return A list with two fields \code{constantFactor} and
#'    \code{nonConstantFactors}. In the field \code{constantFactor}, there is
#'    a \code{bigq} rational number, the constant factor of the factorization.
#'    In the field \code{nonConstantFactors}, there is a list providing the
#'    square-free and pairwise coprime \code{qspray} polynomials of the
#'    factorization with their multiplicity.
#'
#' @export
#' @importFrom qspray qsprayMaker numberOfVariables isConstant qone getConstantTerm showQsprayXYZ showQsprayX1X2X3 showQsprayOption<-
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(resultant)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- x^8*y^7 + 2*x^7*y^5 + x^6*y^4 + 2*x^5*y^2
#' squareFreeFactorization(p)
squareFreeFactorization <- function(qspray) {
  if(isConstant(qspray)) {
    return(
      list(
        "constantFactor"     = getConstantTerm(qspray),
        "nonConstantFactors" = list(
          list(
            "qspray"       = qone(),
            "multiplicity" = 1L
          )
        )
      )
    )
  }
  n <- numberOfVariables(qspray)
  if(n >= 10L) {
    stop(
      "Only polynomials with at most nine variables are allowed."
    )
  }
  coeffs <- qspray@coeffs
  pows <- vapply(qspray@powers, function(pwrs) {
    out <- integer(n)
    out[seq_along(pwrs)] <- pwrs
    out
  }, integer(n))
  if(n == 1L) {
    factorization <- squareFreeFactorizationCPP1(
      rbind(pows), coeffs
    )
  } else if(n == 2L) {
    factorization <- squareFreeFactorizationCPP2(
      pows, coeffs
    )
  } else if(n == 3L) {
    factorization <- squareFreeFactorizationCPP3(
      pows, coeffs
    )
  } else if(n == 4L) {
    factorization <- squareFreeFactorizationCPP4(
      pows, coeffs
    )
  } else if(n == 5L) {
    factorization <- squareFreeFactorizationCPP5(
      pows, coeffs
    )
  } else if(n == 6L) {
    factorization <- squareFreeFactorizationCPP6(
      pows, coeffs
    )
  } else if(n == 7L) {
    factorization <- squareFreeFactorizationCPP7(
      pows, coeffs
    )
  } else if(n == 8L) {
    factorization <- squareFreeFactorizationCPP8(
      pows, coeffs
    )
  } else if(n == 9L) {
    factorization <- squareFreeFactorizationCPP9(
      pows, coeffs
    )
  }
  if(n <= 3L) {
    showFunc <- showQsprayXYZ()
  } else {
    showFunc <- showQsprayX1X2X3()
  }
  constantFactor     <- factorization[["constantFactor"]]
  nonConstantFactors <-
    lapply(factorization[["nonConstantFactors"]], function(x) {
      p <- x[["qspray"]]
      p <- qsprayMaker(
        powers = Columns(p[["Powers"]]),
        coeffs = p[["Coeffs"]]
      )
      showQsprayOption(p, "showQspray") <- showFunc
      list("qspray" = p, "multiplicity" = x[["multiplicity"]])
    })
  list(
    "constantFactor"     = as.bigq(constantFactor),
    "nonConstantFactors" = nonConstantFactors
  )
}
