comboToQspray <- function(combo) {
  lambdas <- lapply(combo, `[[`, "lambda")
  coeffs <- as.character(gmp::c_bigq(lapply(combo, `[[`, "coeff")))
  new("qspray", powers = lambdas, coeffs = coeffs)
}

JackCombinationToQspray <- function(combo, n, alpha, which) {
  lambdas <- lapply(combo, `[[`, "lambda")
  coeffs <- lapply(combo, `[[`, "coeff")
  Reduce(`+`, mapply(function(lambda, coeff) {
    coeff * JackPol(n, lambda, alpha, which)
  }, lambdas, coeffs, SIMPLIFY = FALSE))
}

SchurCombinationToQspray <- function(combo, n) {
  lambdas <- lapply(combo, `[[`, "lambda")
  coeffs <- lapply(combo, `[[`, "coeff")
  Reduce(`+`, mapply(function(lambda, coeff) {
    coeff * SchurPol(n, lambda)
  }, lambdas, coeffs, SIMPLIFY = FALSE))
}

JackSymbolicCombinationToQspray <- function(combo, n, which) {
  lambdas <- lapply(combo, `[[`, "lambda")
  coeffs <- lapply(combo, `[[`, "coeff")
  Reduce(`+`, mapply(function(lambda, coeff) {
    coeff * JackSymPol(n, lambda, which)
  }, lambdas, coeffs, SIMPLIFY = FALSE))
}
