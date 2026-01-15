Print <- function(qspray) {
  capture.output(qspray)
}

comboToQspray <- function(combo) {
  lambdas <- lapply(combo, `[[`, "lambda")
  coeffs <- as.character(gmp::c_bigq(lapply(combo, `[[`, "coeff")))
  new("qspray", powers = lambdas, coeffs = coeffs)
}