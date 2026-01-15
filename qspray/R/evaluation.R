#' @title Evaluate a 'qspray' object
#' @description Evaluation of the multivariate polynomial represented by a 
#'   \code{qspray} object.
#'
#' @param qspray a \code{qspray} object
#' @param values_re vector of the real parts of the values; each element of 
#'   \code{as.character(values_re)} must be a quoted integer or a quoted fraction
#' @param values_im vector of the imaginary parts of the values; each element of 
#'   \code{as.character(values_im)} must be a quoted integer or a quoted fraction
#'
#' @return A \code{bigq} number if \code{values_im=NULL}, a pair of \code{bigq} 
#'   numbers otherwise: the real part and the imaginary part of the result.
#' @export
#' @examples 
#' x <- qlone(1); y <- qlone(2)
#' P <- 2*x + "1/2"*y
#' evalQspray(P, c("2", "5/2", "99999")) # "99999" will be ignored
evalQspray <- function(qspray, values_re, values_im = NULL) {
  powers <- qspray@powers
  if(length(powers) == 0L) {
    return(as.bigq(0L))
  }
  n <- arity(qspray)
  if(length(values_re) < n) {
    stop("Insufficient number of values.")
  }
  values_re <- as.character(values_re)
  check <- all(vapply(values_re, isFraction, logical(1L)))
  if(!check) {
    stop("Invalid vector `values_re`.")
  }
  if(!is.null(values_im)) {
    stopifnot(length(values_re) == length(values_im))
    values_im <- as.character(values_im)
    check <- all(vapply(values_im, isFraction, logical(1L)))
    if(!check) {
      stop("Invalid vector `values_im`.")
    }
    result <- evalQxspray(powers, qspray@coeffs, values_re, values_im)
    return(as.bigq(result))
  }
  coeffs <- as.bigq(qspray@coeffs)
  values <- as.bigq(values_re)
  out <- 0
  for(i in seq_along(powers)) {
    exponents <- powers[[i]]
    term <- 1
    for(j in seq_along(exponents)) {
      term <- term * values[j]^exponents[j]
    }
    out <- out + coeffs[i] * term
  }
  out
}

#' @title Substitutions in a 'qspray' polynomial
#' @description Substitute some variables in a \code{qspray} polynomial.
#'
#' @param qspray a \code{qspray} object
#' @param values the values to be substituted; this must be a vector whose 
#'   length equals the number of variables of \code{qspray}, and whose each 
#'   entry is either \code{NA} (for non-substitution) or a 'scalar' \code{x} 
#'   such that \code{as.character(x)} is a quoted integer or a quoted fraction
#'
#' @return A \code{qspray} object.
#' @export
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' p <- x^2 + y^2 + x*y*z - 1
#' substituteQspray(p, c("2", NA, "3/2"))
substituteQspray <- function(qspray, values) {
  powers <- qspray@powers
  if(length(powers) == 0L) {
    return(qzero())
  }
  n <- arity(qspray)
  if(n == 0L) {
    return(qspray)
  }
  if(length(values) != n) {
    stop("Wrong number of values.")
  }
  values <- as.character(values)
  check <- all(vapply(values, isFractionOrNA, logical(1L)))
  if(!check) {
    stop("Invalid vector `values`.")
  }
  qlones <- lapply(1L:n, qlone)
  indices <- which(is.na(values))
  coeffs <- as.bigq(qspray@coeffs)
  values <- as.bigq(values)
  out <- qzero()
  for(i in seq_along(powers)) {
    exponents <- powers[[i]]
    idx <- setdiff(seq_along(exponents), indices)
    term <- 1L
    for(j in idx) {
      term <- term * values[j]^exponents[j]
    }
    monomial <- qone()
    for(k in intersect(indices, which(exponents != 0L))) {
      monomial <- monomial * qlones[[k]]^exponents[k]
    }
    out <- out + coeffs[i] * term * monomial
  }
  passShowAttributes(qspray, out)
}
