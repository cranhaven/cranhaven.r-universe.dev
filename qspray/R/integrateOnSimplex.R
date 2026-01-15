#' @title Integral of a multivariate polynomial over a simplex
#' @description Returns the exact value of the integral of a multivariate 
#'   polynomial with rational coefficients over a simplex whose vertices have 
#'   rational coordinates.
#'
#' @param P a \code{qspray} object
#' @param S the simplex, a \code{(n+1)xn} matrix such that each entry of the  
#'   matrix \code{as.character(S)} is a quoted integer or a quoted fraction
#'
#' @return A \code{bigq} number, the exact value of the integral.
#' @export
#' @importFrom RationalMatrix Qdet
#' @examples 
#' library(qspray)
#' x <- qlone(1); y <- qlone(2)
#' P <- x/2 + x*y
#' S <- rbind(c("0", "0"), c("1", "0"), c("1", "1")) # a triangle
#' integratePolynomialOnSimplex(P, S)
integratePolynomialOnSimplex <- function(P, S) {
  storage.mode(S) <- "character"
  check <- all(vapply(S, isFraction, logical(1L)))
  if(!check) {
    stop("Invalid entries in the matrix `S`.")
  }
  n <- ncol(S)
  if(nrow(S) != n+1L) {
    stop("The matrix `S` does not represent a simplex.")
  }
  S <- as.bigq(S)
  v <- t(S[n+1L, ])
  B <- t(S[1L:n, ]) - do.call(function(...) cbind(...), replicate(n, v))
  gens <- lapply(1L:n, function(i) qlone(i))
  newvars <- vector("list", n)
  for(i in 1L:n) {
    newvar <- v[i]
    Bi <- B[i, ]
    for(j in 1L:n) {
      newvar <- newvar + Bi[j] * gens[[j]]
    }
    newvars[[i]] <- newvar
  }
  Q <- as.bigq(0L)
  exponents <- P@powers
  coeffs    <- P@coeffs 
  for(i in 1L:length(exponents)) {
    powers <- exponents[[i]]
    term <- as.bigq(1L)
    for(j in seq_along(powers)) {
      term <- term * newvars[[j]]^powers[j] 
    }
    Q <- Q + coeffs[i] * term
  }
  s <- as.bigq(0L)
  exponents <- Q@powers
  coeffs    <- Q@coeffs 
  for(i in 1L:length(exponents)) {
    coef <- as.bigq(coeffs[i])
    powers <- exponents[[i]]
    d <- sum(powers)
    if(d == 0L) {
      s <- s + coef
      next
    }
    coef <- coef * prod(factorialZ(powers))
    s <- s + coef / prod((n+1L):(n+d))
  }
  abs(as.bigq(Qdet(as.character(B)))) *  s / factorialZ(n)
}

