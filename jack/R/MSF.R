#' @importFrom qspray MSFpoly
#' @importFrom spray spray
MSFspray <- function(m, lambda) {
  qspray <- MSFpoly(m, lambda)
  powers <- lapply(qspray@powers, function(exponents) {
    n <- length(exponents)
    if(n < m) {
      c(exponents, rep(0L, m - n))
    } else {
      exponents
    }
  })
  M <- do.call(rbind, powers)
  spray(M, rep(1, nrow(M)))
}

#' Evaluation of monomial symmetric functions
#'
#' Evaluates a monomial symmetric function.
#'
#' @param x a numeric vector or a \code{\link[gmp]{bigq}} vector
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#'
#' @return A number if \code{x} is numeric, a \code{bigq} rational number
#' if \code{x} is a \code{bigq} vector.
#' @importFrom gmp as.bigq is.bigq
#' @importFrom DescTools Permn
#' @export
#'
#' @examples x <- c(1, 2, 5/2)
#' lambda <- c(3, 1)
#' MSF(x, lambda)
#' library(gmp)
#' x <- c(as.bigq(1), as.bigq(2), as.bigq(5,2))
#' MSF(x, lambda)
MSF <- function(x, lambda){
  stopifnot(isPartition(lambda))
  gmp <- is.bigq(x)
  m <- length(x)
  lambda <- lambda[lambda > 0L]
  if(length(lambda) > m) return(if(gmp) as.bigq(0L) else 0)
  kappa <- numeric(m)
  kappa[seq_along(lambda)] <- lambda
  perms <- Permn(kappa)
  if(gmp){
    out <- as.bigq(0L)
    for(i in 1L:nrow(perms)){
      pows <- as.bigq(rep(0L, m))
      for(j in 1L:m){
        pows[j] <- x[j]^perms[i, j]
      }
      out <- out + prod(pows)
    }
  }else{
    out <- 0
    for(i in 1L:nrow(perms)){
      out <- out + prod(x^perms[i, ])
    }
  }
  out
}
