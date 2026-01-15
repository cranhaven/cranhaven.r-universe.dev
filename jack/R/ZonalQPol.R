ZonalQPolNaive <- function(m, lambda, basis = "canonical", exact = TRUE){
  stopifnot(isPositiveInteger(m), isPartition(lambda))
  basis <- match.arg(basis, c("canonical", "MSF"))
  if(length(lambda) == 0L){
    if(basis == "canonical"){
      return(if(exact) as.qspray(1) else as_mvp_spray(one(m)))
    }else{
      return("M_()")
    }
  }
  lambda <- lambda[lambda > 0L]
  if(length(lambda) > m) return(if(exact) as.qspray(0) else as_mvp_spray(zero(m)))
  lambda00 <- numeric(sum(lambda))
  lambda00[seq_along(lambda)] <- lambda
  mus <- dominatedPartitions(lambda)
  if(exact){
    coefs <- zonalQCoefficientsQ(sum(lambda), until = lambda)
  }else{
    coefs <- zonalQCoefficientsNum(sum(lambda), until = lambda)
  }
  coefs <- coefs[toString(lambda00),]
  if(basis == "canonical"){
    if(exact){
      out <- as.qspray(0)
      for(i in 1L:ncol(mus)){
        mu <- mus[,i]
        l <- sum(mu > 0L)
        if(l <= m){
          toAdd <- MSFpoly(m, mu)
          if(coefs[toString(mu)] != "1")
            toAdd <- toAdd * coefs[toString(mu)]
          out <- out + toAdd
        }
      }
      out
    }else{
      out <- zero(m)
      for(i in 1L:ncol(mus)){
        mu <- mus[,i]
        l <- sum(mu > 0L)
        if(l <= m){
          toAdd <- MSFspray(m, mu) * coefs[toString(mu)]
          out <- out + toAdd
        }
      }
      as_mvp_spray(out)
    }
  }else{
    vars <- apply(mus, 2L, function(mu){
      paste0("M_(", paste0(mu[mu>0L], collapse = ","), ")")
    })
    coefs <- coefs[coefs != "0"]
    coefs <- ifelse(coefs == "1", "", paste0(coefs, " "))
    paste0(coefs, vars, collapse = " + ")
  }
}

ZonalQPolDK <- function(m, lambda){
  jack <- JackPolDK(m, lambda, alpha = 1/2)
  jlambda <- sum(logHookLengths(lambda, alpha = 1/2))
  n <- sum(lambda)
  exp(-n*log(2) + lfactorial(n) - jlambda) * jack
}

#' @importFrom gmp as.bigq factorialZ
#' @noRd
ZonalQPolDK_gmp <- function(m, lambda){
  onehalfq <- as.bigq(1L, 2L)
  jack <- JackPolDK_gmp(m, lambda, alpha = onehalfq)
  jlambda <- prod(hookLengths_gmp(lambda, alpha = onehalfq))
  n <- sum(lambda)
  as.qspray(onehalfq^n * factorialZ(n) / jlambda) * jack
}

#' Quaternionic zonal polynomial
#'
#' Returns the quaternionic (or symplectic) zonal polynomial.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#'   integers
#' @param algorithm the algorithm used, either \code{"DK"} or \code{"naive"}
#' @param basis the polynomial basis for \code{algorithm = "naive"},
#' either \code{"canonical"} or \code{"MSF"} (monomial symmetric functions);
#' for \code{algorithm = "DK"} the canonical basis is always used and
#' this parameter is ignored
#' @param exact logical, whether to get rational coefficients
#'
#' @return A \code{mvp} multivariate polynomial (see \link[mvp]{mvp-package}),
#'  or a \code{qspray} multivariate polynomial if
#'  \code{exact = TRUE} and \code{algorithm = "DK"}, or a
#'  character string if \code{basis = "MSF"}.
#'
#' @export
#'
#' @examples ZonalQPolR(3, lambda = c(3,1), algorithm = "naive")
#' ZonalQPolR(3, lambda = c(3,1), algorithm = "DK")
#' ZonalQPolR(3, lambda = c(3,1), algorithm = "DK", exact = FALSE)
#' ZonalQPolR(3, lambda = c(3,1), algorithm = "naive", basis = "MSF")
ZonalQPolR <- function(n, lambda, algorithm = "DK", basis = "canonical",
                     exact = TRUE){
  algo <- match.arg(algorithm, c("DK", "naive"))
  lambda <- as.integer(lambda)
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0L]
  if(algo == "DK"){
    if(exact){
      ZonalQPolDK_gmp(n, lambda)
    }else{
      ZonalQPolDK(n, lambda)
    }
  }else{
    ZonalQPolNaive(n, lambda, basis, exact)
  }
}
