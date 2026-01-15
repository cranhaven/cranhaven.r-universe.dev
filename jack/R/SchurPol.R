
SchurPolNaive <- function(m, lambda, basis = "canonical",
                          exact = TRUE){
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
  lambda00 <- integer(sum(lambda))
  lambda00[seq_along(lambda)] <- lambda
  mus <- dominatedPartitions(lambda)
  if(exact){
    coefs <- SchurCoefficientsQ(sum(lambda), until = lambda)
  }else{
    coefs <- SchurCoefficientsNum(sum(lambda), until = lambda)
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

#' @importFrom mvp constant mvp
#' @noRd
SchurPolDK <- function(n, lambda){
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  sch <- function(m, k, nu){
    if(length(nu) == 0L || nu[1L] == 0L || m == 0L){
      return(one(n))
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(zero(n))
    if(m == 1L) return(lone(1, n)^nu[1L])
    if(!is.na(s <- S[[.N(lambda, nu), m]])) return(s)
    s <- sch(m-1L, 1L, nu)
    i <- k
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        if(nu[i] > 1L){
          s <- s + lone(m, n) * sch(m, i, .nu)
        }else{
          s <- s + lone(m, n) * sch(m-1L, 1L, .nu)
        }
      }
      i <- i + 1L
    }
    if(k == 1L) S[[.N(lambda, nu), m]] <- s
    return(s)
  }
  Nlambdalambda <- .N(lambda,lambda)
  S <- as.list(rep(NA, Nlambdalambda*n))
  dim(S) <- c(Nlambdalambda, n)
  sch(n, 1L, as.integer(lambda))
}

SchurPolDK_gmp <- function(n, lambda){
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  sch <- function(m, k, nu){
    if(length(nu) == 0L || nu[1L] == 0L || m == 0L){
      return(as.qspray(1))
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(as.qspray(0))
    if(m == 1L) return(qlone(1)^nu[1L])
    if(inherits(s <- S[[.N(lambda, nu), m]], "qspray")) return(s)
    s <- sch(m-1L, 1L, nu)
    i <- k
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        if(nu[i] > 1L){
          s <- s + x[[m]] * sch(m, i, .nu)
        }else{
          s <- s + x[[m]] * sch(m-1L, 1L, .nu)
        }
      }
      i <- i + 1L
    }
    if(k == 1L) S[[.N(lambda, nu), m]] <- s
    return(s)
  }
  Nlambdalambda <- .N(lambda,lambda)
  S <- as.list(rep(NA, Nlambdalambda*n))
  dim(S) <- c(Nlambdalambda, n)
  oneq <- as.bigq(1L)
  x <- lapply(1L:n, function(m){
    qsprayMaker(
      coeffs = "1",
      powers = list(c(rep(0L, m-1L), 1L))
    )
  })
  sch(n, 1L, as.integer(lambda))
}

#' Schur polynomial
#'
#' Returns the Schur polynomial.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#' @param algorithm the algorithm used, either \code{"DK"} or \code{"naive"}
#' @param basis the polynomial basis for \code{algorithm = "naive"},
#' either \code{"canonical"} or \code{"MSF"} (monomial symmetric functions);
#' for \code{algorithm = "DK"} the canonical basis is always used and
#' this parameter is ignored
#' @param exact logical, whether to use exact arithmetic
#'
#' @return A \code{mvp} multivariate polynomial (see \link[mvp]{mvp-package}),
#'  or a \code{qspray} multivariate polynomial if
#'  \code{exact = TRUE} and \code{algorithm = "DK"}, or a
#'  character string if \code{basis = "MSF"}.
#'
#' @export
#'
#' @examples SchurPolR(3, lambda = c(3,1), algorithm = "naive")
#' SchurPolR(3, lambda = c(3,1), algorithm = "DK")
#' SchurPolR(3, lambda = c(3,1), algorithm = "DK", exact = FALSE)
#' SchurPolR(3, lambda = c(3,1), algorithm = "naive", basis = "MSF")
SchurPolR <- function(n, lambda, algorithm = "DK", basis = "canonical",
                     exact = TRUE){
  algo <- match.arg(algorithm, c("DK", "naive"))
  lambda <- as.integer(lambda)
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0L]
  if(algo == "DK"){
    if(exact){
      SchurPolDK_gmp(n, lambda)
    }else{
      SchurPolDK(n, lambda)
    }
  }else{
    SchurPolNaive(n, lambda, basis, exact)
  }
}
