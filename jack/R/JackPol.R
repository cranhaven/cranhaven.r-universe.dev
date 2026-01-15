JackPolNaive <- function(n, lambda, alpha, basis = "canonical"){
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  basis <- match.arg(basis, c("canonical", "MSF"))
  gmp <- is.bigq(alpha)
  if(length(lambda) == 0L) {
    if(basis == "canonical") {
      return(if(gmp) qone() else as_mvp_spray(one(n)))
    } else {
      return("M_()")
    }
  }
  lambda <- lambda[lambda > 0L]
  if(length(lambda) > n)
    return(if(gmp) as.qspray(0L) else as_mvp_spray(zero(n)))
  lambda00 <- integer(sum(lambda))
  lambda00[seq_along(lambda)] <- lambda
  mus <- dominatedPartitions(lambda)
  if(gmp) {
    coefs <- JackCoefficientsQ(sum(lambda), alpha, until = lambda)
  } else {
    coefs <- JackCoefficientsNum(sum(lambda), alpha, until = lambda)
  }
  coefs <- coefs[toString(lambda00), ]
  if(basis == "canonical") {
    if(gmp) {
      out <- qzero()
      for(i in 1L:ncol(mus)){
        mu <- mus[, i]
        l <- sum(mu > 0L)
        if(l <= n) {
          toAdd <- MSFpoly(n, mu)
          if(coefs[toString(mu)] != "1")
            toAdd <- toAdd * coefs[toString(mu)]
          out <- out + toAdd
        }
      }
      out
    } else {
      out <- zero(n)
      for(i in 1L:ncol(mus)) {
        mu <- mus[, i]
        l <- sum(mu > 0L)
        if(l <= n) {
          toAdd <- MSFspray(n, mu) * coefs[toString(mu)]
          out <- out + toAdd
        }
      }
      as_mvp_spray(out)
    }
  } else {
    vars <- apply(mus, 2L, function(mu) {
      paste0("M_(", paste0(mu[mu > 0L], collapse = ","), ")")
    })
    rowIdx <- match(toString(lambda00), names(coefs))
    coefs <- coefs[-seq_len(rowIdx-1L)]
    coefs <- ifelse(coefs == "1", "", paste0(coefs, " "))
    paste0(coefs, vars, collapse = " + ")
  }
}

JackPolDK <- function(n, lambda, alpha) {
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  jac <- function(m, k, mu, nu, beta) {
    if(length(nu) == 0L || nu[1L] == 0L || m == 0L) return(one(n))
    if(length(nu) > m && nu[m+1L] > 0L) return(zero(n))
    if(m == 1L) return(prod(alpha*seq_len(nu[1L]-1L)+1) * lone(1, n)^nu[1L])
    if(k == 0L && !is.na(s <- S[[.N(lambda,nu),m]])) return(s)
    i <- max(1L,k)
    s <- jac(m-1L, 0L, nu, nu, 1) * beta * lone(m, n)^(sum(mu)-sum(nu))
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        gamma <- beta * .betaratio(mu, nu, i, alpha)
        if(nu[i] > 1L){
          s <- s + jac(m, i, mu, .nu, gamma)
        }else{
          s <- s + jac(m-1L, 0L, .nu, .nu, 1) * gamma *
            lone(m, n)^(sum(mu)-sum(.nu))
        }
      }
      i <- i + 1L
    }
    if(k == 0L) S[[.N(lambda,nu),m]] <- s
    return(s)
  }
  S <- as.list(rep(NA, .N(lambda,lambda) * n))
  dim(S) <- c(.N(lambda,lambda), n)
  as_mvp_spray(jac(n, 0L, lambda, lambda, 1))
}

#' @importFrom qspray as.qspray qsprayMaker qlone
#' @importFrom gmp as.bigq
#' @noRd
JackPolDK_gmp <- function(n, lambda, alpha) {
  stopifnot(isPositiveInteger(n), isPartition(lambda))
  jac <- function(m, k, mu, nu, beta) {
    if(length(nu) == 0L || nu[1L] == 0L || m == 0L) {
      return(as.qspray(1L))
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(as.qspray(0L))
    if(m == 1L) {
      return(
        prod(alpha * seq_len(nu[1L]-1L) + 1L) * qlone(1L)^nu[1L]
      )
    }
    if(k == 0L && inherits(s <- S[[.N(lambda, nu), m]], "qspray")) return(s)
    i <- max(1L, k)
    s <- jac(m-1L, 0L, nu, nu, oneq) * as.qspray(beta) *
      qsprayMaker(
        coeffs = "1",
        powers = list(c(rep(0L, m-1L), sum(mu)-sum(nu)))
      )
    while(length(nu) >= i && nu[i] > 0L) {
      if(length(nu) == i || nu[i] > nu[i+1L]) {
        .nu <- nu; .nu[i] <- nu[i]-1L
        gamma <- beta * .betaratio(mu, nu, i, alpha)
        if(nu[i] > 1L) {
          s <- s + jac(m, i, mu, .nu, gamma)
        } else {
          s <- s + jac(m-1L, 0L, .nu, .nu, oneq) * as.qspray(gamma) *
            qsprayMaker(
              coeffs = "1",
              powers = list(c(rep(0L, m-1L), sum(mu)-sum(.nu)))
            )
        }
      }
      i <- i + 1L
    }
    if(k == 0L) S[[.N(lambda, nu), m]] <- s
    return(s)
  }
  Nlambdalambda <- .N(lambda, lambda)
  S <- as.list(rep(NA, Nlambdalambda * n))
  dim(S) <- c(Nlambdalambda, n)
  oneq <- as.bigq(1L)
  jac(n, 0L, lambda, lambda, oneq)
}

#' Jack polynomial
#'
#' Returns the Jack polynomial.
#'
#' @param n number of variables, a positive integer
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#' @param alpha parameter of the Jack polynomial, a number, possibly (and
#'   preferably) a \code{\link[gmp]{bigq}} rational number
#' @param algorithm the algorithm used, either \code{"DK"} or \code{"naive"}
#' @param basis the polynomial basis for \code{algorithm = "naive"},
#' either \code{"canonical"} or \code{"MSF"} (monomial symmetric functions);
#' for \code{algorithm = "DK"} the canonical basis is always used and
#' this parameter is ignored
#' @param which which Jack polynomial, \code{"J"}, \code{"P"} or \code{"Q"};
#'   this argument is taken into account \strong{only} if \code{alpha} is a
#'   \code{bigq} number and \code{algorithm = "DK"}
#'
#' @return A \code{mvp} multivariate polynomial (see \link[mvp]{mvp-package}),
#'  or a \code{qspray} multivariate polynomial if \code{alpha}
#'  is a \code{bigq} rational number and \code{algorithm = "DK"}, or a
#'  character string if \code{basis = "MSF"}.
#' @importFrom gmp is.bigq
#' @export
#'
#' @examples JackPolR(3, lambda = c(3,1), alpha = gmp::as.bigq(2,3),
#'                   algorithm = "naive")
#' JackPolR(3, lambda = c(3,1), alpha = 2/3, algorithm = "DK")
#' JackPolR(3, lambda = c(3,1), alpha = gmp::as.bigq(2,3), algorithm = "DK")
#' JackPolR(3, lambda = c(3,1), alpha= gmp::as.bigq(2,3),
#'         algorithm = "naive", basis = "MSF")
#' # when the Jack polynomial is a `qspray` object, you can
#' # evaluate it with `qspray::evalQspray`:
#' jack <- JackPolR(3, lambda = c(3, 1), alpha = gmp::as.bigq(2))
#' evalQspray(jack, c("1", "1/2", "3"))
JackPolR <- function(n, lambda, alpha, algorithm = "DK",
                    basis = "canonical", which = "J"){
  stopifnot(
    is.numeric(alpha) || is.bigq(alpha),
    length(alpha) == 1L
  )
  algo  <- match.arg(algorithm, c("DK", "naive"))
  which <- match.arg(which, c("J", "P", "Q"))
  if(which != "J" && !(algo == "DK" && is.bigq(alpha))) {
    warning(
      "You selected a Jack polynomial other than \"J\" and your choice ",
      "will be ignored. ",
      "Use `algorithm=\"DK\"` and a `bigq` number for `alpha` if you want ",
      "this Jack polynomial."
    )
  }
  lambda <- as.integer(lambda[lambda != 0])
  if(algo == "DK"){
    if(is.bigq(alpha)) {
      K <- switch(
        which,
        "J" = as.bigq(1L),
        "P" = JackPcoefficient(lambda, alpha),
        "Q" = JackQcoefficient(lambda, alpha)
      )
      K * JackPolDK_gmp(n, lambda, alpha)
    } else {
      JackPolDK(n, lambda, alpha)
    }
  } else {
    JackPolNaive(n, lambda, alpha, basis)
  }
}

