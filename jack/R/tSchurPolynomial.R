.flambda <- function(pq, r, nu) {
  p <- pq[1L]
  q <- pq[2L]
  if(p == 1L) {
    tnu <- nu
  } else {
    tnu <- tail(nu, 1L-p)
  }
  c(head(nu, p-1L), nu[q]+p-q+r, head(tnu, q-p) + 1L, tail(nu, -q))
}

.ok <- function(lambda, q, r, nu) {
  nu_q <- nu[q]
  nu_q - q + r >= nu[1L] && nu_q <= lambda[1L]
}

.okp <- function(lambda, pq, r, nu) {
  p <- pq[1L]
  q <- pq[2L]
  nu_q <- nu[q]
  nu_q - q + r > nu[p] - p &&
    nu[p-1L] - p >= nu_q - q + r &&
      nu_q <= lambda[p] &&
        all(head(tail(nu, 1L-p), q-p) < tail(lambda, -p))
}

sequencesOfRibbons <- function(lambda, mu, rho) {
  f <- function(zs, r) {
    do.call(c, lapply(zs, function(z) {
      lapply(
          Filter(
            function(lbda) {
              all(lbda <= lambda[seq_along(lbda)])
            },
            .lambdas(lambda, r, z[[length(z)]])
          ),
          function(lbda) {
            c(z, list(lbda))
          }
      )
    }))
  }
  Reduce(f, rho, init = list(list(c(mu, rep(0L, length(lambda) - length(mu))))))
}

.lambdas <- function(lambda, r, nu) {
  if(length(lambda) >= 2L) {
    pairs <- rbind(.pairs(lambda, r, nu), .pairsp(lambda, r, nu))
  } else {
    pairs <- .pairs(lambda, r, nu)
  }
  apply(
    pairs,
    1L,
    function(pq) {
      .flambda(pq, r, nu)
    },
    simplify = FALSE
  )
}

.pairs <- function(lambda, r, nu) {
  cbind(
    1L,
    Filter(
      function(q) .ok(lambda, q, r, nu),
      seq_along(lambda)
    )
  )
}

.pairsp <- function(lambda, r, nu) {
  n <- length(lambda)
  Grid <- do.call(
    rbind,
    lapply(2L:n, function(p) cbind(p, p:n))
  )
  keep <- apply(Grid, 1L, function(pq) {
    .okp(lambda, pq, r, nu)
  })
  Grid[keep, , drop = FALSE]
}

chi_lambda_mu_rho <- function(lambda, mu, rho) {
  if(length(rho) == 0L) {
    1L
  } else {
    sequences <- sequencesOfRibbons(lambda, mu, rho)
    nevens <- sum(
      vapply(sequences, function(sq) {
        sum(
          vapply(seq_len(length(sq)-1L), function(i) {
            kappa <- sq[[i+1L]]
            nu <- sq[[i]]
            sum(kappa != nu) - 1L
          }, integer(1L))
        ) %% 2L == 0L
      }, logical(1L))
    )
    2L * nevens - length(sequences)
  }
}

zlambda <- function(lambda) {
  parts <- unique(lambda)
  mjs <- vapply(parts, function(j) {
    sum(lambda == j)
  }, integer(1L))
  prod(factorial(mjs) * parts^mjs)
}

#' @importFrom symbolicQspray Qone
#' @importFrom qspray qone qlone qzero PSFpoly
#' @importFrom ratioOfQsprays as.ratioOfQsprays
#' @importFrom gmp as.bigq
#' @noRd
.tSkewSchurPolynomial <- function(n, lambda, mu) {
  w <- sum(lambda) - sum(mu)
  if(w == 0L) {
    return(Qone())
  }
  rhos <- listOfPartitions(w)
  unitSpray <- qone()
  t <- qlone(1L)
  mapOfSprays <- lapply(seq_len(w), function(r) {
    unitSpray - t^r
  })
  sprays <- lapply(rhos, function(rho) {
    c <- chi_lambda_mu_rho(lambda, mu, rho)
    if(c == 0L) {
      qzero()
    } else {
      psPoly <- PSFpoly(n, rho)
      coeffs <- lapply(psPoly@coeffs, function(coeff) {
        as.ratioOfQsprays(coeff * Reduce(`*`, mapOfSprays[rho]))
      })
      tPowerSumPol <- new(
        "symbolicQspray",
        powers = psPoly@powers,
        coeffs = coeffs
      )
      as.bigq(c, zlambda(rho)) * tPowerSumPol
    }
  })
  Reduce(`+`, sprays)
}

#' @title t-Schur polynomial
#' @description Returns the t-Schur polynomial associated to
#'   the given partition.
#'
#' @param n number of variables, a positive integer
#' @param lambda integer partition
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the
#'   t-Schur polynomial associated to \code{lambda}.
#'   It has a single parameter usually denoted by \eqn{t} and its
#'   coefficients are polynomials in this parameter. Substituting
#'   \eqn{t} with \eqn{0} yields the Schur polynomials.
#' @export
#' @importFrom symbolicQspray showSymbolicQsprayOption<-
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#' @note The name "t-Schur polynomial" is taken from
#'   \href{https://www.sciencedirect.com/science/article/pii/S0097316518300724}{Wheeler and Zinn-Justin's paper}
#'   \emph{Hall polynomials, inverse Kostka polynomials and puzzles}.
tSchurPol <- function(n, lambda) {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda))
  out <- .tSkewSchurPolynomial(
    n, as.integer(removeTrailingZeros(lambda)), integer(0L)
  )
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("t"))
  out
}

#' @title Skew t-Schur polynomial
#' @description Returns the skew t-Schur polynomial associated to
#'   the given skew partition.
#'
#' @param n number of variables, a positive integer
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the skew
#'   t-Schur polynomial associated to the skew partition defined by
#'   \code{lambda} and \code{mu}.
#'   It has a single parameter usually denoted by \eqn{t} and its
#'   coefficients are polynomials in this parameter. Substituting
#'   \eqn{t} with \eqn{0} yields the skew Schur polynomials.
#' @export
#' @importFrom symbolicQspray showSymbolicQsprayOption<-
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
tSkewSchurPol <- function(n, lambda, mu) {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  out <- .tSkewSchurPolynomial(n, lambda, mu)
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("t"))
  out
}
