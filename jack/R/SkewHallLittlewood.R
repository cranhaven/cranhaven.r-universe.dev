#' # assumes lambda clean and length(mu)=length(lambda)
#' horizontalStrip <- function(lambda, mu) {
#'   ellLambda <- length(lambda)
#'   test <- lambda[1L] >= mu[1L]
#'   i <- 1L
#'   while(test && i < ellLambda) {
#'     j <- i + 1L
#'     k <- lambda[j]
#'     test <- mu[i] >= k && k >= mu[j]
#'     i <- j
#'   }
#'   test
#' }
#'
#' #' @importFrom utils head tail
#' #' @noRd
#' columnStrictTableau <- function(tableau) {
#'   all(
#'     mapply(
#'       horizontalStrip,
#'       head(tableau, -1L), tail(tableau, -1L),
#'       SIMPLIFY = TRUE, USE.NAMES = FALSE
#'     )
#'   )
#' }

#' @importFrom qspray qlone qone
#' @noRd
psi <- function(lambda, mu) {
  t <- qlone(1L)
  out <- qone()
  i_ <- seq_len(lambda[1L])
  mlambda <- vapply(i_, function(i) {
    sum(lambda == i)
  }, integer(1L))
  mmu <- vapply(i_, function(i) {
    sum(mu == i)
  }, integer(1L))
  for(i in i_) {
    mmu_i <- mmu[i]
    if(mmu_i == mlambda[i]+1L) {
      out <- out * (1L - t^mmu_i)
    }
  }
  out
}

phi <- function(lambda, mu) {
  t <- qlone(1L)
  out <- qone()
  i_ <- seq_len(lambda[1L])
  mlambda <- vapply(i_, function(i) {
    sum(lambda == i)
  }, integer(1L))
  mmu <- vapply(i_, function(i) {
    sum(mu == i)
  }, integer(1L))
  for(i in i_) {
    mlambda_i <- mlambda[i]
    if(mmu[i]+1L == mlambda_i) {
      out <- out * (1L - t^mlambda_i)
    }
  }
  out
}

# Combos <- function(a, b, n) {
#   if(n == 0L) {
#     return(matrix(NA_integer_, nrow = 1L, ncol = 0L))
#   }
#   if(n == 1L) {
#     return(cbind(a:b))
#   }
#   do.call(rbind, lapply(a:b, function(i) {
#     cbind(i, Combos(i, b, n-1L))
#   }))
# }
#
# cartesianProduct <- function(diffs) {
#   if(length(diffs) == 1L) {
#     return(cbind(rev(c(0L, seq_len(diffs)))))
#   }
#   previous <- cartesianProduct(tail(diffs, -1L))
#   do.call(rbind, lapply(rev(c(0L, seq_len(diffs[1L]))), function(i) {
#     cbind(i, previous)
#   }))
# }

# # assumes lambda is clean and length(mu)=length(lambda)
# Paths <- function(n, lambda, mu) {
#   diffs <- lambda - mu
#   Grid <- cartesianProduct(diffs)
#   kappas <- Filter(isDecreasing, apply(Grid, 1L, function(kappa) {
#     kappa + mu
#   }, simplify = FALSE))
#   # kappas identical to boundedNonIncrSeqs(0L, mu, lambda)
#   combos <- Combos(1L, length(kappas), n - 1L)
#   Filter(columnStrictTableau, apply(combos, 1L, function(combo) {
#     c(list(lambda), lapply(combo, function(i) {
#       kappas[[i]]
#     }), list(mu))
#   }, simplify = FALSE))
# }
# # compos = partitions::compositions(sum(lambda)-sum(mu), n)
# # do.call(c, apply(compos, 2L, function(w) {
# #   skewGelfandTsetlinPatterns(lambda, mu, w)
# #  }, simplify = FALSE))

#' @importFrom syt skewGelfandTsetlinPatterns
#' @noRd
Paths <- function(n, lambda, mu) {
  nus <- Filter(
    function(nu) length(nu) <= n,
    listOfDominatedPartitions(lastSubpartition(sum(lambda) - sum(mu), lambda))
  )
  Filter(
    Negate(is.null),
    lapply(nus, function(nu) {
      w <- c(nu, rep(0L, n - length(nu)))
      patterns <- skewGelfandTsetlinPatterns(lambda, mu, w)
      if(length(patterns) == 0L) {
        NULL
      } else {
        list(
          "weight" = w,
          "pairs" = lapply(patterns, function(pattern) {
            pairing(
              apply(pattern, 1L, removeTrailingZeros, simplify = FALSE)
            )
          })
        )
      }
    })
  )
}

#' @importFrom ratioOfQsprays as.ratioOfQsprays
#' @importFrom DescTools Permn
.SkewHallLittlewood <- function(f, n, lambda, mu) {
  paths <- Paths(n, lambda, mu)
  allPairs <- unique(
    do.call(
      c,
      do.call(
        c,
        lapply(paths, `[[`, "pairs")
      )
    )
  )
  listOfSprays <- lapply(allPairs, function(pair) {
    f(pair[[1L]], pair[[2L]])
  })
  names(listOfSprays) <-
    vapply(allPairs, toString, character(1L), USE.NAMES = FALSE)
  listsOfSprays <- lapply(paths, function(nu_listsOfPairs) {
    nu <- nu_listsOfPairs[[1L]]
    listsOfPairs <- nu_listsOfPairs[[2L]]
    sprays <- lapply(listsOfPairs, function(pairs) {
      Reduce(
        `*`,
        listOfSprays[vapply(pairs, toString, character(1L), USE.NAMES = FALSE)]
      )
    })
    listOfPowers <- apply(Permn(nu), 1L, removeTrailingZeros, simplify = FALSE)
    do.call(c, lapply(listOfPowers, function(powers) {
      lapply(sprays, function(spray) {
        new(
          "symbolicQspray",
          powers = list(powers),
          coeffs = list(as.ratioOfQsprays(spray))
        )
      })
    }))
  })
  Reduce(`+`, do.call(c, listsOfSprays))
}

#' @title Skew Hall-Littlewood polynomial
#' @description Returns the skew Hall-Littlewood polynomial associated to
#'   the given skew partition.
#'
#' @param n number of variables, a positive integer
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param which which skew Hall-Littlewood polynomial, \code{"P"} or \code{"Q"}
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the skew
#'   Hall-Littlewood polynomial associated to the skew partition defined by
#'   \code{lambda} and \code{mu}. It has a single parameter usually denoted
#'   by \eqn{t} and its coefficients are polynomial in this parameter.
#'   When substituting \eqn{t} with \eqn{0} in the skew Hall-Littlewood
#'   \eqn{P}-polynomials, one obtains the skew Schur polynomials.
#' @export
#' @importFrom symbolicQspray Qzero Qone showSymbolicQsprayOption<-
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#'
#' @examples
#' n <- 3; lambda <- c(3, 2, 1); mu <- c(1, 1)
#' skewHLpoly <- SkewHallLittlewoodPol(n, lambda, mu)
#' skewSchurPoly <- SkewSchurPol(n, lambda, mu)
#' substituteParameters(skewHLpoly, 0) == skewSchurPoly # should be TRUE
SkewHallLittlewoodPol <- function(n, lambda, mu, which = "P") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda), isPartition(mu))
  which <- match.arg(which, c("P", "Q"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  if(n == 0L){
    if(ellLambda == ellMu && all(lambda == mu)) {
      return(Qone())
    } else {
      return(Qzero())
    }
  }
  if(which == "P") {
    out <- .SkewHallLittlewood(psi, n, lambda, mu)
  } else {
    out <- .SkewHallLittlewood(phi, n, lambda, mu)
  }
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ("t")
  out
}
