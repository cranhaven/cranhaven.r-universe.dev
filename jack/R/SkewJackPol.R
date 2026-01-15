#' @importFrom syt skewGelfandTsetlinPatterns
#' @noRd
.skewJackInMSPbasis <- function(func, ccoeff, which, lambda, mu) {
  nus <- listOfDominatedPartitions(
    lastSubpartition(sum(lambda) - sum(mu), lambda)
  )
  listsOfPatterns <- lapply(nus, function(nu) {
    skewGelfandTsetlinPatterns(lambda, mu, nu)
  })
  i_ <- which(lengths(listsOfPatterns) != 0L)
  listsOfPairs <- lapply(listsOfPatterns[i_], function(patterns) {
    lapply(patterns, function(pattern) {
      pairing(apply(pattern, 1L, removeTrailingZeros, simplify = FALSE))
    })
  })
  allPairs <- unique(
    do.call(
      c,
      do.call(
        c,
        listsOfPairs
      )
    )
  )
  if(which == "Q") {
    funcLambdaMu <- phiLambdaMu
  } else {
    funcLambdaMu <- psiLambdaMu
  }
  pairsMap <- lapply(allPairs, function(pair) {
    funcLambdaMu(pair[[1L]], pair[[2L]])
  })
  names(pairsMap) <- vapply(allPairs, toString, character(1L))
  nus <- nus[i_]
  names(listsOfPairs) <- vapply(nus, partitionAsString, character(1L))
  makeAssocsFromPairs <- function(pairs) {
    pairsOfMatrices <- pairsMap[vapply(pairs, toString, character(1L))]
    matrix1 <- do.call(
      rbind,
      lapply(pairsOfMatrices, `[[`, 1L)
    )
    matrix2 <- do.call(
      rbind,
      lapply(pairsOfMatrices, `[[`, 2L)
    )
    if(is.null(matrix1)) {
      matrix1 <- matrix(NA_integer_, nrow = 0L, ncol = 2L)
    }
    if(is.null(matrix2)) {
      matrix2 <- matrix(NA_integer_, nrow = 0L, ncol = 2L)
    }
    simplifyTheTwoMatrices(matrix1, matrix2)
  }
  makeCoeffFromListOfPairs <- function(listOfPairs) {
    coeff <-
      Reduce(
        `+`,
        lapply(listOfPairs, function(pairs) {
          func(makeAssocsFromPairs(pairs))
        })
      )
    if(which == "J") {
      c <- func(.clambdamuMatrices(lambda, mu))
      c * coeff
    } else if(which == "C") {
      c <- func(.clambdamuMatrices(lambda, mu))
      cc <- ccoeff(lambda, mu)
      c * cc * coeff
    } else {
      coeff
    }
  }
  mapply(
    function(listOfPairs, nu) {
      list(
        "nu" = nu,
        "ellNu" = length(nu),
        "coeff" = makeCoeffFromListOfPairs(listOfPairs)
      )
    },
    listsOfPairs, nus,
    USE.NAMES = TRUE, SIMPLIFY = FALSE
  )
}

#' @importFrom qspray qlone qone
#' @noRd
skewSymbolicJackInMSPbasis <- function(which, lambda, mu) {
  alpha <- qlone(1L)
  poly_from_alc <- function(alc) {
    (alc[1L] * alpha + alc[2L])^(alc[3L])
  }
  poly_from_alcs <- function(alcs) {
    Reduce(`*`, apply(alcs, 1L, poly_from_alc, simplify = FALSE))
  }
  rosFromMatrices <- function(alcsMatrices) {
    matrix1 <- alcsMatrices[[1L]]
    if(nrow(matrix1) >= 1L) {
      num <- poly_from_alcs(matrix1)
    } else {
      num <- qone()
    }
    matrix2 <- alcsMatrices[[2L]]
    if(nrow(matrix2) >= 1L) {
      den <- poly_from_alcs(matrix2)
    } else {
      den <- qone()
    }
    num / den
  }
  ccoeff <- function(.lambda, .mu) {
    symbolicJackCcoefficient(.lambda) / symbolicJackCcoefficient(.mu)
  }
  .skewJackInMSPbasis(rosFromMatrices, ccoeff, which, lambda, mu)
}

#' @importFrom gmp c_bigq as.bigq
#' @noRd
skewJackInMSPbasis <- function(alpha, which, lambda, mu) {
  coeff_from_alc <- function(alc) {
    (alc[1L] * alpha + alc[2L])^(alc[3L])
  }
  coeff_from_alcs <- function(alcs) {
    prod(c_bigq(apply(alcs, 1L, coeff_from_alc, simplify = FALSE)))
  }
  ratioFromMatrices <- function(alcsMatrices) {
    matrix1 <- alcsMatrices[[1L]]
    if(nrow(matrix1) >= 1L) {
      num <- coeff_from_alcs(matrix1)
    } else {
      num <- as.bigq(1L)
    }
    matrix2 <- alcsMatrices[[2L]]
    if(nrow(matrix2) >= 1L) {
      den <- coeff_from_alcs(matrix2)
    } else {
      den <- as.bigq(1L)
    }
    num / den
  }
  ccoeff <- function(.lambda, .mu) {
    JackCcoefficient(.lambda, alpha) / JackCcoefficient(.mu, alpha)
  }
  .skewJackInMSPbasis(ratioFromMatrices, ccoeff, which, lambda, mu)
}

#' Skew Jack polynomial
#' @description Computes a skew Jack polynomial with a given Jack parameter.
#'
#' @param n positive integer, the number of variables
#' @param lambda outer integer partition of the skew partition
#' @param mu inner integer partition of the skew partition; it must be a
#'   subpartition of \code{lambda}
#' @param alpha the Jack parameter, any object coercible to a \code{bigq}
#'   number
#' @param which which skew Jack polynomial, \code{"J"}, \code{"P"}, \code{"Q"}
#'   or \code{"C"}
#'
#' @return A \code{qspray} polynomial.
#' @export
#' @seealso \code{\link{SkewJackSymPol}}.
#' @importFrom gmp as.bigq
#' @importFrom DescTools Permn
#' @importFrom methods new
#' @importFrom utils head
#' @importFrom qspray qone qzero
#'
#' @examples
#' SkewJackPol(3, c(3,1), c(2), "2")
SkewJackPol <- function(n, lambda, mu, alpha, which = "J") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid alpha.")
  }
  which <- match.arg(which, c("J", "P", "Q", "C"))
  if(n == 0L) {
    if(ellLambda == ellMu && all(lambda == mu)) {
      return(qone())
    } else {
      return(qzero())
    }
  }
  msCombo <-
    Filter(
      function(lst) {
        lst[["ellNu"]] <= n
      },
      skewJackInMSPbasis(alpha, which, lambda, mu)
    )
  powers_and_coeffs <-
    Reduce(
      function(l1, l2) {
        list(
          "powers" = c(l1[["powers"]], l2[["powers"]]),
          "coeffs" = c(l1[["coeffs"]], l2[["coeffs"]])
        )
      },
      lapply(msCombo, function(lst) {
        nu <- c(lst[["nu"]], rep(0L, n - lst[["ellNu"]]))
        coeff <- as.character(lst[["coeff"]])
        powers <- apply(Permn(nu), 1L, removeTrailingZeros, simplify = FALSE)
        list(
          "powers" = powers,
          "coeffs" = rep(coeff, length(powers))
        )
      })
    )
  new(
    "qspray",
    powers = powers_and_coeffs[["powers"]],
    coeffs = powers_and_coeffs[["coeffs"]]
  )
}

#' Skew Jack polynomial with symbolic Jack parameter
#' @description Computes a skew Jack polynomial with a symbolic Jack parameter.
#'
#' @param n positive integer, the number of variables
#' @param lambda outer integer partition of the skew partition
#' @param mu inner integer partition of the skew partition; it must be a
#'   subpartition of \code{lambda}
#' @param which which skew Jack polynomial, \code{"J"}, \code{"P"}, \code{"Q"}
#'   or \code{"C"}
#'
#' @return A \code{symbolicQspray} polynomial.
#' @export
#' @importFrom DescTools Permn
#' @importFrom methods new
#' @importFrom utils head
#' @importFrom symbolicQspray Qone Qzero
#'
#' @examples
#' SkewJackSymPol(3, c(3,1), c(2))
SkewJackSymPol <- function(n, lambda, mu, which = "J") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  which <- match.arg(which, c("J", "P", "Q", "C"))
  if(n == 0L) {
    if(ellLambda == ellMu && all(lambda == mu)) {
      return(Qone())
    } else {
      return(Qzero())
    }
  }
  msCombo <-
    Filter(
      function(lst) {
        lst[["ellNu"]] <= n
      },
      skewSymbolicJackInMSPbasis(which, lambda, mu)
    )
  powers_and_coeffs <-
    Reduce(
      function(l1, l2) {
        list(
          "powers" = c(l1[["powers"]], l2[["powers"]]),
          "coeffs" = c(l1[["coeffs"]], l2[["coeffs"]])
        )
      },
      lapply(msCombo, function(lst) {
        nu <- c(lst[["nu"]], rep(0L, n - lst[["ellNu"]]))
        coeff <- lst[["coeff"]]
        powers <- apply(Permn(nu), 1L, removeTrailingZeros, simplify = FALSE)
        list(
          "powers" = powers,
          "coeffs" = rep(list(coeff), length(powers))
        )
      })
    )
  new(
    "symbolicQspray",
    powers = powers_and_coeffs[["powers"]],
    coeffs = powers_and_coeffs[["coeffs"]]
  )
}
