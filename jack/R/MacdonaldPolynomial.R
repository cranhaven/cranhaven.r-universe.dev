codedRatio <- function(
    lambda, lambdap, mu, mup, ij
) {
  i <- ij[1L]
  j <- ij[2L]
  ellMu <- length(mu)
  if(i <= ellMu && j <= (mu_i <- mu[i])) {
    a <- mu_i - j
    l <- mup[j] - i
    ap <- lambda[i] - j
    lp <- lambdap[j] - i
    list(
      rbind(
        c(a + 1L, l),
        c(ap, lp + 1L)
      ),
      rbind(
        c(a, l + 1L),
        c(ap + 1L, lp)
      )
    )
  } else if(j <= (lambda_i <- lambda[i])) {
    ap <- lambda_i - j
    lp <- lambdap[j] - i
    list(
      rbind(
        c(ap, lp + 1L)
      ),
      rbind(
        c(ap + 1L, lp)
      )
    )
  } else {
    list(
      matrix(NA_integer_, nrow = 0L, ncol = 2L),
      matrix(NA_integer_, nrow = 0L, ncol = 2L)
    )
  }
}

#' @importFrom partitions conjugate
#' @noRd
psiLambdaMu <- function(lambda, mu) {
  lambdap <- conjugate(lambda)
  mup <- conjugate(mu)
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  nonEmptyRows <-
    c(which(lambda[seq_len(ellMu)] != mu), .rg(ellMu + 1L, ellLambda))
  emptyColumns <- which(lambdap[seq_along(mup)] == mup)
  ss <- do.call(
    rbind,
    lapply(nonEmptyRows, function(i) {
      columns <- Filter(function(x) x <= lambda[i], emptyColumns)
      cbind(rep(i, length(columns)), columns)
    })
  )
  if(nrow(ss) >= 1L) {
    codedRatios <- apply(ss, 1L, function(ij) {
      codedRatio(lambda, lambdap, mu, mup, ij)
    }, simplify = FALSE)
    list(
      do.call(
        rbind,
        lapply(codedRatios, `[[`, 2L)
      ),
      do.call(
        rbind,
        lapply(codedRatios, `[[`, 1L)
      )
    )
  } else {
    list(
      matrix(NA_integer_, nrow = 0L, ncol = 2L),
      matrix(NA_integer_, nrow = 0L, ncol = 2L)
    )
  }
}

#' @importFrom partitions conjugate
#' @noRd
phiLambdaMu <- function(lambda, mu) {
  lambdap <- conjugate(lambda)
  mup <- conjugate(mu)
  ellLambdap <- length(lambdap)
  ellMup <- length(mup)
  nonEmptyColumns <-
    c(which(lambdap[seq_len(ellMup)] != mup), .rg(ellMup + 1L, ellLambdap))
  ss <- do.call(
    rbind,
    lapply(nonEmptyColumns, function(j) {
      lambdap_j <- lambdap[j]
      cbind(seq_len(lambdap_j), rep(j, lambdap_j))
    })
  )
  if(nrow(ss) >= 1L) {
    codedRatios <- apply(ss, 1L, function(ij) {
      codedRatio(lambda, lambdap, mu, mup, ij)
    }, simplify = FALSE)
    list(
      do.call(
        rbind,
        lapply(codedRatios, `[[`, 1L)
      ),
      do.call(
        rbind,
        lapply(codedRatios, `[[`, 2L)
      )
    )
  } else {
    list(
      matrix(NA_integer_, nrow = 0L, ncol = 2L),
      matrix(NA_integer_, nrow = 0L, ncol = 2L)
    )
  }
}

gtPatternDiagonals <- function(pattern) {
  ell <- length(pattern)
  c(list(integer(0L)), lapply(seq_len(ell), function(j) {
    indices <- cbind((ell-j+1L):ell, seq_len(j))
    removeTrailingZeros(do.call(c, apply(indices, 1L, function(rc) {
      pattern[[rc[1L]]][rc[2L]]
    }, simplify = FALSE)))
  }))
}

simplifyTheTwoMatrices <- function(matrix1, matrix2) {
  pairs1 <- apply(matrix1, 1L, toString)
  pairs2 <- apply(matrix2, 1L, toString)
  allPairs <- union(pairs1, pairs2)
  table1 <- table(factor(pairs1, levels = allPairs))
  table2 <- table(factor(pairs2, levels = allPairs))
  diffs <- table1 - table2
  pairsNumerator <- Filter(function(count) count > 0L, diffs)
  pairsDenominator <- Filter(function(count) count > 0L, -diffs)
  rownames(matrix1) <- pairs1
  rownames(matrix2) <- pairs2
  colnames(matrix1) <- colnames(matrix2) <- c("i", "j")
  list(
    cbind(
      matrix1[names(pairsNumerator), , drop = FALSE],
      count = pairsNumerator,
      deparse.level = 0L
    ),
    cbind(
      matrix2[names(pairsDenominator), , drop = FALSE],
      count = pairsDenominator,
      deparse.level = 0L
    )
  )
}

#' @importFrom qspray qone qlone
#' @noRd
makeRatioOfSprays <- function(pairsMap, pairs) {
  pairsOfMatrices <- pairsMap[vapply(pairs, toString, character(1L))]
  matrix1 <- do.call(
    rbind,
    lapply(pairsOfMatrices, `[[`, 1L)
  )
  matrix2 <- do.call(
    rbind,
    lapply(pairsOfMatrices, `[[`, 2L)
  )
  simplifiedMatrices <- simplifyTheTwoMatrices(matrix1, matrix2)
  matrix1 <- simplifiedMatrices[[1L]]
  matrix2 <- simplifiedMatrices[[2L]]
  q <- qlone(1L)
  t <- qlone(2L)
  unitQSpray <- qone()
  if(nrow(matrix1) >= 1L) {
    num <- Reduce(
      `*`,
      apply(matrix1, 1L, function(alc) {
        (unitQSpray - q^(alc[1L]) * t^(alc[2L]))^alc[3L]
      }, simplify = FALSE)
    )
  } else {
    num <- unitQSpray
  }
  if(nrow(matrix2) >= 1L) {
    den <- Reduce(
      `*`,
      apply(matrix2, 1L, function(alc) {
        (unitQSpray - q^(alc[1L]) * t^(alc[2L]))^alc[3L]
      }, simplify = FALSE)
    )
  } else {
    den <- unitQSpray
  }
  num / den
}


#' @importFrom qspray qlone qone
#' @importFrom partitions conjugate
#' @noRd
clambda <- function(lambda) {
  q <- qlone(1)
  t <- qlone(2)
  lambdap <- conjugate(lambda)
  unitSpray <- qone()
  sprays <- do.call(
    c,
    lapply(seq_along(lambda), function(i) {
      lambda_i <- lambda[i]
      lapply(seq_len(lambda_i), function(j) {
        a <- lambda_i - j
        l <- lambdap[j] - i
        unitSpray - q^a * t^(l + 1L)
      })
    })
  )
  Reduce(`*`, sprays)
}

#' @importFrom DescTools Permn
#' @importFrom methods new
#' @importFrom syt GelfandTsetlinPatterns
#' @noRd
.MacdonaldPolynomial <- function(f, n, lambda) {
  mus <- Filter(
    function(mu) length(mu) <= n,
    listOfDominatedPartitions(lambda)
  )
  listsOfPairs <- lapply(mus, function(mu) {
    lapply(GelfandTsetlinPatterns(lambda, mu), function(pattern) {
      pairing(gtPatternDiagonals(pattern))
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
  pairsMap <- lapply(allPairs, function(pair) {
    f(pair[[1L]], pair[[2L]])
  })
  names(pairsMap) <- vapply(allPairs, toString, character(1L))
  QSprays <- lapply(seq_along(mus), function(i) {
    mu <- mus[[i]]
    listOfPairs <- listsOfPairs[[i]]
    rOQ <- Reduce(`+`, lapply(listOfPairs, function(pairs) {
      makeRatioOfSprays(pairsMap, pairs)
    }))
    compos <- Permn(c(mu, rep(0L, n - length(mu))))
    powers <- apply(compos, 1L, removeTrailingZeros, simplify = FALSE)
    list(
      "powers" = powers,
      "coeffs" = rep(list(rOQ), length(powers))
    )
  })
  new(
    "symbolicQspray",
    powers = do.call(
      c,
      lapply(QSprays, `[[`, "powers")
    ),
    coeffs = do.call(
      c,
      lapply(QSprays, `[[`, "coeffs")
    )
  )
}

#' @title Macdonald polynomial
#' @description Returns the Macdonald polynomial associated to
#'   the given integer partition.
#'
#' @param n number of variables, a positive integer
#' @param lambda integer partition
#' @param which which Macdonald polynomial, \code{"P"}, \code{"Q"},
#'   or \code{"J"}
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the
#'   Macdonald polynomial associated to the integer partition
#'   \code{lambda}. It has two parameters usually denoted by \eqn{q}
#'   and \eqn{t}. Substituting \eqn{q} with \eqn{0} yields the
#'   Hall-Littlewood polynomials.
#' @export
#' @importFrom symbolicQspray showSymbolicQsprayOption<- Qone Qzero
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
MacdonaldPol <- function(n, lambda, which = "P") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda))
  stopifnot(which %in% c("P", "Q", "J"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  ellLambda <- length(lambda)
  if(ellLambda == 0L) {
    return(Qone())
  }
  if(n < ellLambda){
    return(Qzero())
  }
  if(which == "P") {
    out <- .MacdonaldPolynomial(psiLambdaMu, n, lambda)
  } else if(which == "Q") {
    out <- .MacdonaldPolynomial(phiLambdaMu, n, lambda)
  } else {
    out <- clambda(lambda) * .MacdonaldPolynomial(psiLambdaMu, n, lambda)
  }
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("q", "t"))
  out
}
