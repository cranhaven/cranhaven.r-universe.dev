#' @importFrom syt GelfandTsetlinPatterns
#' @noRd
MacdonaldPolynomialJinMSPbasis <- function(lambda) {
  mus <- listOfDominatedPartitions(lambda)
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
    psiLambdaMu(pair[[1L]], pair[[2L]])
  })
  names(pairsMap) <- vapply(allPairs, toString, character(1L))
  c <- clambda(lambda)
  lapply(seq_along(mus), function(i) {
    mu <- mus[[i]]
    listOfPairs <- listsOfPairs[[i]]
    rOQ <- c * Reduce(`+`, lapply(listOfPairs, function(pairs) {
      makeRatioOfSprays(pairsMap, pairs)
    }))
    list(
      "mu" = mu,
      "coeff" = rOQ@numerator
    )
  })
}

#' @importFrom qspray PSPcombination MSFpoly
#' @importFrom symbolicQspray isQzero
#' @noRd
MacdonaldPolynomialJinPSbasis <- function(mu) {
  macdonaldCombo <- MacdonaldPolynomialJinMSPbasis(mu)
  mapOfMaps <- lapply(macdonaldCombo, function(t1) {
    lambda <- t1[["mu"]]
    spray <- t1[["coeff"]]
    lapply(PSPcombination(MSFpoly(sum(lambda), lambda)), function(t2) {
      list(
        "lambda" = t2[["lambda"]],
        "lambdaAsString" = partitionAsString(t2[["lambda"]]),
        "coeff" = t2[["coeff"]] * spray
      )
    })
  })
  cmapOfMaps <- do.call(c, mapOfMaps)
  f <- vapply(cmapOfMaps, `[[`, character(1L), "lambdaAsString")
  Filter(
    Negate(is.null),
    lapply(split(cmapOfMaps, f), function(l) {
      spray <- Reduce(`+`, lapply(l, `[[`, "coeff"))
      if(isQzero(spray)) {
        NULL
      } else {
        list(
          "lambda" = l[[1L]][["lambda"]],
          "coeff" = spray
        )
      }
    })
  )
}

#' @importFrom qspray qlone
#' @importFrom ratioOfQsprays as.ratioOfQsprays
#' @noRd
.rOS_from_term <- function(powers, coeff) {
  q <- qlone(1)
  t <- qlone(2)
  if(length(powers) == 1L) {
    coeff * as.ratioOfQsprays(q^powers)
  } else {
    coeff *
      new(
        "ratioOfQsprays",
        numerator = q^(powers[1L]),
        denominator = t^(powers[2L])
      )
  }
}

.toROS <- function(spray) {
  Reduce(
    `+`,
    mapply(
      .rOS_from_term,
      spray@powers, spray@coeffs,
      USE.NAMES = FALSE, SIMPLIFY = FALSE
    )
  )
}

#' @title Modified Macdonald polynomial
#' @description Returns the modified Macdonald polynomial associated to a
#'   given integer partition.
#'
#' @param n number of variables, a positive integer
#' @param mu integer partition
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the modified
#'   Macdonald polynomial associated to the integer partition \code{mu}.
#'   It has two parameters and its coefficients are polynomials in these
#'   parameters.
#' @export
#' @importFrom symbolicQspray showSymbolicQsprayOption<- Qone
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#' @importFrom methods as
#' @importFrom qspray qlone qone PSFpoly
#' @importFrom utils tail
modifiedMacdonaldPol <- function(n, mu) {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(mu))
  mu <- as.integer(removeTrailingZeros(mu))
  ellMu <- length(mu)
  if(ellMu == 0L){
    return(Qone())
  }
  psCombo <- MacdonaldPolynomialJinPSbasis(mu)
  nmu <- sum(seq_len(ellMu - 1L) * tail(mu, -1L))
  t <- qlone(2L)
  unitSpray <- qone()
  out <- Reduce(
    `+`,
    lapply(psCombo, function(term) {
      lambda <- term[["lambda"]]
      spray <- term[["coeff"]]
      den_lambda <- Reduce(
        `*`,
        lapply(lambda, function(k) {
          t^k - unitSpray
        })
      )
      rOS <- .toROS(t^(nmu + sum(lambda)) * spray) / den_lambda
      rOS@numerator * as(PSFpoly(n, lambda), "symbolicQspray")
    })
  )
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("q", "t"))
  out
}
