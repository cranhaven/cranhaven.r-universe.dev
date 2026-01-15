#' @importFrom DescTools Permn
#' @importFrom methods new
#' @importFrom syt skewGelfandTsetlinPatterns
#' @noRd
.SkewMacdonaldPolynomial <- function(f, n, lambda, mu) {
  nus <- Filter(
    function(nu) length(nu) <= n,
    listOfDominatedPartitions(
      lastSubpartition(sum(lambda) - sum(mu), lambda)
    )
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
  pairsMap <- lapply(allPairs, function(pair) {
    f(pair[[1L]], pair[[2L]])
  })
  names(pairsMap) <- vapply(allPairs, toString, character(1L))
  nus <- nus[i_]
  QSprays <- lapply(seq_along(nus), function(i) {
    nu <- nus[[i]]
    listOfPairs <- listsOfPairs[[i]]
    rOQ <- Reduce(`+`, lapply(listOfPairs, function(pairs) {
      makeRatioOfSprays(pairsMap, pairs)
    }))
    compos <- Permn(c(nu, rep(0L, n - length(nu))))
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

#' @title Skew Macdonald polynomial
#' @description Returns the skew Macdonald polynomial associated to
#'   the given skew partition.
#'
#' @param n number of variables, a positive integer
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param which which skew Macdonald polynomial, \code{"P"}, \code{"Q"}
#'   or \code{"J"}
#'
#' @return A \code{symbolicQspray} multivariate polynomial, the skew
#'   Macdonald polynomial associated to the skew partition defined by
#'   \code{lambda} and \code{mu}. It has two parameters usually
#'   denoted by \eqn{q} and \eqn{t}. Substituting \eqn{q} with \eqn{0}
#'   yields the skew Hall-Littlewood polynomials.
#' @export
#' @importFrom symbolicQspray showSymbolicQsprayOption<- Qone Qzero
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
SkewMacdonaldPol <- function(n, lambda, mu, which = "P") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda))
  stopifnot(isPartition(mu))
  stopifnot(which %in% c("P", "Q", "J"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(lambda[seq_len(ellMu)] < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  if(ellLambda == ellMu && all(lambda == mu)) {
    return(Qone())
  }
  if(n == 0L){
    return(Qzero())
  }
  if(which == "P") {
    out <- .SkewMacdonaldPolynomial(psiLambdaMu, n, lambda, mu)
  } else if(which == "Q") {
    out <- .SkewMacdonaldPolynomial(phiLambdaMu, n, lambda, mu)
  } else {
    out <- clambdamu(lambda, mu) *
      .SkewMacdonaldPolynomial(psiLambdaMu, n, lambda, mu)
  }
  showSymbolicQsprayOption(out, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ(c("q", "t"))
  out
}
