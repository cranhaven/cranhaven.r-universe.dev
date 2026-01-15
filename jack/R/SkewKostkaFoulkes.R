#' @title Skew Kostka-Foulkes polynomial
#' @description Computes a skew Kostka-Foulkes polynomial.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param nu integer partition; the condition
#'   \code{sum(nu)==sum(lambda)-sum(mu)} is necessary in order to get a
#'   non-zero polynomial
#'
#' @return The skew Kostka-Foulkes polynomial associated to the skew
#'   partitiion defined by \code{lambda} and \code{mu} and to the partition
#'   \code{nu}. This is a univariate \code{qspray} polynomial whose value
#'   at \code{1} is the skew Kostka number associated to the skew partition
#'   defined by \code{lambda} and \code{mu} and to the partition \code{nu}.
#' @export
#' @importFrom syt skewTableauxWithGivenShapeAndWeight
#' @importFrom utils head
#' @importFrom qspray qsprayMaker qzero showQsprayOption<- showQsprayXYZ
SkewKostkaFoulkesPolynomial <- function(lambda, mu, nu) {
  stopifnot(isPartition(lambda), isPartition(mu), isPartition(nu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  if(any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  nu <- as.integer(removeTrailingZeros(nu))
  if(sum(lambda) == sum(mu) + sum(nu)) {
    skewTx <- skewTableauxWithGivenShapeAndWeight(lambda, mu, nu)
    charges <- lapply(skewTx, function(skewT) {
      charge(Filter(Negate(is.na), ssytWord(skewT)))
    })
    out <- qsprayMaker(powers = charges, coeffs = rep("1", length(charges)))
  } else {
    out <- qzero()
  }
  showQsprayOption(out, "showQspray") <- showQsprayXYZ("t")
  out
}
