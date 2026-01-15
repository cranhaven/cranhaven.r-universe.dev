#' @title qt-Kostka polynomials
#' @description qt-Kostka polynomials, aka Kostka-Macdonald polynomials.
#'
#' @param mu integer partition
#'
#' @return A list. The qt-Kostka polynomials are usually denoted by
#'   \eqn{K_{\lambda, \mu}(q, t)} where \eqn{q} and \eqn{t} denote the two
#'   variables and \eqn{\lambda} and \eqn{\mu} are two integer partitions.
#'   One obtains the Kostka-Foulkes polynomials by substituting \eqn{q}
#'   with \eqn{0}.
#'   For a given partition \eqn{\mu}, the function returns the
#'   polynomials \eqn{K_{\lambda, \mu}(q, t)} as \code{qspray} objects
#'   for all partitions \eqn{\lambda} of the same weight as \eqn{\mu}. The
#'   generated list is a list of lists with two elements: the integer
#'   partition \eqn{\lambda} and the polynomial.
#' @export
#' @importFrom qspray MSPcombination PSFpoly qlone qone showQsprayOption<- showQsprayXYZ
#' @importFrom RationalMatrix Qinverse
#' @importFrom partitions parts
#' @importFrom gmp c_bigq
qtKostkaPolynomials <- function(mu) {
  stopifnot(isPartition(mu))
  mu <- as.integer(removeTrailingZeros(mu))
  n <- sum(mu)
  if(n == 0L) {
    out <- list(
      list(
        "lambda" = integer(0L),
        "polynomial" = qone()
      )
    )
    names(out) <- partitionAsString(integer(0L))
    return(out)
  }
  psCombo <- MacdonaldPolynomialJinPSbasis(mu)
  iknMatrix <- Qinverse(KostkaJackNumbers(n))
  lambdas <- listOfPartitions(n)
  lambdasAsStrings <-
    vapply(lambdas, partitionAsString, character(1L))
  rownames(iknMatrix) <- lambdasAsStrings
  coeffs <- function(lambda) {
    combo <- MSPcombination(PSFpoly(length(lambda), lambda), check = FALSE)
    out <- lapply(seq_along(lambdas), function(i) {
      list(
        "lambda" = lambdas[[i]],
        "coeff" = sum(c_bigq(lapply(names(combo), function(p) {
          combo[[p]][["coeff"]] * iknMatrix[p, i]
        })))
      )
    })
    names(out) <- lambdasAsStrings
    out
  }
  coeffsMap <- lapply(lambdas, coeffs)
  names(coeffsMap) <- lambdasAsStrings
  t <- qlone(2L)
  unitSpray <- qone()
  maps <- lapply(names(psCombo), function(p) {
    lambda <- psCombo[[p]][["lambda"]]
    spray <- psCombo[[p]][["coeff"]]
    coeffs_lambda <- coeffsMap[[p]]
    den_lambda <- Reduce(
      `*`,
      lapply(lambda, function(k) {
        unitSpray - t^k
      })
    )
    lapply(lambdasAsStrings, function(lambdaAsString) {
      coeff_lambda <- coeffs_lambda[[lambdaAsString]]
      list(
        "lambda" = coeff_lambda[["lambda"]],
        "lambdaAsString" = lambdaAsString,
        "coeff" =  coeff_lambda[["coeff"]] * spray / den_lambda
      )
    })
  })
  cmapOfMaps <- do.call(c, maps)
  f <- vapply(cmapOfMaps, `[[`, character(1L), "lambdaAsString")
  lapply(split(cmapOfMaps, f), function(l) {
    rOQ <- Reduce(`+`, lapply(l, `[[`, "coeff"))
    spray <- rOQ@numerator
    showQsprayOption(spray, "showQspray") <- showQsprayXYZ(c("q", "t"))
    list(
      "lambda" = l[[1L]][["lambda"]],
      "polynomial" = spray
    )
  })
}

#' @title Skew qt-Kostka polynomials
#' @description Skew qt-Kostka polynomials associated to a given skew
#'    partition.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#'
#' @return A list. The skew qt-Kostka polynomials are usually denoted by
#'   \eqn{K_{\lambda/\mu, \nu}(q, t)} where \eqn{q} and \eqn{t} denote the two
#'   variables, \eqn{\lambda} and \eqn{\mu} are the two integer partitions
#'   defining the skew partition, and \eqn{\nu} is an integer partition.
#'   One obtains the skew Kostka-Foulkes polynomials by substituting \eqn{q}
#'   with \eqn{0}.
#'   For given partitions \eqn{\lambda} and \eqn{\mu}, the function returns the
#'   polynomials \eqn{K_{\lambda/\mu, \nu}(q, t)} as \code{qspray} objects
#'   for all partitions \eqn{\nu} of the same weight as the skew partition. The
#'   generated list is a list of lists with two elements: the integer
#'   partition \eqn{\nu} and the polynomial.
#' @export
#' @importFrom qspray qone showQsprayOption<- showQsprayXYZ
qtSkewKostkaPolynomials <- function(lambda, mu) {
  stopifnot(isPartition(lambda))
  stopifnot(isPartition(mu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(lambda[seq_len(ellMu)] < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  w <- sum(lambda) - sum(mu)
  if(w == 0L){
    out <- list(
      list(
        "nu" = integer(0L),
        "polynomial" = qone()
      )
    )
    names(out) <- partitionAsString(integer(0L))
    return(out)
  }
  lrCoeffs <- LRskew(lambda, mu, output = "list")
  nus <- listOfPartitions(w)
  out <- lapply(nus, function(nu) {
    qtKostkaPolys <- qtKostkaPolynomials(nu)
    pis <- intersect(names(lrCoeffs), names(qtKostkaPolys))
    poly <- Reduce(
      `+`,
      lapply(pis, function(pi) {
        lrCoeffs[[pi]][["coeff"]] * qtKostkaPolys[[pi]][["polynomial"]]
      })
    )
    showQsprayOption(poly, "showQspray") <- showQsprayXYZ(c("q", "t"))
    list(
      "nu" = nu,
      "polynomial" = poly
    )
  })
  names(out) <- vapply(nus, partitionAsString, character(1L))
  out
}
