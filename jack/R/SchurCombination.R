#' @title Symmetric polynomial in terms of the Schur polynomials
#' @description Expression of a symmetric polynomial as a linear combination
#'   of some Schur polynomials.
#'
#' @param qspray a \code{qspray} object defining a symmetric polynomial
#' @param check Boolean, whether to check the symmetry of \code{qspray}
#'
#' @return A list defining the combination. Each element of this list is a
#'   list with two elements: \code{coeff}, a \code{bigq} number, and
#'   \code{lambda}, an integer partition; then this list corresponds to the
#'   term \code{coeff * SchurPol(n, lambda)}, where \code{n} is the number of
#'   variables in the symmetric polynomial.
#' @export
#' @seealso \code{\link{JackCombination}}.
#' @importFrom syt KostkaNumbersWithGivenLambda
#' @importFrom gmp as.bigq
#' @importFrom qspray getConstantTerm MSPcombination qzero orderedQspray
#' @importFrom methods new
SchurCombination <- function(qspray, check = TRUE) {
  stopifnot(inherits(qspray, "qspray"))
  constantTerm <- getConstantTerm(qspray)
  combo <- MSPcombination(qspray - constantTerm, check = check)
  weights <- unique(vapply(combo, function(term) {
    sum(term[["lambda"]])
  }, integer(1L)))
  invKostkaMatrices <- lapply(weights, function(n) {
    lambdas <- listOfPartitions(n)
    nparts <- length(lambdas)
    lambdasAsStrings <-
      vapply(lambdas, partitionAsString, character(1L))
    KostkaMatrix <- matrix(0L, nrow = nparts, ncol = nparts)
    colnames(KostkaMatrix) <- lambdasAsStrings
    for(i in seq_len(nparts)) {
      kNumbers <- KostkaNumbersWithGivenLambda(lambdas[[i]], output = "vector")
      KostkaMatrix[i, names(kNumbers)] <- kNumbers
    }
    invKostkaMatrix <- backsolve(KostkaMatrix, diag(nparts))
    storage.mode(invKostkaMatrix) <- "integer"
    rownames(invKostkaMatrix) <- lambdasAsStrings
    list("matrix" = invKostkaMatrix, "lambdas" = lambdas)
  })
  names(invKostkaMatrices) <- as.character(weights)
  spray <- qzero()
  for(term in combo) {
    lambda <- term[["lambda"]]
    invKostkaMatrix <- invKostkaMatrices[[as.character(sum(lambda))]]
    invKostkaNumbers <- invKostkaMatrix[["matrix"]][partitionAsString(lambda), ]
    lambdas <- invKostkaMatrix[["lambdas"]]
    for(j in seq_along(lambdas)) {
      ikn <- invKostkaNumbers[j]
      if(ikn != 0L) {
        spray <- spray +
          new(
            "qspray",
            powers = list(lambdas[[j]]),
            coeffs = as.character(ikn * term[["coeff"]])
          )
      }
    }
  }
  spray <- orderedQspray(spray + constantTerm)
  lambdas <- spray@powers
  combo <- mapply(
    function(lambda, coeff) {
      list("lambda" = lambda, "coeff" = as.bigq(coeff))
    },
    lambdas, spray@coeffs,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  names(combo) <-
    vapply(lambdas, partitionAsString, character(1L))
  combo
}
