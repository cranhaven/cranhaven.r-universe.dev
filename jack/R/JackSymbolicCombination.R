#' @importFrom utils head
#' @noRd
invTriMatrix <- function(L) {
  d <- length(L)
  f <- 1L / L[[d]][[1L]]
  if(d == 1L) {
    invL <- L
    invL[[1L]][[1L]] <- f
    return(invL)
  } else {
    B <- invTriMatrix(lapply(head(L, -1L), function(row) {
      head(row, -1L)
    }))
    newColumn <- lapply(seq_len(d-1L), function(i) {
      toAdd <- mapply(
        `*`,
        B[[i]], lapply(i:(d-1L), function(j) {
          L[[j]][[d-j+1L]]
        }),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
      -Reduce(`+`, toAdd) * f
    })
    B <- c(B, list(list()))
    newColumn <- c(newColumn, list(f))
    names(B) <- names(newColumn) <- names(L)
    Names <- lapply(L, names)
    mapply(
      function(row, x, nms) {
        out <- c(row, list(x))
        names(out) <- nms
        out
      },
      B, newColumn, Names,
      SIMPLIFY = FALSE, USE.NAMES = TRUE
    )
  }
}

msPolynomialsInJackSymbolicBasis <- function(which, n, weight) {
  symbolicKN <- .symbolicKostkaNumbers(n, weight, which)
  invTriMatrix(symbolicKN)
}

#' @title Symmetric polynomial in terms of symbolic Jack polynomials
#' @description Expression of a symmetric polynomial as a linear combination
#'   of Jack polynomials with a symbolic Jack parameter.
#'
#' @param qspray a \code{qspray} object or a \code{symbolicQspray} object
#'   defining a symmetric polynomial
#' @param which which Jack polynomials, \code{"J"}, \code{"P"}, \code{"Q"} or
#'   \code{"C"}
#' @param check Boolean, whether to check the symmetry
#'
#' @return A list defining the combination. Each element of this list is a
#'   list with two elements: \code{coeff}, a \code{bigq} number, and
#'   \code{lambda}, an integer partition; then this list corresponds to the
#'   term \code{coeff * JackSymPol(n, lambda, which)}, where \code{n} is
#'   the number of variables in the symmetric polynomial.
#' @export
#' @importFrom methods new
#' @importFrom qspray MSPcombination orderedQspray isConstant isQzero getConstantTerm numberOfVariables
#' @importFrom symbolicQspray Qzero isConstant isQzero getConstantTerm numberOfVariables
#' @importFrom ratioOfQsprays as.ratioOfQsprays
symbolicJackCombination <- function(qspray, which = "J", check = TRUE) {
  stopifnot(inherits(qspray, "qspray") || inherits(qspray, "symbolicQspray"))
  if(isConstant(qspray)) {
    if(isQzero(qspray)) {
      out <- list()
    } else {
      out <-
        list(list(
          "coeff" = as.ratioOfQsprays(getConstantTerm(qspray)),
          "lambda" = integer(0L)
        ))
      names(out) <- "[]"
    }
    return(out)
  }
  constantTerm <- getConstantTerm(qspray)
  which <- match.arg(which, c("J", "P", "Q", "C"))
  fullMsCombo <- MSPcombination(qspray - constantTerm, check = check)
  lambdas <- lapply(fullMsCombo, `[[`, "lambda")
  weights <- unique(vapply(lambdas, sum, integer(1L)))
  n <- numberOfVariables(qspray)
  finalQspray <- Qzero()
  unitRatioOfQsprays <- as.ratioOfQsprays(1L)
  for(weight in weights) {
    invKostkaMatrix <- msPolynomialsInJackSymbolicBasis(which, n, weight)
    kappas <- lapply(names(invKostkaMatrix), fromPartitionAsString)
    msCombo <- Filter(function(t) {sum(t[["lambda"]]) == weight}, fullMsCombo)
    coeffs <- lapply(msCombo, `[[`, "coeff")
    sprays <- lapply(kappas, function(kappa) {
      new(
        "symbolicQspray",
        powers = list(kappa),
        coeffs = list(unitRatioOfQsprays)
      )
    })
    names(sprays) <- names(invKostkaMatrix)
    lambdas <- names(msCombo)
    for(i in seq_along(lambdas)) {
      invKostkaNumbers <- invKostkaMatrix[[lambdas[i]]]
      spray <- Qzero()
      for(kappa in names(invKostkaNumbers)) {
        coeff <- invKostkaNumbers[[kappa]]
        if(coeff != 0L) {
          spray <- spray + coeff * sprays[[kappa]]
        }
      }
      finalQspray <- finalQspray + coeffs[[i]]*spray
    }
  }
  finalQspray <- orderedQspray(finalQspray)
  powers <- finalQspray@powers
  coeffs <- finalQspray@coeffs
  combo <- mapply(
    function(lambda, coeff) {
      list("coeff" = coeff, "lambda" = lambda)
    },
    powers, coeffs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  names(combo) <-
    vapply(powers, partitionAsString, character(1L), USE.NAMES = FALSE)
  if(constantTerm != 0L) {
    combo <-
      c(
        combo,
        list("[]" = list(
          "coeff" = as.ratioOfQsprays(constantTerm),
          "lambda" = integer(0L)
          )
        )
      )
  }
  combo
}
