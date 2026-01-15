makeTheMatrix <- function(A) {
  d <- nrow(A)
  M <- list()
  lambda <- qlone(1L)
  for(i in seq_len(d)) {
    for(j in seq_len(d)) {
      indices <- toString(c(i, j))
      if(i == j) {
        entry <- A[i, j][] - lambda
      } else {
        entry <- A[i, j][]
      }
      M[[indices]] <- entry
    }
  }
  attr(M, "nrow") <- d
  attr(M, "ncol") <- d
  M
}

dropRow <- function(M, i) {
  nrow <- attr(M, "nrow")
  ncol <- attr(M, "ncol")
  Mnew <- list()
  for(k in seq_len(nrow-1L)) {
    ii <- if(k < i) k else k+1L
    for(j in seq_len(ncol)) {
      Mnew[[toString(c(k, j))]] <- M[[toString(c(ii, j))]]
    }
  }
  attr(Mnew, "nrow") <- nrow - 1L
  attr(Mnew, "ncol") <- ncol
  Mnew
}

dropCol <- function(M, j) {
  nrow <- attr(M, "nrow")
  ncol <- attr(M, "ncol")
  Mnew <- list()
  for(k in seq_len(ncol-1L)) {
    jj <- if(k < j) k else k+1L
    for(i in seq_len(nrow)) {
      Mnew[[toString(c(i, k))]] <- M[[toString(c(i, jj))]]
    }
  }
  attr(Mnew, "nrow") <- nrow
  attr(Mnew, "ncol") <- ncol - 1L
  Mnew
}

minorMatrix <- function(M, i, j) {
  dropCol(dropRow(M, i), j)
}

detLaplace <- function(M) {
  d <- attr(M, "nrow")
  if(d == 1L) {
    M[[toString(c(1L, 1L))]]
  } else {
    result <- qzero()
    for(i in seq_len(d)) {
      entry <- M[[toString(c(i, 1L))]]
      if(i %% 2L == 0L) {
        result <- result + entry * detLaplace(minorMatrix(M, i, 1L))
      } else {
        result <- result - entry * detLaplace(minorMatrix(M, i, 1L))
      }
    }
    result
  }
}

#' @title Characteristic polynomial
#' @description Characteristic polynomial of a matrix.
#' 
#' @param A a square matrix with numeric, character, or \code{bigq} entries
#' 
#' @return A univariate \code{qspray} polynomial.
#' @export
#' @importFrom gmp is.matrixZQ as.bigq
#' 
#' @examples
#' set.seed(666)
#' A <- matrix(rpois(9L, 10), nrow = 3, ncol = 3)
#' ( P <- characteristicPolynomial(A) )
#' # check the roots are the eigen values:
#' f <- as.function(P, N = TRUE)
#' sapply(eigen(A)$values, f) # approx c(0, 0, 0)
characteristicPolynomial <- function(A) {
  if(!is.matrix(A) && !is.matrixZQ(A)) {
    stop("`A` must be a matrix.")
  }
  stopifnot(nrow(A) == ncol(A))
  A <- as.bigq(A)
  if(anyNA(A)) {
    stop("Invalid matrix.")
  }
  detLaplace(makeTheMatrix(A))
}
