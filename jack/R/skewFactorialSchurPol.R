#' @title Skew factorial Schur polynomial
#' @description Computes the skew factorial Schur polynomial associated to a
#'   given skew partition.
#'
#' @param n number of variables
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param a vector of \code{bigq} numbers, or vector of elements coercible
#'   to \code{bigq} numbers; this vector corresponds to the sequence denoted by
#'   \eqn{a} in the
#'   \href{https://www.kurims.kyoto-u.ac.jp/EMIS/journals/SLC/opapers/s28macdonald.pdf}{reference paper},
#'   section \strong{6th Variation} (in this paper \eqn{a} is a doubly
#'   infinite sequence, but only a finite number of indices are not involved);
#'   the length of this vector must be large enough (an error will be thrown
#'   if it is too small) but it is not easy to know the minimal possible length
#' @param i0 positive integer, the index of \code{a} that must be considered
#'   as the zero index of the sequence denoted by \eqn{a} in the reference
#'   paper
#'
#' @return A \code{qspray} polynomial.
#' @export
#' @importFrom syt all_ssSkewTableaux
#' @importFrom qspray qlone qone qzero
#'
#' @references I.G. Macdonald.
#' \emph{Schur functions: theme and variations}.
#' Publ. IRMA Strasbourg, 1992.
#'
#' @examples
#' # for a=c(0, 0, ...), the skew factorial Schur polynomial is the
#' # skew Schur polynomial; let's check
#' n <- 4
#' lambda <- c(3, 3, 2, 2); mu <- c(2, 2)
#' a <- rep(0, 9)
#' i0 <- 3
#' skewFactorialSchurPoly <- SkewFactorialSchurPol(n, lambda, mu, a, i0)
#' skewSchurPoly <- SkewSchurPol(n, lambda, mu)
#' skewFactorialSchurPoly == skewSchurPoly # should be TRUE
SkewFactorialSchurPol <- function(n, lambda, mu, a, i0) {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda), isPartition(mu))
  stopifnot(isPositiveInteger(i0), i0 >= 1L)
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  mu <- c(mu, rep(0L, ellLambda - ellMu))
  if(any(lambda < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  if(n == 0L) {
    if(all(lambda == mu)) {
      return(qone())
    } else {
      return(qzero())
    }
  }
  tableaux <- all_ssSkewTableaux(lambda, mu, n)
  i_ <- 1L:ellLambda
  qlones <- lapply(1L:n, qlone)
  toAdd <- lapply(tableaux, function(tableau) {
    factors <- lapply(i_, function(i) {
      toMultiply <- lapply(.rg(mu[i]+1L, lambda[i]), function(j) {
        entry <- tableau[[i]][j]
        k <- entry + j - i
        qlones[[entry]] + a[k + i0]
      })
      Reduce(`*`, toMultiply)
    })
    Reduce(`*`, factors)
  })
  Reduce(`+`, toAdd)
}
