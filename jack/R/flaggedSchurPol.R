flaggedSemiStandardYoungTableaux <- function(lambda, a, b) {
  row <- function(n, len, prev, xxs) {
    if(len == 0L) {
      list(integer(0L))
    } else {
      x <- xxs[[1L]]
      xs <- xxs[-1L]
      do.call(c, lapply(.rg(max(x, prev), n), function(j) {
        lapply(row(n, len - 1L, j, xs), function(js) {
          c(j, js)
        })
      }))
    }
  }
  worker <- function(prevRow, sss, i) {
    if(length(sss) == 0L) {
      list(list())
    } else {
      s <- sss[[1L]]
      ss <- sss[-1L]
      do.call(c, lapply(row(b[i], s, a[i], prevRow), function(r) {
        lapply(worker(r + 1L, ss, i + 1L), function(rs) {
          c(list(r), rs)
        })
      }))
    }
  }
  worker(rep(0L, lambda[1L]), lambda, 1L)
}

tableauWeight <- function(tableau) {
  x <- unlist(tableau)
  m <- max(x)
  vapply(1L:m, function(i) {
    sum(x == i)
  }, integer(1L))
}

#' @title Flagged Schur polynomial
#' @description Computes a flagged Schur polynomial (which is not symmetric
#'   in general). See
#'   \href{https://math.mit.edu/~apost/papers/degreeschub.pdf}{Chains in the Bruhat order}
#'   for the definition.
#'
#' @param lambda integer partition
#' @param a,b lower bounds and upper bounds, weakly increasing vectors of
#'   integers; \code{lambda}, \code{a} and \code{b} must have the same length
#'
#' @return A \code{qspray} polynomial.
#' @export
#' @importFrom qspray qlone qzero qone
#' @importFrom utils head
#'
#' @examples
#' lambda <- c(3, 2, 2)
#' n <- 3
#' a <- c(1, 1, 1); b <- c(n, n, n)
#' flaggedPoly <- flaggedSchurPol(lambda, a, b)
#' poly <- SchurPol(n, lambda)
#' flaggedPoly == poly # should be TRUE
flaggedSchurPol <- function(lambda, a, b) {
  stopifnot(isPartition(lambda))
  lambda <- removeTrailingZeros(as.integer(lambda))
  l <- length(lambda)
  if(l == 0L) {
    return(qone())
  }
  if(!(l == length(a) && l == length(b))) {
    stop("`lambda`, `a`, and `b` must have the same length.")
  }
  stopifnot(isIncreasing(a))
  stopifnot(isIncreasing(b))
  if(any(b < a)) {
    stop("The lower bounds (`a`) must be smaller than the upper bounds (`b`).")
  }
  tableaux <- flaggedSemiStandardYoungTableaux(lambda, a, b)
  n <- b[l]
  qlones <- lapply(1L:n, qlone)
  out <- qzero()
  for(ssyt in tableaux) {
    weight <- tableauWeight(ssyt)
    out <- out +
      Reduce(`*`, mapply(
        function(lone, w) {
          lone^w
        },
        head(qlones, length(weight)), weight,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      ))
  }
  out
}
