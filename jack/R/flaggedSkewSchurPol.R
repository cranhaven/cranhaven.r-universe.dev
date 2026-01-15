diffSequence <- function(x) {
  c(diff(-x), x[length(x)])
}

mkSkewPartition <- function(skewpart) {
  lapply(skewpart, function(row) {
    offset <- row[[1L]]
    c(rep(NA_integer_, offset), row[[2L]])
  })
}

flaggedSkewTableaux <- function(lambda, mu, a, b) {
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
  worker <- function(uus, vvs, dds, lb, i) {
    if(length(uus) == 0L) {
      list(list())
    } else {
      u <- uus[1L]
      us <- uus[-1L]
      v <- vvs[1L]
      vs <- vvs[-1L]
      d <- dds[1L]
      ds <- dds[-1L]
      do.call(c, lapply(row(b[i], v, a[i], lb), function(this) {
        lbprime <- c(rep(1L, d), this + 1L)
        lapply(worker(us, vs, ds, lbprime, i + 1L), function(rest) {
          c(list(list(u, this)), rest)
        })
      }))
    }
  }
  us <- c(as.integer(mu), rep(0L, length(lambda) - length(mu)))
  vs <- as.integer(lambda) - us
  if(any(vs < 0L)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  ds <- diffSequence(us)
  results <- worker(us, vs, ds, rep(1L, vs[1L]), 1L)
  lapply(results, mkSkewPartition)
}

skewTableauWeight <- function(tableau) {
  x <- Filter(Negate(is.na), unlist(tableau))
  m <- max(x)
  vapply(1L:m, function(i) {
    sum(x == i)
  }, integer(1L))
}

#' @title Flagged skew Schur polynomial
#' @description Computes a flagged skew Schur polynomial (which is not symmetric
#'   in general). See
#'   \href{https://www.symmetricfunctions.com/schurFlagged.htm#schurFlagged}{Schur polynomials (flagged)}
#'   for the definition.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param a,b lower bounds and upper bounds, weakly increasing vectors of
#'   integers; \code{lambda}, \code{a} and \code{b} must have the same length
#'
#' @return A \code{qspray} polynomial.
#' @export
#' @importFrom qspray qlone qzero qone
#' @importFrom utils head
#'
#' @examples
#' lambda <- c(3, 2, 2); mu <- c(2, 1)
#' n <- 3
#' a <- c(1, 1, 1); b <- c(n, n, n)
#' flaggedPoly <- flaggedSkewSchurPol(lambda, mu, a, b)
#' poly <- SkewSchurPol(n, lambda, mu)
#' flaggedPoly == poly # should be TRUE
flaggedSkewSchurPol <- function(lambda, mu, a, b) {
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- removeTrailingZeros(as.integer(lambda))
  l <- length(lambda)
  if(!(l == length(a) && l == length(b))) {
    stop("`lambda`, `a`, and `b` must have the same length.")
  }
  mu <- as.integer(removeTrailingZeros(mu))
  ellMu <- length(mu)
  if(ellMu == l && all(lambda == mu)) {
    return(qone())
  }
  if(l < ellMu) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  stopifnot(isIncreasing(a))
  stopifnot(isIncreasing(b))
  if(any(b < a)) {
    stop("The lower bounds (`a`) must be smaller than the upper bounds (`b`).")
  }
  tableaux <- flaggedSkewTableaux(lambda, mu, a, b)
  n <- b[l]
  qlones <- lapply(1L:n, qlone)
  out <- qzero()
  for(ssyt in tableaux) {
    weight <- skewTableauWeight(ssyt)
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
