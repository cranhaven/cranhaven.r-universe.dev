isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x > 0
}

integerRange <- function(start, stop) {
  if(stop >= start) {
    start:stop
  } else {
    integer(0L)
  }
}

inversePermutation <- function(p) {
  p[p] <- seq_along(p)
  p
}

makePermutation <- function(n, var) {
  var <- as.integer(var)
  c(integerRange(1L, var-1L), integerRange(var+1L, n), var)
}

Columns <- function(M) {
  apply(M, 2L, identity, simplify = FALSE)
}

isPlusInfinity <- function(x) {
  is.infinite(x) && x > 0
}

isMinusInfinity <- function(x) {
  is.infinite(x) && x < 0
}
