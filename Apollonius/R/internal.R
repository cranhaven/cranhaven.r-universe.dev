isBoolean <- function(x) {
  is.atomic(x) && is.logical(x) && length(x) == 1L && !is.na(x)
}

isString <- function(x) {
  is.atomic(x) && is.character(x) && length(x) == 1L && !is.na(x)
}

isNumber <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

isPositiveNumber <- function(x) {
  isNumber(x) && x > 0
}


segment <- function(A, B, n) {
  t_ <- seq(0, 1, length.out = n)
  t(vapply(t_, function(t) {A + t*(B-A)}, numeric(2L)))
}

ray <- function(O, A, n, tmax, OtoA) {
  stopifnot(isBoolean(OtoA))
  if(OtoA) {
    t_ <- seq(0, tmax, length.out = n)
  } else {
    t_ <- seq(-tmax, 0, length.out = n)
  }
  t(vapply(t_, function(t) {O + t*(A-O)}, numeric(2L)))
}
