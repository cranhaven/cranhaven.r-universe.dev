isBoolean <- function(x){
  is.atomic(x) && is.logical(x) && length(x) == 1L && !is.na(x)
}

isNumber <- function(x){
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

isPositiveNumber <- function(x){
  isNumber(x) && x > 0
}

isAtomicVector <- function(x){
  is.atomic(x) && is.vector(x)
}

isPoint <- function(A){
  isAtomicVector(A) && is.numeric(A) && length(A) >= 2L && !anyNA(A)
}

is3dPoint <- function(A){
  isAtomicVector(A) && is.numeric(A) && length(A) == 3L && !anyNA(A)
}

is2dPoint <- function(A){
  isAtomicVector(A) && is.numeric(A) && length(A) == 2L && !anyNA(A)
}

isPositiveInteger <- function(m){
  isPositiveNumber(m) && floor(m) == m
}

areDistinct <- function(A, B){
  !isTRUE(all.equal(A, B))
}

dotprod <- function(x, y = NULL){
  c(crossprod(x, y))
}

distance <- function(A, B){
  sqrt(c(crossprod(B - A)))
}
