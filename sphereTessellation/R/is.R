isAtomicVector <- function(x) {
  is.atomic(x) && is.vector(x)
}

isBoolean <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

isString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

isStringVector <- function(x) {
  is.character(x) && !anyNA(x)
}

isFilename <- function(x) {
  isString(x) && file.exists(x)
}

isNumber <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

isPositiveNumber <- function(x) {
  isNumber(x) && x > 0
}

isNonNegativeNumber <- function(x) {
  isNumber(x) && x >= 0
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isStrictPositiveInteger <- function(x) {
  isPositiveInteger(x) && x > 0
}

isVector3 <- function(x) {
  is.numeric(x) && length(x) == 3L && !anyNA(x)
}
