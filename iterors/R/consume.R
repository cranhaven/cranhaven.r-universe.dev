#' Consumes the first n elements of an iterator
#'
#' Advances the iterator n-steps ahead without returning anything.
#'
#' @export
#' @param obj an iterable object
#' @param n The number of elements to consume.
#' @param ... passed along to [iteror] constructor.
#' @return `obj`, invisibly.
#' @seealso take collect
#'
#' @examples
#' it <- iteror(1:10)
#' # Skips the first 5 elements
#' consume(it, n=5)
#' # Returns 6
#' nextOr(it, NA)
#'
#' it2 <- iteror(letters)
#' # Skips the first 4 elements
#' consume(it2, 4)
#' # Returns 'e'
#' nextOr(it2, NA)
consume <- function(obj, n=Inf, ...) UseMethod("consume")

#' @exportS3Method
#' @rdname consume
consume.iteror <- function(obj, n=Inf, ...) {
  obj <- iteror(obj, ...)

  if (n <= 0 | !is.numeric(n) | length(n) != 1) {
    stop("n must be a non-negative integer of length 1")
  }

  if (is.finite(n)) {
    while (n > 0) {
      obj(or=break)
      n <- n - 1
    }
  } else repeat {
    obj(or=break)
  }
  invisible(obj)
}

#' Returns the nth item of an iteror
#'
#' Returns the \code{n}th item of an \code{iteror} after advancing the
#' iteror \code{n} steps ahead. If the \code{iteror} is entirely consumed,
#' the argument \code{or} is returned instead. That is, if either \code{n >
#' length(iteror)} or \code{n} is 0, then the \code{iteror} is consumed.
#'
#' @export
#' @param obj an iterable.
#' @param n The index of the desired element to return.
#' @param or If the iteror finishes before retuning `n` elements,
#'           this argument will be forced and returned.
#' @param ... passed along to [iteror] constructor.
#' @return The nth element of the iteror or the result of forcing `or`.
#' @seealso take consume collect
#'
#' @examples
#' it <- iteror(1:10)
#' # Returns 5
#' nth(it, 5, NA)
#'
#' it2 <- iteror(letters)
#' # Returns 'e'
#' nth(it2, 5, NA)
#'
#' it3 <- iteror(letters)
#' # Returns default value of NA
#' nth(it3, 42, NA)
#'
#' it4 <- iteror(letters)
#' # Returns default value of "foo"
#' nth(it4, 42, or="foo")
nth <- function(obj, n, or, ...) UseMethod("nth")

#' @exportS3Method
nth.default <- function(obj, n, or, ...) {
  nth.iteror(iteror(obj, ...), n=n, or=or)
}

#' @exportS3Method
nth.iteror <- function(obj, n, or, ...) {
  if (n <= 0 | !is.numeric(n) | length(n) != 1) {
    stop("n must be a positive integer of length 1")
  }

  stop_unused(...)

  n <- as.integer(n)
  last <- NULL
  while (n > 0) {
    last <- obj(return(or))
    n <- n - 1
  }
  last
}
