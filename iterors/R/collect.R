collectIteror <- function(x, mode="list", n=as.integer(2^31-1), ...) {
  stop_unused(...)
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("n must be a positive integer of length 1")
  }
  size <- min(64, n)
  a <- vector(mode, length=size)
  i <- 0

  if (mode == "list") {
    wrap <- list
  } else {
    wrap <- identity
  }
  while (i < n) {
    length(a) <- size
    i <- i + 1
    for (i in i:size) {
      a[i] <- wrap(x(or = {length(a) <- i-1; return(a)}))
    }
    size <- min(2 * size, n)
  }
  a
}


#' Collect all (or some given number of) values from an iteror,
#' returning a vector of the given type.
#'
#' @param x an iterable object
#' @param n the maximum number of elements to return.
#' @param mode What mode to use for the output vector.
#' @param ... Unused arguments will throw an error.
#'
#' @return The returned value will be `n` elements long if the
#'   iterator did not stop.
#' @seealso concat take
#' @rdname as.vector.iteror
#' @exportS3Method as.list iteror
as.list.iteror <- function(x, n=as.integer(2^31-1), ...) {
  collectIteror(x, mode="list", n=n, ...)
}

#' @exportS3Method as.double iteror
#' @rdname as.vector.iteror
as.double.iteror <- function(x, n=as.integer(2^31-1), ...) {
  collectIteror(x, mode="numeric", n=n, ...)
}

#' @exportS3Method as.numeric iteror
#' @rdname as.vector.iteror
as.numeric.iteror <- function(x, n=as.integer(2^31-1), ...) {
  collectIteror(x, mode="numeric", n=n, ...)
}

#' @exportS3Method as.logical iteror
#' @rdname as.vector.iteror
as.logical.iteror <- function(x, n=as.integer(2^31-1), ...) {
  collectIteror(x, mode="logical", n=n, ...)
}

#' @exportS3Method as.character iteror
#' @rdname as.vector.iteror
as.character.iteror <- function(x, n=as.integer(2^31-1), ...) {
  collectIteror(x, mode="character", n=n, ...)
}

#' @exportS3Method as.vector iteror
#' @rdname as.vector.iteror
as.vector.iteror <- function(x, mode) {
  collectIteror(x, mode)
}

#' Return the first n elements of an iterable object in a vector.
#'
#' Returns the first \code{n} elements of an iterable \code{object} as a list.
#' If \code{n} is larger than the number of elements in \code{object}, the
#' entire iterator is consumed.
#'
#' @param obj An iterable object.
#' @param n The maximum number of elements to extract from the iteror.
#' @param mode The mode of vector to return.
#' @param ... Further arguments may be passed along to the [iteror] constructor.
#' @seealso concat as.vector.iteror
#' @return a list of the first \code{n} items of the iterable
#'   \code{obj}
#' @details A function `take` first appeared in package `itertools2`.
#'   It is basically an alias for [as.list] but defaults to n=1.
#' @seealso as.vector.iteror
#' @examples
#' take(1:10, 3) # 1 2 3
#' take(icount(), 10) # 1:10
#' take(icount(5), 10) # 1 2 3 4 5
#' @export
take <- function(obj, n=1, mode="list", ...) UseMethod("take")

#' @rdname take
#' @exportS3Method
take.default <- function(obj, n=1, mode="list", ...) {
  obj <- iteror(obj, ...)
  take.iteror(obj, n=n, mode=mode)
}

#' @rdname take
#' @exportS3Method
take.iteror <- function(obj, n=1, mode="list", ...) {
  collectIteror(obj, n=n, mode=mode, ...)
}
