#' Concatenate contents of multiple iterators into a vector.
#'
#' `concat` collects all values from an iterable object, and pastes
#' them end to end into one vector.  In other words `concat` is to
#' `as.list.iteror` as `c` is to `list`.
#'
#' @param obj An iteror.
#' @param mode The mode of vector to return.
#' @param n The maximum number of times to call `nextOr(obj)`.
#' @param length.out The target size of the output vector (after
#'   results have been pasted together). If the iteror ends (or emits
#'   `n` results) before emitting this many elements, the result will be
#'   shorter than `length.out`. If the iterator does not end early, the output
#'   will have at least `length.out` elements, and possibly more,
#'   as the entire last chunk will be included.
#' @param ... passed along to [iteror] constructor.
#' @return a vector with mode `mode`.
#' @examples
#'
#' it <- i_apply(icount(), seq_len) # [1], [1, 2], [1, 2, 3], ...
#' concat(it, n=4, mode="numeric")  # [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
#' concat(it, length.out=4, mode="numeric")  # [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
#' @rdname concat
#' @export
concat <- function(obj, mode = "list", n = Inf, ...) UseMethod("concat")

#' @rdname concat
#' @exportS3Method
concat.default <- function(obj, mode = "list", n = as.integer(2^31-1), ...) {
  concat.iteror(iteror(obj, ...), mode=mode, n = n)
}

#' @rdname concat
#' @exportS3Method
concat.iteror <- function(obj, mode = "list", n = Inf,
                          length.out = Inf, ...) {
  if (length(length.out) != 1 || length.out < 0) {
    stop("length.out must be a positive number of length 1")
  }
  if (length(n) != 1 || n < 0) {
    stop("n must be a positive number of length 1")
  }
  stop_unused(...)

  size <- min(64, n)
  a <- vector(mode, length=size)
  i <- 0
  nc <- 0
  while (i < length.out && nc < n) {
    chunk <- obj(or = break)
    nc <- nc + 1
    l <- length(chunk)
    to <- i + l
    if (to > size) {
      size <- min(2 * size, to)
      length(a) <- size
    }
    a[i + seq_len(l)] <- chunk
    i <- i + l
  }
  length(a) <- i
  a

}
