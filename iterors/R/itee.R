#' Create multiple iterators from one source
#'
#' `i_tee(obj, n)` consumes and buffers the output of a single iterator
#' `obj` so that it can be read by `n` independent sub-iterators.
#'
#' It works by saving the output of source `obj` in a queue, while
#' each sub-iterator has a "read pointer" indexing into the
#' queue. Items are dropped from the queue after all sub-iterators
#' have seen them.
#'
#' This means that if one sub-iterator falls far behind the others, or
#' equivalently if one sub-iterator reads far ahead its cohort the
#' others, the intervening values will be kept in memory. The `max`
#' argument gives a limit on how many items will be held. If this
#' limit is exceeded due to one sub-iterator reading far ahead of the
#' others, an error will be thrown when that sub-iterator attempts to
#' read a new value.
#'
#' @export
#' @param obj an iterable object
#' @param n the number of iterators to return
#' @param max The maximum number of values to buffer.
#' @param ... passed along to `iteror(obj, ...)`
#' @return a list of \code{n} iterators.
#' @author Peter Meilstrup
i_tee <- function(obj, n, max=2^16-1, ...) {
  obj <- iteror(obj, ...)

  n <- as.integer(n)
  if (length(n) != 1) {
    stop("'n' must be an integer value of length 1")
  } else if (n < 1) {
    stop("'n' must be a positive integer")
  }

  data <- deque()
  pointers <- rep(1L, n)

  make_subiteror <- function(id) {
    force(id)
    close <- function() {
      pointers[[id]] <- NA_integer_
    }
    iteror_internal(function(or) {
      len <- data$length()
      ptr <- pointers[[id]]
      if (len < ptr) {
        if (len >= max) {
          stop("i_tee: queue is full")
        }
        val <- obj(or = return(or))
        data$append(val)
      } else {
        val <- data$peek(ptr)
      }
      pointers[[id]] <<- ptr + 1L
      while (all(pointers > 1L, na.rm=TRUE)) {
        data$getFirst()
        pointers <<- pointers - 1L
      }
      val
    })
  }

  lapply(seq_along(pointers), make_subiteror)
}
