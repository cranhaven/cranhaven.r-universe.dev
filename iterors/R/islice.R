#' Iteror that returns selected elements from an iterable.
#'
#' Constructs an iteror that returns elements from an iterable following the
#' given sequence with starting value \code{start} and ending value \code{end}.
#' The sequence's step size is given by \code{step}.
#'
#' The iterable given in \code{object} is traversed beginning with element
#' having index specified in \code{start}. If \code{start} is greater than 1,
#' then elements from the \code{object} are skipped until \code{start} is
#' reached. By default, elements are returned consecutively. However, if the
#' \code{step} size is greater than 1, elements in \code{object} are skipped.
#'
#' If \code{stop} is \code{Inf} (default), the iteration continues until the
#' iteror is exhausted unless \code{end} is specified. In this case,
#' \code{end} specifies the sequence position to stop iteration.
#'
#' @export
#' @param object iterable object through which this function iterates
#' @param start the index of the first element to return from \code{object}
#' @param end the index of the last element to return from \code{object}
#' @param step the step size of the sequence
#' @param ... passed along to `iteror(object, ...)`
#' @return iteror that returns \code{object} in sequence
#' @details Originally from package `itertools2`.
#'
#' @examples
#' it <- i_slice(1:5, start=2)
#' nextOr(it, NULL) # 2
#' nextOr(it, NULL) # 3
#' nextOr(it, NULL) # 4
#' nextOr(it, NULL) # 5
#'
#' it2 <- i_slice(1:10, start=2, end=5)
#' unlist(as.list(it2)) == 2:5
#'
#' it3 <- i_slice(1:10, start=2, end=9, step=2)
#' unlist(as.list(it3)) == c(2, 4, 6, 8)
i_slice <- function(object, start=1, end=NULL, step=1, ...) {
  start <- as.integer(start)
  step <- as.integer(step)
  object <- iteror(object, ...)

  if (length(start) != 1 || start < 1) {
    stop("'start' must be positive integer of length 1")
  }
  if (length(step) != 1 || step < 1) {
    stop("'step' must be a positive integer of length 1")
  }
  if (!is.null(end)) {
    end <- as.integer(end)
    if (length(end) != 1) {
      stop("'end' must be a numeric value of length 1")
    }
  } else {
    end <- Inf
  }

  at <- start
  i <- 0

  nextOr_ <- function(or) {
    repeat {
      if (i >= end) {
        return(or)
      }
      val <- object(or = return(or))
      i <<- i + 1
      if (i == at) {
        at <<- at + step
        return(val)
      }
    }
  }

  iteror(nextOr_)
}
