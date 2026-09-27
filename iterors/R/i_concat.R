#' @rdname i_chain
#' @param obj an iterable.
#' @description `i_concat(obj)` takes an iterable that returns
#'   iterables, and chains together all inner values of iterables into
#'   one iterator. Analogous to `unlist(recursive=FALSE)`.
#' @export
i_concat <- function(obj, ...) {
  obj <- iteror(obj, ...)
  current <- NULL

  nextOr_ <- function(or) {
    if (is.null(current)) {
      current <<- iteror(obj(or = return(or)))
    }
    repeat {
      return(current(or = {
        current <<- iteror(obj(or = return(or)))
        next
      }))
    }
  }

  iteror_internal(nextOr_)
}
