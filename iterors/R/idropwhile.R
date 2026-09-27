#' Iterator that drops elements until the predicate function returns FALSE
#'
#' Constructs an iterator that drops elements from the iterable \code{object} as
#' long as the \code{predicate} function is true; afterwards, every element of
#' \code{iterable} object is returned.
#'
#' Because the iterator does not return any elements until the \code{predicate}
#' first becomes false, there may have a lengthy start-up time before elements
#' are returned.
#'
#' @export
#' @param object an iterable object
#' @param predicate a function that determines whether an element is \code{TRUE}
#' or \code{FALSE}. The function is assumed to take only one argument.
#' @param ... Further arguments forwarded to [iteror].
#' @return An [iteror] object.
#'
#' @examples
#' # Filters out numbers exceeding 3
#' not_too_large <- function(x) {
#'   x <= 3
#' }
#' it <- i_dropwhile(1:8, not_too_large)
#' as.list(it)
#'
#' # Same approach but uses an anonymous function
#' it2 <- i_dropwhile(seq(2, 20, by=2), function(x) x <= 10)
#' as.list(it2)
i_dropwhile <- function(object, predicate, ...) {
  iter_obj <- iteror(object, ...)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- nextOr(iter_obj, return(or))
      if (!predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror_internal(nextOr_)
}
