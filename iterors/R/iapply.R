#' Apply a function to each element of an iterator.
#'
#' `i_apply(obj, f)` returns the iteror that applies `f` to
#' each element of the given iterable `obj`. It is an iterator
#' equivalent of `lapply`.
#'
#' @param obj an iterable.
#' @param f a function
#' @param ... Additional arguments will be passed along to `f`
#' @return An iteror.
#' @seealso To apply a function of multiple arguments to multiple
#'   iterators, see [i_map]. To split an array over margins (like
#'   `iterators::i_apply` use [`iteror(obj, by=MARGIN`][iteror.array]
#' @export i_apply
i_apply <- function(obj, f, ...) {
  obj <- iteror(obj, ...)
  nextOr_ <- function(or) f(obj(or = return(or)), ...)
  iteror_internal(nextOr_)
}
