#' Wrap an iteror to appear as a Python iterator or vice versa.
#'
#' This requires the `reticulate` package to be installed.
#' @param x An iterable object.
#' @param ... Passed along to [`iteror(x, ...)`][iteror].
#' @param convert does nothing.
#' @return `r_to_py(it)` returns a Python iterator.
#' @exportS3Method reticulate::r_to_py iteror
#' @examples
#' pit <- reticulate::r_to_py(iseq(2, 11, 5))
#' reticulate::iter_next(pit, NULL)
#' reticulate::iter_next(pit, NULL)
#' reticulate::iter_next(pit, NULL)
#'
#' # create an R iterator and ask Python to sum it
#' triangulars <- icount() |> i_accum() |> i_limit(10)
#' builtins <- reticulate::import_builtins()
#' builtins$sum(triangulars) # r_to_py is called automatically
r_to_py.iteror <- function(x, convert=FALSE, ...) {
  x <- iteror(x, ...)
  sentinel <- reticulate::r_to_py(function() NULL)
  reticulate::py_iterator(function() x(sentinel), sentinel)
}

#' @rdname r_to_py.iteror
#' @param obj A Python object (as viewed by package `reticulate`.)
#' @return Method `iteror.python.builtin.object` returns an [iteror].
#' @exportS3Method iteror python.builtin.object
#' @examples
#'
#' # create a generator in Python and sum it in R
#' pit <- reticulate::py_eval("(n for n in range(1, 25) if n % 3 == 0)")
#' sum(iteror(pit))
iteror.python.builtin.object <- function(obj, ...) {
  stop_unused(...)
  obj <- reticulate::as_iterator(obj)
  sentinel <- function() NULL

  nextOr_ <- function(or) {
    val <- reticulate::iter_next(obj, sentinel)
    if (identical(val, sentinel)) or else val
  }

  iteror_internal(nextOr_)
}
