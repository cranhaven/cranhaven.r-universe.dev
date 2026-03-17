



#' @export
`[.add_` <- function(x, i) {
  ret <- unclass(x)[i]
  class(ret) <- class(x) # otherwise class info dropped
  return(ret)
}






#' @title [sort_by.add_]
#' 
#' @param x an object returned from functions 
#' [add_dummy_partition()], [add_dummy()] or [add_numeric()]
#' 
#' @param y \link[base]{language}, see function \link[base]{sort_by}
#' 
#' @param ... additional parameters of S3 generic \link[base]{sort_by}, etc.
#' 
#' @details
#' The `S3` method [sort_by.add_()] sorts the elements of an `'add_'` object by a certain criterion `y`.
#' We suggest using `y = abc(effsize)` and `decreasing = TRUE` order of the \link[base]{abs}olute values of the effect sizes of additional predictor.  
#'
#' @returns 
#' The `S3` method [sort_by.add_()] returns an object of the same \link[base]{class} as input `x`.
#' 
#' @keywords internal
#' @export sort_by.add_
#' @export
sort_by.add_ <- function(x, y, ...) {
  effsize <- vapply(x, FUN = attr, which = 'effsize', exact = TRUE, FUN.VALUE = NA_real_)
  o <- substitute(y) |> 
    eval() |> 
    order(...) # ?base::order
  x[o]
}



