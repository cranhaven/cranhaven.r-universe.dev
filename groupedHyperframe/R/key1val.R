
#' @title Black-Solid-Curve in \link[spatstat.explore]{plot.fv}
#' 
#' @description
#' Name and value of the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' i.e., the primary outcome of an \link[spatstat.explore]{fv.object}. 
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' The function [keyval.fv()] finds the value of the (primary) outcome
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' The function [keyval.fv()] returns a \link[base]{numeric} \link[base]{vector}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name keyval
#' @export
keyval <- function(x, ...) UseMethod(generic = 'keyval')

#' @rdname keyval
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @importFrom spatstat.explore fvnames
#' @export keyval.fv
#' @export
keyval.fv <- function(
    x, 
    key = fvnames(x, a = '.y'),
    .x = fvnames(x, a = '.x'),
    ...
) {
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  ret <- x[[key]] # no need to drop additional attributes (I think..)
  names(ret) <- x[[.x]] # additional attributes (since \pkg{spatstat.explore} v3.5.3.9) kept
  return(ret)
}
# read ?spatstat.explore::eval.fv more carefully!!



#' @rdname keyval
#' 
#' @importFrom spatstat.geom anylapply
#' @export keyval.fvlist
#' @export
keyval.fvlist <- function(x, key = attr(x., which = '.y', exact = TRUE), ...) {
  
  x. <- x |>
    is.fvlist()
  
  force(key)
  .x <- x. |>
    attr(which = '.x', exact = TRUE)
  
  out <- x |> 
    anylapply(FUN = \(i) keyval.fv(i, key = key, .x = .x, ...)) |>
    as.vectorlist(mode = 'numeric')
  attr(out, which = 'key') <- if (missing(key)) {
    'y' # for [keyval.hyperframe]
  } else key # for [keyval.hyperframe]
  return(out)
  
}



#' @rdname keyval
#' @importFrom spatstat.geom names.hyperframe as.list.hyperframe
#' @export keyval.hyperframe
#' @export
keyval.hyperframe <- function(
    x, 
    ...
) {
  
  if (!any(id <- (unclass(x)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(x)[id]
  
  ret0 <- (as.list.hyperframe(x)[nm]) |>
    lapply(FUN = keyval.fvlist, ...)
  
  key <- ret0 |> 
    vapply(FUN = attr, which = 'key', exact = TRUE, FUN.VALUE = '')
  
  names(ret0) <- paste(names(ret0), key, sep = '.')

  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), ret0)
  ))
  
}









