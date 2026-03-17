
# @note
# TZhan does not think any function from package \CRANpkg{spatstat.univar}
# has the same functionality as [kerndens()]. She could be wrong.

#' @title Kernel Density via \link[stats]{density.default}
#' 
#' @description
#' Kernel density estimates only.
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters of the function \link[stats]{density.default}
#' 
#' @note
#' Do *not* overwrite function `spatstat.explore::density.ppp()` !!
#' 
#' @details
#' The function [kerndens()] finds 
#' the estimated density values,
#' i.e., element `$y` of the function \link[stats]{density.default} return.
#' 
#' @returns 
#' The function [kerndens()] returns
#' a \link[base]{numeric} \link[base]{vector}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name kerndens
#' @export
kerndens <- function(x, ...) UseMethod(generic = 'kerndens')

#' @rdname kerndens
#' @export kerndens.numeric
#' @export
kerndens.numeric <- function(x, ...) density.default(x, ...)$y


#' @rdname kerndens
#' @export kerndens.ppp
#' @export
kerndens.ppp <- function(x, ...) {
  d <- x |>
    density_marks.ppp(...)
  if (!length(d)) return(invisible())
  if (inherits(d, what = 'density')) return(d$y)
  d |> 
    lapply(FUN = \(i) i$y)
}



#' @rdname kerndens
#' @export kerndens.ppplist
#' @export
kerndens.ppplist <- function(x, ...) {
  
  z <- x |>
    lapply(FUN = kerndens.ppp, ...)
  
  nm. <- z |>
    lapply(FUN = names)
  if (!all(duplicated.default(nm.)[-1L])) stop('ppp.objects not having same numeric marks?')
  
  ret <- z |> 
    .mapply(FUN = list, dots = _, MoreArgs = NULL)
  if (!length(ret)) return(invisible())
  names(ret) <- nm.[[1L]]
  return(ret)
  
}



#' @rdname kerndens
#' @importFrom spatstat.geom anylist
#' @export kerndens.anylist
#' @export
kerndens.anylist <- function(x, ...) {
  
  x_num <- x |>
    vapply(FUN = is.vector, mode = 'numeric', FUN.VALUE = NA)
  if (!all(x_num)) return(invisible()) # exception handling
  
  x |> 
    lapply(FUN = kerndens.numeric, ...) |>
    do.call(what = anylist, args = _) |>
    as.vectorlist(mode = 'numeric')
  
}



#' @rdname kerndens
#' @export kerndens.hyperframe
#' @export
kerndens.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  # 'numeric' 'marks' in 'ppp'-`hypercolumns`
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  n_ppp <- sum(hc_ppp)
  if (n_ppp > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  if (n_ppp == 1L) {
    mark. <- hc[[which(hc_ppp)]] |>
      kerndens.ppplist(...)
  } else mark. <- NULL
  
  # 'numeric'-`hypercolumns`
  numlist_ <- hc |>
    lapply(FUN = kerndens.anylist, ...)
  numlist. <- numlist_[lengths(numlist_) > 0L]
  
  z <- c(numlist., mark.)
  names(z) <- names(z) |>
    sprintf(fmt = '%s.kerndens')
  
  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(x), z)
  ))
  
}


