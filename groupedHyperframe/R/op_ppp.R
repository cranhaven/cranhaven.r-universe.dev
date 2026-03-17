
#' @title Operations on \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' Create
#' \link[spatstat.explore]{fv.object}s
#' or 
#' distances
#' from a \link[spatstat.geom]{ppp.object}.
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param fun a distance \link[base]{function}, 
#' or a \link[base]{function} that returns an \link[spatstat.explore]{fv.object}, 
#' see **Details**
#' 
#' @param ... additional parameters of the function `fun`
#' 
#' @returns 
#' The functions [ppp_numeric2fv()] and [ppp_multitype2fv()] return a \link[stats]{listof} 
#' \link[spatstat.explore]{fv.object}s.
#' 
#' The function [ppp2dist()] returns a \link[stats]{listof} 
#' \link[base]{double} \link[base]{vector}s.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name ppp2.
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
#' @export
ppp2dist <- function(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- vapply(x., FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  fn_mtp_ <- c(
    nncross = '.nncross'
  )
  fn_mtp <- lapply(fn_mtp_, FUN = get)
  id_mtp <- vapply(fn_mtp, FUN = identical, x = fun, FUN.VALUE = NA)
  
  if (!any(id_mtp)) stop('fun not supported')

  ret <- x.[mtp] |> 
    anylapply(FUN = fun, ...)
  #names(ret) <- paste(names(ret), names(fn_mtp_)[id_mtp], sep = '.')
  
  id <- (lengths(ret) > 0L)
  if (!any(id)) return(invisible())
  
  #return(ret[id])
  z <- ret[id] |>
    as.vectorlist(mode = 'numeric')
  attr(z, which = 'suffix') <- names(fn_mtp_)[id_mtp]
  return(z)
  
}





#' @rdname ppp2.
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
#' @export
ppp_numeric2fv <- function(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  num <- x |>
    is.numeric.ppp()
  if (!any(num)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  ret <- x.[num] |> 
    anylapply(FUN = fun, ...) |>
    as.fvlist()
  return(ret)
  
  if (FALSE) {
  # restore names of `fv`-hypercolumns from the result
  # attr(,'fname') is determined by `fun`
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  names(ret) <- paste(names(ret), fname1, sep = '.')
  
  return(ret)
  } # before 2025-12-05
}






#' @rdname ppp2.
#' @importFrom spatstat.geom unstack.ppp is.multitype.ppp anylapply
#' @export
ppp_multitype2fv <- function(x, fun, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) {
    # unstacking a 'vector' `mark`
    names(x.) <- 'm'
  }
  
  mtp <- x. |> 
    vapply(FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  ret <- x.[mtp] |> 
    anylapply(FUN = fun, ...) |>
    as.fvlist()
  return(ret)
  
  if (FALSE) {
  # restore names of `fv`-hypercolumns from the result
  # attr(,'fname') is determined by `fun`
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  names(ret) <- paste(names(ret), fname1, sep = '.')
  
  return(ret)
  } # before 2025-12-05
}


















