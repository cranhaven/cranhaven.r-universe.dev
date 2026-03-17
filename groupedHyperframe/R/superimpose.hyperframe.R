

#' @title superimpose.hyperframe
#' 
#' @param ... one or more \link[spatstat.geom]{hyperframe}s or [groupedHyperframe]s
#' 
#' @returns
#' The function [superimpose.hyperframe()] returns a 
#' \link[spatstat.geom]{hyperframe} or [groupedHyperframe].
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom superimpose dim.hyperframe
#' @method superimpose hyperframe
#' @export superimpose.hyperframe
#' @export
superimpose.hyperframe <- function(...) {
  
  dots <- list(...)
  if (!length(dots)) return(invisible())
  if (length(dots) == 1L) return(dots[[1L]]) 
  
  z <- dots |>
    vapply(FUN = inherits, what = 'hyperframe', FUN.VALUE = NA) |>
    all()
  if (!z) stop('all input must be hyperframe')
  
  z <- dots |>
    lapply(FUN = dim.hyperframe)
  if (!all(duplicated.default(z)[-1L])) stop('all input must have same dimensions')
  
  d0 <- dots |>
    lapply(FUN = unclass)
  
  z <- d0 |> 
    lapply(FUN = `[[`, 'df')
  if (!all(duplicated(z)[-1L])) stop('all input must have same column(s)')
  
  hc <- d0 |>
    lapply(FUN = `[[`, 'hypercolumns')
  z <- hc |>
    lapply(FUN = names)
  if (!all(duplicated(z)[-1L])) stop('all input must have same names of hypercolumn(s)')
  
  hc_class <- hc |> 
    lapply(FUN = \(i) {
      i |> lapply(FUN = class)
    })
  if (!all(duplicated(hc_class)[-1L])) stop('all input must have same classes of hypercolumn(s)')
  
  hc_out <- hc |> 
    c(list(
      .class = hc_class[[1L]],
      FUN = \(..., .class) {
        out <- mapply(FUN = superimpose, ..., SIMPLIFY = FALSE)
        # use S3 generic ?spatstat.geom::superimpose !! super smart!!
        # tzh does not know what ?spatstat.geom::superimpose.ppp does - see if (FALSE) at the bottom of this file
        class(out) <- .class
        return(out)
      },
      SIMPLIFY = FALSE
    )) |> 
    do.call(what = mapply, args = _)

  # identical(hc_out, unclass(fluM)$hypercolumns) # no, tzh does not care why :)
  # stopifnot(identical(hc_out$pattern, fluM$pattern)) # :))
  
  ret0 <- d0[[1L]]
  # tzh does not understand `$hyperatoms` yet..
  ret0$hypercolumns <- hc_out
  class(ret0) <- class(dots[[1L]])
  
  z <- dots |>
    vapply(FUN = inherits, what = 'groupedHyperframe', FUN.VALUE = NA)
  if (!any(z)) return(ret0)
  
  if (all(z)) {
    z <- dots |> 
      lapply(FUN = attr, which = 'group')
    if (!all(duplicated(z)[-1L])) stop('all input must have same grouping structure')
    return(ret0)
  }
  
  stop('input must all-be, or all-not-be, `groupedHyperframe`s')

}




