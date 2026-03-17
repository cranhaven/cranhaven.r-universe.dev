

#' @title Inspect a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @description
#' A helper function to check the validity of a \link[stats]{listof} \link[spatstat.explore]{fv.object}s.
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @returns 
#' The function [is.fvlist()] returns a \link[base]{logical} scalar with \link[base]{attributes}
#' \describe{
#' \item{`attr(,'r')`}{\eqn{x}-axis, or the \eqn{r}-values}
#' \item{`attr(,'fname')`}{see explanation of this \link[base]{attributes} in function \link[spatstat.explore]{fv}}
#' \item{`attr(,'.x')`}{`spatstat.explore::fvnames(x, a = '.x')` returns}
#' \item{`attr(,'.y')`}{`spatstat.explore::fvnames(x, a = '.y')` returns}
#' }
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames
#' @export
is.fvlist <- function(X) {
  
  # tzh is aware that
  # ?spatstat.explore::roc.ppp returns an `'roc'` object, inherits from `'fv'`, first argument being `p` instead of `r`!!!
  # in [as.fvlist()] tzh still uses `r`
  # because we have function [.rmax()] ...
  
  id <- X |>
    vapply(FUN = inherits, what = 'fv', FUN.VALUE = NA)
  if (!all(id)) {
    message('not all elements are `fv.object`')
    return(FALSE)
  }
  .x <- X |>
    vapply(FUN = fvnames, a = '.x', FUN.VALUE = '')
  if (!all(duplicated.default(.x)[-1L])) {
    message('`.x` of all fv.objects are not the same')
    return(FALSE)
  }
  
  .y <- X |> 
    vapply(FUN = fvnames, a = '.y', FUN.VALUE = '')
  if (!all(duplicated.default(.y)[-1L])) {
    message('`.y` of all fv.objects are not the same')
    return(FALSE)
  }
  
  fname. <- X |>
    lapply(FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname.)[-1L])) {
    message('`fname` of all fv.objects are not the same')
    return(FALSE)
  }
  
  r. <- X |>
    lapply(FUN = `[[`, .x[1L])
  if (!all(duplicated.default(r.)[-1L])) {
    message('x-axis of all fv.objects are not the same')
    return(FALSE)
  }
  
  ret <- TRUE
  attr(ret, which = 'r') <- r.[[1L]]
  attr(ret, which = '.x') <- .x[[1L]]
  attr(ret, which = '.y') <- .y[[1L]]
  attr(ret, which = 'fname') <- fname.[[1L]]
  return(ret)
  
}


#' @title Convert a \link[stats]{listof} \link[spatstat.explore]{fv.object}s into `'fvlist'`
#' 
#' @param X a \link[stats]{listof} \link[spatstat.explore]{fv.object}s
#' 
#' @param data.name \link[base]{character} scalar, name of `X`, for console message output
#' 
#' @returns 
#' The function [as.fvlist()] returns an \link[base]{invisible} \link[base]{list}.
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames
#' @export
as.fvlist <- function(X, data.name) {
  
  force(X) 
  # hahahaha, must!  otherwise suppressMessage of my [.illegal2theo.fv()]!!!
  
  tmp <- is.fvlist(X) |>
    suppressMessages()
  if (!tmp) return(X) # exception handling
  
  attr(X, which = 'r') <- r <- attr(tmp, which = 'r', exact = TRUE)
  nr <- length(r)
  attr(X, which = '.x') <- attr(tmp, which = '.x', exact = TRUE)
  attr(X, which = '.y') <- .y <- attr(tmp, which = '.y', exact = TRUE)
  attr(X, which = 'fname') <- attr(tmp, which = 'fname', exact = TRUE)
  
  id <- X |> 
    vapply(FUN = \(x) {
      c(x[[.y]]) |> # drop attributes since \pkg{spatstat.explore} v3.5.3.9
        lastLegal()
    }, FUN.VALUE = NA_integer_)
  
  if (any(id < nr)) {
    
    id0 <- id[id != nr]
    tb <- id0 |> table()
    uid <- id0 |> unique.default() |> sort.int()
    loc <- uid |>
      vapply(FUN = \(u) {
        which(id == u) |>
          paste0('L', collapse = ', ') |>
          col_magenta() |> style_bold()
      }, FUN.VALUE = '')
    
    if (!missing(data.name)) {
      paste0(
        'Legal ', 
        'rmax' |> col_magenta() |> style_bold(),
        '(', 
        data.name |> col_blue() |> style_bold(), 
        sprintf(fmt = '), smaller than user input of rmax = %.1f, are\n', max(r)), 
        sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
          paste0('rmax=', r[uid], ' at location ', loc, collapse = '\n')
      ) |>
        message()
    }
    
    rmax <- min(r[uid]) # minimum legal rmax of 'fvlist'
    
  } else rmax <- max(r)
  
  if (rmax == 0) stop("check the `'ppplist'` that created the input `x`")
  attr(X, which = 'rmax') <- rmax
  
  class(X) <- c('fvlist', 'anylist', 'listof', 'list', class(X)) |> 
    unique.default()
  return(X)
  
}




#' @title Print `'fvlist'`
#' 
#' @param x an `'fvlist'`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export print.fvlist
#' @export  
print.fvlist <- function(x, ...) {
  
  fname <- x |> 
    attr(which = 'fname', exact = TRUE)
  .x <- x |>
    attr(which = '.x', exact = TRUE)
  
  ftext0 <- if (length(fname) == 1L) {
    sprintf(fmt = '%s(%s)', fname, .x)
  } else if (length(fname) == 2L) {
    fnm2 <- fname[2L] |> str2lang() |> as.list()
    .subscript <- if (length(fnm2) == 1L) {
      fnm2[[1L]] |> deparse1()
    } else {
      fnm2[-1L] |> 
        vapply(FUN = deparse1, FUN.VALUE = '') |>
        paste(collapse = ',')
    }
    sprintf(fmt = '%s[%s](%s)', fname[1L], .subscript, .x)
  } else stop('not supported!!')
  ftext <- ftext0 |>
    col_blue() |> style_bold()
  
  x |>
    length() |>
    col_red () |> style_bold() |>
    sprintf(fmt = 'An \'fvlist\' of %s fv.objects %s', ftext) |>
    cat('\n')
  
  nm <- x |>
    names() 
  if (length(nm) && !anyNA(nm) && all(nzchar(nm))) {
    nm |>
      col_cyan() |> style_bold() |>
      paste(collapse = ', ') |> # will create `''` from NULL input!!
      sprintf(fmt = 'Name(s): %s') |>
      cat('\n')
  }
  
  available_rmax <- x |>
    attr(which = 'r', exact = TRUE) |>
    max() |>
    col_magenta() |> style_bold()
  
  sprintf(fmt = 'Available %smax: %s', .x, available_rmax) |>
    cat('\n')
  
  x |>
    attr(which = 'rmax', exact = TRUE) |>
    sprintf(fmt = '%.4g') |>
    col_green() |> style_bold() |>
    sprintf(fmt = 'Minimum Legal rmax: %s') |>
    cat('\n')

}


#' @title [as.list.fvlist()]
#' 
#' @description
#' Converts an `fvlist` to a simple \link[base]{list}
#' 
#' @param x `fvlist`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export as.list.fvlist
#' @export
as.list.fvlist <- function(x, ...) {
  class(x) <- 'list'
  attributes(x)[c('r', '.x', '.y', 'fname', 'rmax')] <- NULL
  return(x)
}









