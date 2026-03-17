

#' @title [is.vectorlist]
#' 
#' @param x an \link[spatstat.geom]{anylist}
#' 
#' @param mode \link[base]{character} scalar other than `'any'`, `'complex'` and '`raw`',
#' see function \link[base]{is.vector}
#' 
#' @keywords internal
#' @export
is.vectorlist <- function(
    x, 
    mode = c('any', 'logical', 'integer', 'numeric', 'double', 'character')
) {
  
  mode <- match.arg(mode)
  
  if (!inherits(x, what = 'anylist')) return(FALSE)

  id <- x |>
    vapply(FUN = is.vector, mode = mode, FUN.VALUE = NA)
  if (any(!id)) return(FALSE)
  
  id <- x |> 
    lengths(use.names = FALSE) |>
    duplicated.default()
  if (!all(id[-1L])) return(FALSE)
    
  id <- x |>
    #lapply(FUN = names) |> # NULL-name compatible
    lapply(FUN = attributes) |>
    duplicated.default()
  if (!all(id[-1L])) return(FALSE)
  
  return(TRUE)
  
}


#' @title [as.vectorlist]
#' 
#' @param x an \link[spatstat.geom]{anylist}
#' 
#' @param ... additional parameters of the function [is.vectorlist()]
#' 
#' @keywords internal
#' @importFrom spatstat.geom anylist
#' @export
as.vectorlist <- function(x, ...) {
  
  x <- x |> 
    do.call(what = anylist, args = _)
  
  if (!is.vectorlist(x, ...)) stop('input does not qualify as a `vectorlist`')
  
  attr(x, which = 'mode') <- mode(x[[1L]])
  class(x) <- c('vectorlist', 'anylist', 'listof', class(x)) |> 
    unique.default()
  return(x)
  
}


#' @title Print a `'vectorlist'`
#' 
#' @param x a `'vectorlist'`
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export print.vectorlist
#' @export
print.vectorlist <- function(x, ...) {
  
  x |>
    length() |>
    col_red () |> style_bold() |>
    sprintf(fmt = 'A \'vectorlist\' of %s vectors') |>
    cat('\n')
  
  nm <- x |>
    names()
  if (!all(nm == seq_along(x))) {
    nm |>
      col_cyan() |> style_bold() |>
      paste(collapse = ', ') |>
      sprintf(fmt = 'Name(s): %s') |>
      cat('\n')
  }
  
  x |> 
    attr(which = 'mode', exact = TRUE) |>
    col_blue() |> style_bold() |>
    sprintf(fmt = 'Storage Mode: %s') |>
    cat('\n')
  
  x[[1L]] |> 
    length() |>
    col_magenta() |> style_bold() |>
    sprintf(fmt = 'Individual Vector Length: %s') |>
    cat('\n')
  
  suffix. <- x |>
    attr(which = 'suffix', exact = TRUE) 
  if (length(suffix.)) {
    suffix. |>
      col_yellow() |> style_bold() |>
      sprintf(fmt = 'Suffix: %s') |>
      cat('\n')
  }
  
  
}



#' @title Transpose a `'vectorlist'`
#' 
#' @param x a `'vectorlist'`
#' 
#' @details
#' tzh defines a derived class `'vectorlist'`,
#' i.e., a \link[stats]{listof} \link[base]{vector}s,
#' which \link[base]{inherits} from 
#' \link[spatstat.geom]{anylist}. 
#' The implementation of `'vectorlist'` is 
#' inspired by class \link[spatstat.geom]{solist}.
#' 
#' The `S3` method dispatch [t.vectorlist()], 
#' of the generic function \link[base]{t},
#' transposes a `'vectorlist'` of equi-\link[base]{length}.
#' We illustrate this concept using data set 
#' \link[spatstat.data]{Kovesi} in **Examples**.
#' 
#' @note
#' The motivation of 
#' the derived class `'vectorlist'` and 
#' the method dispatch [t.vectorlist()] 
#' is that 
#' The function \link[spatstat.geom]{with.hyperframe}
#' could be slow in a batch process.
#' 
#' @returns
#' The `S3` method dispatch [t.vectorlist()] returns
#' a `'vectorlist'` of equi-\link[base]{length}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom anylist
#' @export t.vectorlist
#' @export
t.vectorlist <- function(x) {
  
  # upstream definition to ensure this
  # if (!all(vapply(x, FUN = is.vector, FUN.VALUE = NA))) stop('each element of `x` must be `vector`')
  
  nx <- lengths(x, use.names = FALSE)
  if (!all(duplicated(nx)[-1L])) stop('each element of `x` must be of same `length`')
  
  nm <- lapply(x, FUN = names)
  if (!all(duplicated(nm)[-1L])) stop('each element of `x` must have the same names, or no name')
  
  .clist <- \(x) {
    # convert columns of 'matrix' to a 'list'
    x |>
      ncol() |>
      seq_len() |>
      lapply(FUN = \(i) x[, i, drop = TRUE]) |>
      setNames(nm = colnames(x)) # colnames-NULL compatible
  }
  
  x |> 
    do.call(what = rbind, args = _) |>
    .clist() |>
    do.call(what = anylist, args = _) |>
    as.vectorlist()

}
