

#' @title A Batch Process of \link[base]{attr}
#' 
#' @param x see **Usage**
#' 
#' @param which,exact parameters of the function \link[base]{attr}
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @name attr_
#' @keywords internal
#' @export
attr_ <- function(x, which, exact = TRUE) UseMethod(generic = 'attr_')



#' @rdname attr_
#' @export attr_.anylist
#' @export
attr_.anylist <- function(x, which, exact = TRUE) {
  z <- x |>
    anylapply(FUN = attr, which = which, exact = exact)
  nz <- lengths(z, use.names = FALSE)
  if (all(nz == 0L)) return(invisible()) # exception
  if (any(nz == 0L)) stop('do not allow!!')
  return(z)
}


#' @rdname attr_
#' @importFrom spatstat.geom hyperframe
#' @export attr_.hyperframe
#' @export
attr_.hyperframe <- function(x, which, exact = TRUE) {
  
  # only hypercolumn (not column!) could have attr-per-element!!
  
  hc <- unclass(x)$hypercolumns
  names(hc) <- paste(names(hc), which, sep = '.')
  
  z0 <- hc |>
    lapply(FUN = attr_.anylist, which = which, exact = exact)
  id <- (lengths(z0, use.names = FALSE) > 0L)
  if (!any(id)) return(x) # exception handling
  z <- z0[id] |>
    lapply(FUN = \(i) {
      n <- lengths(i, use.names = FALSE)
      if (all(n == 1L)) return(unlist(i, use.names = FALSE)) # column, not hypercolumns
      return(i)
    }) |>
    do.call(what = hyperframe, args = _)
  
  return(cbind( # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    x, 
    z
  ))
  
}
