

#' @title \link[stats]{aggregate} the \link[spatstat.geom]{marks} of \link[spatstat.geom]{ppp}-Related Object
#' 
#' @description
#' To \link[stats]{aggregate} the \link[spatstat.geom]{marks} of a \link[spatstat.geom]{ppp}-related object, e.g.,
#' \itemize{
#' \item{\link[spatstat.geom]{ppp.object}}
#' \item{a `ppplist`}
#' \item{the \link[spatstat.geom]{ppp}-hypercolumn of a \link[spatstat.geom]{hyperframe}}
#' }
#' 
#' 
#' @param x see **Usage**
#' 
#' @param by,FUN,... additional parameters of the function \link[stats]{aggregate.data.frame}
#' 
#' @param expr \link[base]{expression}, only used when `markformat.ppp(x)` is `'dataframe'` 
#' **and** the parameter `by` is missing 
#' 
#' @param vectorize \link[base]{logical} scalar (default value `FALSE`), whether to convert the return from 
#' The function \link[stats]{aggregate.data.frame} into a \link[base]{vector}
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name aggregate_marks
#' @export
aggregate_marks <- function(x, by, FUN, expr, ..., vectorize = FALSE) UseMethod(generic = 'aggregate_marks')
  

#' @rdname aggregate_marks
#' @importFrom spatstat.geom marks.ppp
#' @export aggregate_marks.ppp
#' @export
aggregate_marks.ppp <- function(x, by, FUN, expr, ..., vectorize = FALSE) {
  
  fun_ <- substitute(FUN)
  
  mf <- x |>
    markformat.ppp()
  
  if (mf == 'none') return(invisible())
  
  if (mf == 'vector') {
    if (!missing(by)) warning('parameter `by` is ignored, for vector-markformat !')
    z <- x |> 
      marks.ppp() |> 
      FUN() |>
      c() # convert to vector!!!!
    if ((length(z) == 1L) && is.symbol(fun_)) {
      names(z) <- deparse1(fun_)
      # this is difficult to do with `by` ..
    }
    return(z)
  }
  
  # rest: mf == 'dataframe'
  
  mks <- x |> 
    marks.ppp(dfok = TRUE, drop = FALSE)
  
  if (missing(by)) {
    
    z <- substitute(expr) |>
      eval(envir = mks) # inside ?base::with.default
    
    if (!vectorize) return(z)
    
    # if (vectorize):
    return(unlist(z, recursive = TRUE, use.names = TRUE))
    
  }
  
  # rest: (mf == 'dataframe') && !missing(by)
  
  z <- mks |>
    aggregate.data.frame(x = _, by = by, FUN = FUN, ...) # parameter `simplify` must be TRUE
  
  if (!vectorize) return(z)
  
  # if (vectorize):   
  if (!inherits(by, what = 'formula')) stop('`by` must be formula')
  
  # stupid but working :)
  tmp <- z |>
    split.data.frame(f = by[c(1L, 3L)]) # ?base::split.data.frame takes care of group-name !!!
  
  tmp |>
    lapply(FUN = \(d) { # d = tmp[[1L]] #  `d` must be nrow-1 'data.frame'
      out0 <- d[all.vars(by[[2L]])]
      cls <- out0 |>
        lapply(FUN = class) |>
        unique.default()
      if (length(cls) != 1L) stop('shouldnt happen')
      out <- switch(cls[[1L]][[1L]], 'matrix' = {
        out0 |> # this is a 'data.frame' with one-'matrix'-column!!
          lapply(FUN = as.data.frame.matrix) |>
          unlist(recursive = TRUE, use.names = TRUE) # numeric
      }, {
        # atomic vector
        out0
      })
      return(out)
    }) |>
    unlist(use.names = TRUE)

}


#' @rdname aggregate_marks
#' @importFrom spatstat.geom anylapply
#' @export aggregate_marks.ppplist
#' @export
aggregate_marks.ppplist <- function(x, ...) {
  
  z <- x |>
    anylapply(FUN = \(x) {
      aggregate_marks.ppp(x, ..., vectorize = TRUE)
    }) 
  
  if (!is.vectorlist(z, mode = 'numeric')) return(z)
  
  z |>
    as.vectorlist(mode = 'numeric')
  
}



#' @rdname aggregate_marks
#' @export aggregate_marks.hyperframe
#' @export
aggregate_marks.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  n_ppp <- sum(hc_ppp)
  if (!n_ppp) return(invisible()) # exception handling
  if (n_ppp > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  
  z <- hc[[which(hc_ppp)]] |>
    aggregate_marks.ppplist(...)
  
  if (!is.vectorlist(z, mode = 'numeric')) stop('must force `vectorlist`')
  
  return(cbind( # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    x, 
    markstats = z
  ))
  
}


