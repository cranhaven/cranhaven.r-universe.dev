

#' @title Append to Existing \link[spatstat.geom]{marks}
#' 
#' @description
#' Append an additional `mark` to existing \link[spatstat.geom]{marks}.
#' 
#' @param x currently only \link[spatstat.geom]{ppp.object} is supported
#' 
#' @param value a \link[base]{factor} or \link[base]{numeric} \link[base]{vector}
#' 
#' @returns 
#' The `S3` method dispatch [`append_marks<-.ppp`] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name append_marks_set
#' @export
`append_marks<-` <- function(x, value) UseMethod(generic = 'append_marks<-')
  
  
#' @rdname append_marks_set
#' @importFrom spatstat.geom markformat.ppp npoints.ppp
#' @export append_marks<-.ppp
#' @export
`append_marks<-.ppp` <- function(x, value) {
  
  value. <- substitute(value)

  v <- eval(value., envir = parent.frame()) # let err; `language` `eval`uate correctly.
  if (is.call(v)) {
    npt <- x |> 
      npoints.ppp()
    v <- v |> 
      sub_lang(pattern = quote(.), replacement = npt, lang = _) |>
      eval()
  } # else do nothing

  npt <- npoints.ppp(x)
    
  if (!is.recursive(v)) { # 'list' is also ?base::is.vector !
    
    if (length(v) != npt) stop('length not match')
    
    switch(markformat.ppp(x), none = {
      x$markformat <- 'vector'
      x$marks <- v
      
    }, vector = {
      x$markformat <- 'dataframe'
      x$marks <- data.frame(m1 = x$marks, m2 = v)
      
    }, dataframe = {
      newid <- length(x$marks) + 1L
      x$marks <- data.frame(x$marks, v)
      names(x$marks)[newid] <- paste0('m', newid)
      
    }, stop('incorrect markformat?'))
    
    return(x)
    
  }

  # else if (is.recursive(v));
  if (!all(lengths(v) == npt)) stop('list `v` must have all lengths as `npt`')
  
  for (iv in v) {
    append_marks(x) <- iv # lazy and beautiful!
  }
  
  vnm <- names(v)
  if (length(vnm) && !anyNA(vnm) && all(nzchar(vnm))) {
    nv <- length(v)
    nm <- length(x$marks)
    names(x$marks)[(nm-nv+1):nm] <- vnm
  }
  
  return(x)
  
} 


#' @rdname append_marks_set
#' @export append_marks<-.tess
#' @export
`append_marks<-.tess` <- function(x, value) {
  .Deprecated(msg = 'still working')
}

#' @rdname append_marks_set
#' @export append_marks<-.psp
#' @export
`append_marks<-.psp` <- function(x, value) {
  .Deprecated(msg = 'still working')
}



sub_lang <- \(pattern, replacement, lang) {
  x <- lang |>
    as.list.default()
  id <- vapply(x, FUN = is.call, FUN.VALUE = NA)
  if (any(id)) {
    x[id] <- x[id] |>
      lapply(FUN = \(i) {
        sub_lang(pattern = pattern, replacement = replacement, lang = i)
      })
  }
  x |>
    lapply(FUN = \(i) {
      if (identical(i, pattern)) return(replacement)
      return(i)
    }) |>
    as.call()
}




if (FALSE) {
  spatstat.geom::`marks<-`
  library(spatstat.geom); methods(`marks<-`)
  
  if (FALSE) {
    # what's the difference?!
    spatstat.geom::append.psp
    spatstat.geom::superimpose.psp
  }
  base::append # not S3 generic
}

