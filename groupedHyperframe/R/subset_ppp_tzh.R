
# .Defunct(msg = 'no use. spatstat.geom::split.ppp still dispatch to spatstat.geom::`[.ppp`')

# .Defunct(msg = 'Adrian refuses to change spatstat.geom::`[.ppp` :(')

#' @title `[.ppp_tzh`
#' 
#' @param x an object of internal class `'ppp_tzh'`, 
#' a derived class from \link[spatstat.geom]{ppp.object}
#' 
#' @param i,j see function \link[spatstat.geom]{[.ppp}
#' 
#' @param drop ..
#' 
#' @param ... ..
#' 
#' @param clip ..
#' 
#' @returns
#' The function `` `[.ppp_tzh` `` returns a \link[spatstat.geom]{ppp.object},
#' **not** an object of class `'ppp_tzh'`.
#' 
#' @keywords internal
#' @importFrom spatstat.geom ppp verifyclass intersect.owin inside.owin unitname unitname<- solutionset marksubset markformat marks<- is.vanilla
#' @export [.ppp_tzh
#' @export
`[.ppp_tzh` <- function (x, i, j, drop = FALSE, ..., clip = FALSE) {
  verifyclass(x, "ppp")
  if (!missing(i)) {
    if (inherits(i, "owin")) {
      window <- i
      if (clip) 
        window <- intersect.owin(window, x$window)
      if (is.vanilla(unitname(window))) 
        unitname(window) <- unitname(x)
      ok <- inside.owin(x$x, x$y, window)
      x <- ppp(x$x[ok], x$y[ok], window = window, marks = marksubset(x$marks, ok), check = FALSE, drop = FALSE) # tzh added `drop = FALSE`
    }
    else if (inherits(i, "im")) {
      if (i$type != "logical") 
        stop(paste("Subset operator X[i] undefined", 
                   "when i is a pixel image", "unless it has logical values"), 
             call. = FALSE)
      e <- sys.frame(sys.nframe())
      window <- solutionset(i, e)
      if (clip) 
        window <- intersect.owin(window, x$window)
      ok <- inside.owin(x$x, x$y, window)
      x <- ppp(x$x[ok], x$y[ok], window = window, marks = marksubset(x$marks, ok), check = FALSE, drop = FALSE) # tzh added `drop = FALSE`
    }
    else {
      nx <- x$n
      if (nx == 0) 
        return(x)
      subset <- seq_len(nx)[i]
      if (anyNA(subset)) 
        stop("Index out of bounds in [.ppp", call. = FALSE)
      x <- ppp(x$x[subset], x$y[subset], window = x$window, 
               marks = marksubset(x$marks, subset), check = FALSE, drop = FALSE) # tzh added `drop = FALSE`
    }
  }
  if (!missing(j)) 
    x <- x[j]
  if (drop) {
    mx <- x$marks
    switch(markformat(mx), none = {
    }, vector = {
      if (is.factor(mx)) marks(x) <- factor(mx)
    }, dataframe = {
      if (is.data.frame(mx)) {
        ml <- as.list(mx)
        isfac <- sapply(ml, is.factor)
        if (any(isfac)) mx[, isfac] <- as.data.frame(lapply(ml[isfac], factor))
      }
    }, hyperframe = {
    })
  }
  return(x)
}




if (FALSE) {
  library(spatstat.geom)
  (x = ppp(
    x = runif(6), y = runif(6), 
    window = square(),
    marks = data.frame(protein = runif(6)),
    drop = FALSE
  ))
  
  x |> markformat() # 'dataframe'
  
  # invokes spatstat.geom::`[.ppp`
  (x[1:3, , drop = FALSE]) |> markformat() # 'vector'; not 'data.frame'!!
  
  # tzh edits
  (`[.ppp_tzh`(x = x, i = 1:3, drop = FALSE)) |>
    markformat() # 'dataframe' :) 
  
  
  
}
