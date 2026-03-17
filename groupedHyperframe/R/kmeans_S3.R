
#' @title Print `'pppkm'` object
#' 
#' @param x a `'pppkm'` object, returned from function [kmeans.ppp()]
#' 
#' @param ... additional parameters, currently no use
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @export print.pppkm
#' @export
print.pppkm <- function(x, ...) {
  
  NextMethod(generic = print) # ?spatstat.geom::print.ppp
  
  x |> 
    attr(which = 'f', exact = TRUE) |>
    table() |>
    c() |>
    paste(collapse = ', ') |>
    col_blue() |> style_bold() |>
    sprintf(fmt = 'with k-means clustering of %s points') |>
    message()
  
}




#' @title Split `'pppkm'` object
#' 
#' @param x a `'pppkm'` object, returned from function [kmeans.ppp()]
#' 
#' @param f \link[base]{factor}, default is `attr(x,'f')`
#' 
#' @param ... additional parameters, currently no use
#' 
#' @returns
#' The function [split.pppkm()] returns a `'splitppp'` object from the
#' workhorse function \link[spatstat.geom]{split.ppp}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom split.ppp
#' @export split.pppkm
#' @export
split.pppkm <- function(x, f = attr(x, which = 'f', exact = TRUE), ...) {
  
  x |> 
    split.ppp(f = f, drop = FALSE)
  
}


#' @title Plot `'pppkm'` object
#' 
#' @param x a `'pppkm'` object, returned from function [kmeans.ppp()]
#' 
#' @param main,...,cols additional parameters of the function \link[spatstat.geom]{plot.ppp}
#' 
#' @keywords internal
#' @importFrom spatstat.geom plot.ppp
#' @importFrom scales pal_hue
#' @export plot.pppkm
#' @export
plot.pppkm <- function(
    x, 
    main = 'k-Means Clustering',
    ..., 
    cols = pal_hue()(n = length(levels(f)))
) {
  z <- x
  f <- x |>
    attr(which = 'f', exact = TRUE)
  marks(z) <- f
  plot.ppp(z, main = main, cols = cols, ...)
}





#' @title `split.pppkmlist`
#' 
#' @param x a `'pppkmlist'` object, returned from function [kmeans.ppplist()]
#' 
#' @param ... additional parameters, currently no use
#' 
#' @returns
#' The function [split.pppkmlist()] returns a `'splitppp'` object from the
#' workhorse function \link[spatstat.geom]{split.ppp}.
#' 
#' @keywords internal
#' @export split.pppkmlist
#' @export
split.pppkmlist <- function(x, ...) {
  
  tmp <- x |>
    lapply(FUN = split.pppkm, ...)
  
  sq <- x |>
    seq_along()
  
  ns <- tmp |> 
    lengths(use.names = FALSE)
  
  ret <- tmp |> 
    do.call(what = c)
  attr(ret, which = 'id') <- rep(sq, times = ns)
  attr(ret, which = 'cluster') <- ns |> 
    lapply(FUN = seq_len) |> 
    unlist(use.names = FALSE)
  return(ret)
  
}






#' @title [split.hyperframekm()]
#' 
#' @param x a `'hyperframekm'`, returned from function [kmeans.hyperframe()]
#' 
#' @param ... additional parameters, currently no use
#' 
#' @returns
#' The function [split.hyperframekm()] returns a `'groupedHyperframe'`.
#' 
#' @keywords internal
#' @importFrom spatstat.geom is.ppplist hyperframe cbind.hyperframe
#' @export split.hyperframekm
#' @export
split.hyperframekm <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  x. <- unclass(x)$df
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA) |>
    which()
  if (length(hc_ppp) != 1L) stop('shouldnt happen')
  
  ok <- hc[[hc_ppp]] |> 
    vapply(FUN = inherits, what = 'pppkm', FUN.VALUE = NA)
  if (!all(ok)) stop('shouldnt happen')
  
  tmp <- (hc[[hc_ppp]]) |>
    split.pppkmlist(...)
    
  id <- tmp |> 
    attr(which = 'id', exact = TRUE)
    
  ret <- hyperframe(
    tmp,
    .id = id,
    .cluster = tmp |> attr(which = 'cluster', exact = TRUE)
  ) |>
    cbind.hyperframe(x.[id, , drop = FALSE])
  
  names(ret)[1L] <- names(hc_ppp)
  
  if (inherits(x, what = 'groupedHyperframe')) {
    # haven't tested, but should be correct; very simple anyway!
    grp <- x |> 
      attr(which = 'group', exact = TRUE)
    
    # `.id` should be equivalent to the existing lowest cluster!!!
    ret$.id <- NULL
    
    grp[[2L]] <- call('/', grp[[2L]], quote(.cluster))
    attr(ret, which = 'group') <- grp
  } else attr(ret, which = 'group') <- '~ .id/.cluster' |> str2lang()
  
  class(ret) <- c('groupedHyperframe', class(ret)) |> unique.default()
  return(ret)
    
}

