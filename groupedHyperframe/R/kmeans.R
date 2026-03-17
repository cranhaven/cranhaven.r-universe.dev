

#' @title Pseudo `S3` Methods based on \link[stats]{kmeans}
#' 
#' @param x see **Usage**
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param ... additional parameters of the function \link[stats]{kmeans}
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name kmeans_etc
NULL



#' @rdname kmeans_etc
#' 
#' @param centers \link[base]{integer} scalar, number of clusters \eqn{k}, see function \link[stats]{kmeans}
#' 
#' @param clusterSize \link[base]{integer} scalar, number of points per cluster
#' 
#' @returns
#' The function [kmeans.ppp()] returns an object of class `'pppkm'`, 
#' which inherits from \link[spatstat.geom]{ppp.object}.
#' 
#' @importFrom spatstat.geom marks.ppp markformat.ppp
#' @export
kmeans.ppp <- function(
    x, 
    formula, 
    centers = (x[['n']]/clusterSize) |> ceiling() |> as.integer(),
    clusterSize, 
    ...
) {
  
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  
  v <- formula[[2L]] |> all.vars()
  v_m <- v |>
    setdiff(y = c('x', 'y'))
  
  #if (markformat.ppp(x) != 'dataframe') stop('markformat must be dataframe')
  
  m <- x |> 
    marks.ppp(drop = FALSE)
  
  switch(markformat.ppp(x), none = {
    num_ <- character()
    m. <- NULL
  }, vector = {
    num_ <- 'marks' # `x$marks`
    m. <- array(m, dim = c(length(m), 1L), dimnames = list(NULL, 'marks'))
  }, dataframe = {
    num_ <- m |>
      vapply(FUN = is.numeric, FUN.VALUE = NA) |>
      which() |>
      names()
    m. <- m[num_] |> 
      as.matrix.data.frame()
  })

  if (!all(v_m %in% num_)) stop('some terms in formula are not numeric mark')
  
  tmp <- cbind(
    x = if ('x' %in% v) x$x, # else NULL
    y = if ('y' %in% v) x$y, # else NULL
    m.[, v_m, drop = FALSE] # 'matrix'
  )
  
  km <- tmp |> 
    kmeans(centers = centers, ...)
  
  z <- x
  attr(z, which = 'f') <- km[['cluster']] |> # 'integer'
    factor()
  class(z) <- c('pppkm', class(z)) |> 
    unique.default()
  return(z)
  
}


#' @rdname kmeans_etc
#' 
#' @returns
#' The function [kmeans.ppplist()] returns an object of class `'pppkmlist'`, 
#' which inherits from `'ppplist'`.
#' 
#' @importFrom spatstat.geom solapply
#' @export
kmeans.ppplist <- function(x, ...) {
  
  z <- x |>
    solapply(FUN = kmeans.ppp, ...)
  class(z) <- c('pppkmlist', class(z)) |>
    unique.default()
  return(z)
  
}
    




#' @rdname kmeans_etc
#' @importFrom spatstat.geom is.ppplist
#' @export
kmeans.hyperframe <- function(x, ...) {
  
  x0 <- unclass(x)
  
  hc <- x0$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA) |>
    which()
  n_ppp <- length(hc_ppp)
  
  if (!n_ppp) {
    
    return(x) # exception handling
    
  } else if (n_ppp == 1L) {
    
    x0$hypercolumns[[hc_ppp]] <- (hc[[hc_ppp]]) |>
      kmeans.ppplist(...)
    class(x0) <- c('hyperframekm', class(x)) |>
      unique.default()
    return(x0)
    
  } else stop('more than one ppp-hypercolumn, ambiguity!')
  
}




