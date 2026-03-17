

#' @title Pairwise Tjostheim's Coefficient
#' 
#' @param x see **Usage**
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Workhorse function \link[SpatialPack]{cor.spatial}.
#' 
#' @returns 
#' The function [pairwise_cor_spatial()] returns a \link[stats]{dist} object.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name pairwise_cor_spatial
#' @export
pairwise_cor_spatial <- function(x, ...) UseMethod(generic = 'pairwise_cor_spatial')



#' @rdname pairwise_cor_spatial
#' @importFrom SpatialPack cor.spatial
#' @importFrom spatstat.geom marks
#' @export pairwise_cor_spatial.ppp
#' @export
pairwise_cor_spatial.ppp <- function(x, ...) {
  
  m <- x |> marks()
  
  id <- x |>
    is.numeric.ppp()
  nv <- sum(id)
  if (nv <= 1L) stop('`x` must have 2+ numeric marks')
  v <- id |> which() |> names()
  
  #co <- x |> spatstat.geom::coords.ppp() |> as.matrix.data.frame() # nah..
  co <- cbind(x$x, x$y)

  ret <- double()
  for (i in seq_len(nv-1L)) {
    for (j in (i+1):nv) {
      ret <- c(ret, cor.spatial(x = m[[v[i]]], y = m[[v[j]]], coords = co))
    }
  }
  attr(ret, which = 'Size') <- nv
  attr(ret, which = 'Labels') <- v
  attr(ret, which = 'Diag') <- FALSE
  class(ret) <- c('pairwise_cor_spatial', 'dist') # ?stats::dist
  return(ret)
  
}




#' @rdname pairwise_cor_spatial
#' @export pairwise_cor_spatial.ppplist
#' @export
pairwise_cor_spatial.ppplist <- function(x, ...) {
  x |>
    lapply(FUN = pairwise_cor_spatial.ppp, ...)
}
  
  
  
  

#' @title Convert [pairwise_cor_spatial] to \link[base]{matrix}
#' 
#' @param x a [pairwise_cor_spatial]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [as.matrix.pairwise_cor_spatial()] returns a \link[base]{matrix}.
#' 
#' @keywords internal
#' @method as.matrix pairwise_cor_spatial
#' @export as.matrix.pairwise_cor_spatial
#' @export
as.matrix.pairwise_cor_spatial <- function(x, ...) {
  ret <- NextMethod(generic = 'as.matrix') # invokes ?stats:::as.matrix.dist
  diag(ret) <- 1 # ?SpatialPack::cor.spatial returns `1` for `identical(x, y)`
  return(ret)
}

