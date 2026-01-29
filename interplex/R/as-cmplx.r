#' @title Coerce objects to lists of simplices
#'
#' @description Coerce objects to lists of simplices, as used by
#'   [the TDA package][TDA::TDA-package].
#'
#' @details
#'
#' `as_cmplx()` is a generic function with specific methods for different
#' simplicial complex S3 classes. It returns a list of integer vectors, each of
#' which represents a simplex, and _all_ simplices are included in the list.
#' When a filtration is constructed using `TDA::*Filtration()`, the first named
#' element of the returned list, `cmplx`, is a list whose *i*th element
#' contains the vertices of the *i*th simplex.
#' 
#' @template sec-classes-methods
#' 
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return A list of integer vectors, each encoding one simplex.
#' @example inst/examples/ex-as-cmplx.r
#' @export
as_cmplx <- function(x, ...) UseMethod("as_cmplx")

#' @rdname as_cmplx
#' @export
as_cmplx.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # return list
  x
}

#' @rdname as_cmplx
#' @export
as_cmplx.Rcpp_SimplexTree <- function(x, ...) {
  
  simplextree_list(x)
}

#' @rdname as_cmplx
#' @export
as_cmplx.gudhi.simplex_tree.SimplexTree <- function(x, ...) {
  res <- reticulate::iterate(x$get_simplices(), function(s) s[[1L]])
  res[order(sapply(res, length))]
}

#' @rdname as_cmplx
#' @export
as_cmplx.igraph <- function(x, index = NULL, ...) {
  if (! is.null(index)) ensure_index(x, index)
  
  # generate vertex list
  vl <- if (is.null(index)) igraph::V(x) else igraph::vertex_attr(x, index)
  # generate edge list
  el <- apply(
    igraph::as_edgelist(x, names = FALSE),
    1L, identity, simplify = FALSE
  )
  if (! is.null(index)) el <- lapply(el, function(e) vl[e])
  vl <- as.list(vl)
  
  # concatenate with a list of vertex vectors
  c(vl, el)
}

#' @rdname as_cmplx
#' @export
as_cmplx.network <- function(x, index = NULL, ...) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_cmplx(x, index = index)
}
