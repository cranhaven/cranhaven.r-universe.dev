#' @title Coerce objects to class 'Rcpp_SimplexTree'
#'
#' @description Coerce objects to 'Rcpp_SimplexTree' objects, as implemented in
#'   [the simplextree package][simplextree::simplextree-package].
#'
#' @details
#'
#' `as_rcpp_simplextree()` is a generic function with specific methods for
#' different simplicial complex S3 classes. It returns an object of class
#' ['Rcpp_SimplexTree'][simplextree::SimplexTree], which is an [Rcpp
#' Module][Rcpp::Module] that exposes an instance of a C++ instance of a simplex
#' tree.
#'
#' @template sec-classes-methods
#'
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return An instance of a simplex tree, exposed as an Rcpp Module with class
#'   'Rcpp_SimplexTree'.
#' @example inst/examples/ex-as-simplextree.r
#' @export
as_rcpp_simplextree <- function(x, ...) UseMethod("as_rcpp_simplextree")

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # insert all simplices into a new simplex tree
  res <- simplextree::simplex_tree()
  res$insert(x)
  res
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.Rcpp_SimplexTree <- function(x, ...) x

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.gudhi.simplex_tree.SimplexTree <- function(x, ...) {
  
  # initialize simplex tree
  res <- simplextree::simplex_tree()
  # iteratively insert simplices
  reticulate::iterate(x$get_simplices(), function(s) res$insert(s[[1L]]))
  # return simplex tree
  res
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.igraph <- function(x, index = NULL, ...) {
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
  
  # initialize simplicial complex
  res <- simplextree::simplex_tree()
  # insert vertices and edges
  res$insert(vl)
  res$insert(el)
  
  res
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.network <- function(x, index = NULL, ...) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_rcpp_simplextree(x, index = index)
}
