#' @title Coerce objects to class 'igraph'
#'
#' @description Coerce objects to 'igraph' objects, as implemented in
#'   [the igraph package][igraph::igraph-package].
#'
#' @details
#'
#' `as_igraph()` is a generic function with specific methods for different
#' simplicial complex S3 classes. It returns an [igraph][igraph::igraph] object.
#'
#' @template sec-classes-methods
#' @template to-graph
#'
#' @param x An R object to be coerced. See Details.
#' @param index Character string to be added as a vertex attribute containing
#'   0-simplex indices. Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return An object of class 'igraph'.
#' @example inst/examples/ex-as-igraph.r
#' @export
as_igraph <- function(x, ...) UseMethod("as_igraph")

#' @rdname as_igraph
#' @export
as_igraph.default <- function(x, index = NULL, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # subset to 0- and 1-simplices
  x <- x[sapply(x, length) <= 2L]
  # store vertex IDs
  x_vid <- sort(unique(unlist(x)))
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = match(unlist(x[sapply(x, length) == 2L]), x_vid),
    n = max(match(unlist(x[sapply(x, length) == 1L]), x_vid)),
    directed = FALSE
  )
  # add vertex IDs as an attribute
  if (! is.null(index))
    res <- igraph::set_vertex_attr(res, index, value = x_vid)
  
  res
}

#' @rdname as_igraph
#' @export
as_igraph.Rcpp_SimplexTree <- function(x, index = NULL, ...) {
  
  # store vertex IDs
  x_vid <- as.integer(x$vertices)
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = as.vector(rbind(
      match(x$edges[, 1L], x_vid),
      match(x$edges[, 2L], x_vid)
    )),
    n = length(x$vertices),
    directed = FALSE
  )
  # add vertex IDs as an attribute
  if (! is.null(index))
    res <- igraph::set_vertex_attr(res, index, value = x_vid)
  
  res
}

#' @rdname as_igraph
#' @export
as_igraph.gudhi.simplex_tree.SimplexTree <- function(x, index = NULL, ...) {
  
  # store vertices
  x_vid <- reticulate::iterate(
    x$get_skeleton(0L),
    function(s) s[[1]],
    simplify = TRUE
  )
  # store edges as a vector
  x_edges <- reticulate::iterate(
    x$get_skeleton(1L),
    function(s) s[[1]],
    simplify = FALSE
  )
  x_edges <- unlist(x_edges[sapply(x_edges, length) == 2L])
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = match(x_edges, x_vid),
    n = x$num_vertices(),
    directed = FALSE
  )
  # add vertex IDs as an attribute
  if (! is.null(index))
    res <- igraph::set_vertex_attr(res, index, value = x_vid)
  
  res
}

#' @rdname as_igraph
#' @export
as_igraph.igraph <- function(x, ...) x

#' @rdname as_igraph
#' @export
as_igraph.network <- function(x, ...) {
  # defer to intergraph
  intergraph::asIgraph(x, ...)
}
