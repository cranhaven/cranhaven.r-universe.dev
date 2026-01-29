#' @title Coerce objects to Python GUDHI simplex trees
#'
#' @description Coerce objects to 'SimplexTree' objects in Python GUDHI,
#'   accessed via [the reticulate package][reticulate::reticulate].
#'
#' @details
#'
#' `as_py_gudhi_simplextree()` is a generic function with specific methods for
#' different simplicial complex S3 classes. It returns an object of class
#' 'gudhi.simplex_tree.SimplexTree', which is a
#' [reticulate][reticulate::reticulate] accessor to a Python object of class
#' 'SimplexTree' implemented in GUDHI.
#'
#' @template sec-classes-methods
#'
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return A simplex tree instantiated in Python GUDHI accessed through
#'   reticulate.
#' @example inst/examples/ex-as-py-gudhi.r
#' @author Jason Cory Brunson
#' @author Yara Skaf
#' @export
as_py_gudhi_simplextree <- function(x, ...) UseMethod("as_py_gudhi_simplextree")

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  # import GUDHI
  # TODO: Is it possible to detect already imported, and as what?
  gd <- reticulate::import("gudhi")
  
  # insert all simplices into a new simplex tree
  res <- gd$SimplexTree()
  for (s in x) res$insert(as.list(s))
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.Rcpp_SimplexTree <- function(x, ...) {
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # insert maximal simplices into a new simplex tree
  res <- gd$SimplexTree()
  .simplextree_version <- utils::packageVersion("simplextree")
  if (.simplextree_version >= "1.0.1") {
    # traverse insertion over maximal simplices
    traverse <- utils::getFromNamespace("traverse", "simplextree")
    maximal <- utils::getFromNamespace("maximal", "simplextree")
    traverse(
      maximal(x),
      function(s) res$insert(as.list(s))
    )
  } else if (.simplextree_version == "0.9.1") {
    # loop insertion over serialization
    for (s in x$serialize()) res$insert(as.list(s))
  } else {
    stop("No method available for simplextree v", .simplextree_version)
  }
  
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.igraph <- function(x, index = NULL, ...) {
  if (! is.null(index)) ensure_index(x, index)
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # generate vertex list
  vl <- if (is.null(index)) igraph::V(x) else igraph::vertex_attr(x, index)
  # generate edge list
  el <- apply(
    igraph::as_edgelist(x, names = FALSE),
    1L, identity, simplify = FALSE
  )
  if (! is.null(index)) el <- lapply(el, function(e) vl[e])
  vl <- as.list(vl)
  
  # insert vertices and edges into a new simplex tree
  res <- gd$SimplexTree()
  for (s in c(vl, el)) res$insert(as.list(s))
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.network <- function(x, index = NULL, ...) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_py_gudhi_simplextree(x, index = index)
}
