# extract simplicial complex from TDA package filtration
ensure_cmplx <- function(x) {
  if (! is.null(names(x)) &&
      all(names(x) == c("cmplx", "values", "increasing", "coordinates"))) {
    warning("Taking `cmplx` element as the simplicial complex.")
    x <- x$cmplx
  }
  x
}

# ensure that input is a list of numeric vectors
ensure_list <- function(x) {
  stopifnot(
    typeof(x) == "list",
    all(unique(vapply(x, typeof, "")) %in% c("integer", "double")),
    all(unlist(x) %% 1 == 0)
  )
  x
}

# check that `index` is an integer-valued attribute
ensure_index <- function(x, index) {
  stopifnot(
    is.character(index),
    index %in% igraph::vertex_attr_names(x),
    all(igraph::vertex_attr(x, index) %% 1 == 0)
  )
}

# get list of simplices from a 'Rcpp_SimplexTree' object
simplextree_list <- function(x) {
  # extract list of fixed-dimension simplex matrices
  simps <- x$as_list()
  # convert dimension-specific simplex matrices to a simplex list
  simp_dim <- if (utils::packageVersion("simplextree") >= "1.0.1") 2L else 1L
  do.call(c, lapply(simps, function(mat) {
    apply(mat, MARGIN = simp_dim, FUN = identity, simplify = FALSE)
  }))
}

# get edge matrix from a Python GUDHI simplex tree
py_gudhi_edgelist <- function(x) {
  el <- reticulate::iterate(x$get_skeleton(1L), function(s) s[[1L]])
  do.call(rbind, el[sapply(el, length) == 2L])
}

# sort an undirected edge list matrix by columns in order
sort_el <- function(x) {
  # put lower indices on left
  x <- cbind(pmin(x[, 1L], x[, 2L]), pmax(x[, 1L], x[, 2L]))
  # sort rows by from & to indices
  x[order(x[, 1L], x[, 2L]), ]
}

# sort a list of simplices by dimension and vertex indices
sort_lst <- function(x) {
  # ensure that all simplices are sorted
  x <- lapply(x, sort)
  # put simplices in order of dimension and then vertices
  x[order(sapply(x, length), sapply(x, paste, collapse = ""))]
}
