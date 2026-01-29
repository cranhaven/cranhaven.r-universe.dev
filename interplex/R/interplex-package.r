#' @title **interplex** package
#'
#' @description This is a helper package to coerce simplicial complexes between
#'   different data structures.
#'   

#' @details
#'
#' This package helps interface between different topological data analytic
#' packages and workflows by coercing simplicial complex objects stored using
#' different data structures. Each coercion is designed to retain as much
#' annotation as possible, whether of simplices or of the complex.
#'
#' The package supports coercions between simplicial complexes stored using
#' the following data structures:

#' * a complete list of simplices,
#'   as stored as `cmplx` values of filtration objects
#'   in [the TDA package][TDA::TDA-package]
#' * an object of class 'Rcpp_SimplexTree'
#'   as implemented
#'   in [the simplextree package][simplextree::simplextree-package]
#' * an object of class 'gudhi.simplex_tree.SimplexTree'
#'   as implemented in [Python GUDHI](https://gudhi.inria.fr/python/latest/)
#'   and imported via [reticulate][reticulate::reticulate]
#' * an 'igraph' object,
#'   as implemented
#'   in [the igraph package][igraph::igraph]
#' * a 'network' object,
#'   as implemented
#'   in [the network package][network::network-package]

#' @name interplex
"_PACKAGE"
