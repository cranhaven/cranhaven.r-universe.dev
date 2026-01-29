#' treepcor: correlation from tree
#' @description A tree with two kind of nodes,
#' parents and children. The parents are nodes with
#' children. The children are nodes with no children.
#' This is used to model correlation matrices, where
#' parents represent latent variables, and children
#' represent the variables of interest.
setClass("treepcor")

#' graphpcor: correlation from nodes and edges
#' @description A graphpcor is a graph where
#' a node represents a variable and an edge
#' represent a conditional distribution.
#' The correlation built from a `graphpcor` consider
#' the parameters for the Cholesky of a precision matrix,
#' whose non-zero pattern is given from the graph.
setClass("graphpcor")
