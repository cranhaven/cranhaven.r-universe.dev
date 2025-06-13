#' foodingraph: a package for food network inference
#'
#' The foodingraph package provide two categories of functions :
#' \enumerate{
#' \item confidence-interval (CI) bootstrap inference of mutual information (MI)
#' or maximal information coefficient (MIC) adjacency matrices.
#' \item network visualization in a graph using \code{\link{igraph}} and
#' \code{\link{ggraph}}
#' }
#'
#' @section CI bootstrap network inference:
#' The two functions are
#' \enumerate{
#' \item \code{\link{boot_cat_bin}} : a function to perform the CI bootstrap
#' inference for pairwise associations between ordinal and binary variables.
#' It uses thresholds defined by simulation of independent associations using
#' \code{\link{boot_simulated_cat_bin}}, such that it simulates independent
#' associations between ordinal-ordinal, binary-binary and ordinal-binary pairs
#' of variables.
#' It calculates the CI bootstraps for each pairwise association of the variables'
#' dataset, then compares the 1st percentile of these CI to the corresponding
#' thresholds of independent data.
#' \item \code{\link{boot_simulated_cat_bin}} : a function to determine the threshold
#' values of MI or MIC of independent pairs
#' of variables (ordinal vs. ordinal, and binary vs binary and ordinal vs. binary).
#' It calculates the CI bootstraps of MI or MIC for these pairs of variables,
#' and return a defined percentile of these CI (e.g. 99th percentile).
#' }
#'
#' @section Network visualization:
#' The three main functions are
#' \enumerate{
#' \item \code{\link{graph_from_matrix}} : create a graph from an adjacency matrix.
#' This function need at least two arguments : \bold{1.} the adjacency matrix, in
#' which the column names and row names are the node names. \bold{2.} the legend,
#' which is a data frame of at least two columns : \code{name} (the name of the nodes
#' in the adjacency matrix, e.g. CRUDSAL_cat) and \code{title} (the titles for each
#' name, e.g. raw vegetables)\cr
#' Optionally, you can add a column \code{family} to specify the nodes' families.
#' \item \code{\link{graph_from_links_nodes}} : create a graph from a list of nodes
#' and links. This function needs two arguments : \bold{1.} the list of nodes
#' and links, which should be the result from \code{\link{links_nodes_from_mat}}
#' (if not, make sure the structure corresponds). \bold{2.} the legend
#' (described above).
#' \item \code{\link{compare_graphs}} : a function to compare two graphs.
#' It unifies the legends and attributes, so the graphs can be visually
#' comparable.
#' \item \code{\link{save_graph}} : a function to save the graph in a file at high
#' resolution.
#' }
#'
#' @section Utils functions:
#' Other functions include
#' \enumerate{
#' \item \code{\link{family_palette}} : to create a color palette to be used in the
#' graph. It is usually done automatically, but can prove useful if comparing multiple
#' graphs, to ensure the family colors remain the same throughout the graphs.
#' \item \code{\link{links_nodes_from_mat}} : to extract the links and nodes from an
#' adjacency matrix
#' \item \code{\link{mic_adj_matrix}} : using the \code{cstats} function from
#' the minerva package, calculate the adjacency MIC matrix.
#' }
#'
#' @aliases foodingraph-package
#' @aliases foodingraph
#' @docType package
#' @name foodingraph
NULL
