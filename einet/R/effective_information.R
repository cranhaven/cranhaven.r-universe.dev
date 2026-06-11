#' Effective Information
#'
#' Calculates the effective information (EI) of a network, \eqn{G}, according to
#' the definition provided in Klein & Hoel, 2019. Here, we subtract the
#' average entropies of the out-weights of nodes in a network, WOUT_average
#' from the entropy of the average out-weights in the network, WIN_entropy.
#'
#' @param graph igraph or matrix object.
#' @param effectiveness Logical indicating whether or not to return network effectiveness.
#'
#' @return Numeric value indicating the effective information of the network.
#'
#' @examples
#' graph <- matrix(
#'   cbind(
#'     c(0.0, 1.0, 0.0, 0.0),
#'     c(0.0, 0.0, 1.0, 0.0),
#'     c(0.0, 0.0, 0.0, 1.0),
#'     c(0.0, 0.0, 0.0, 0.0)
#'   ),
#'  nrow = 4
#' ) %>%
#'   igraph::graph.adjacency(mode = "directed")
#'
#' effective_information(graph)
#'
#' @export
effective_information <- function(graph, effectiveness = FALSE) UseMethod("effective_information")

#' @export
effective_information.matrix <- function(graph, effectiveness = FALSE) {
  assertthat::assert_that(is.matrix(graph))

  graph <- igraph::graph.adjacency(graph,  mode = "directed")

  effective_information.igraph(graph, effectiveness)
}

#' @export
effective_information.igraph <- function(graph, effectiveness = FALSE) {
  assertthat::assert_that(igraph::is.igraph(graph))

  graph <- check_network(graph)
  nodes <- igraph::V(graph)
  out_edges <- igraph::incident_edges(graph, nodes, mode = "out")

  n_out <- out_edges %>%
    Filter(function(v) length(v) > 0, .) %>%
    length

  if (length(igraph::incident_edges) == 0) {
    return(0)
  }

  w_out <- vector(mode = "numeric", length = length(nodes))
  w_in <- vector(mode = "numeric", length = length(nodes))

  set_win <- function(i) {
    edges <- out_edges[[i]]

    weights <- edges %>%
      igraph::get.edge.attribute(graph, "weight", .) %>%
      as.numeric

    assertthat::assert_that(
      !any(is.na(weights)),
      msg = paste('missing weight values for node:', i)
    )

    w_out[i] <<- weights %>%
      entropy::entropy(unit = "log2")

    targets <- igraph::head_of(graph, edges)
    w_in[targets] <<-  w_in[targets] + (weights / n_out)
  }

  lapply(seq_along(nodes), set_win)

  w_out_average <- sum(w_out) / n_out
  win_entropy <- entropy::entropy(w_in, unit = "log2")

  if (effectiveness) {
    return(
      (win_entropy - w_out_average) / log2(length(nodes))
    )
  }

  win_entropy - w_out_average
}
