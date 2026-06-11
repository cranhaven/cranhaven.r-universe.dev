#' Create Markov Blanket
#'
#' Given a graph and a specified \code{vector} of internal node(s), returns
#' the parents, the children, and the parents of the children of the
#' internal node(s).
#'
#' @param graph igraph or matrix object.
#' @param nodes Numeric vector of vertices.
#'
#' @return A \code{list} of node descendants, parents, and neighbors.
mb <- function(graph, nodes = igraph::V(graph)) {
  lapply(
    seq_along(nodes),
    function(i) {
      nodes_out <- igraph::neighborhood(graph, nodes[[i]], order=1, mode="out")[[1]] %>%
        setdiff(nodes[[i]]) %>%
        igraph::neighborhood(graph, ., order=1, mode="in") %>%
        unlist

      igraph::neighborhood(graph, nodes[[i]], order=1, mode="in")[[1]] %>%
        union(nodes_out) %>%
        setdiff(nodes[[i]])
    }
  )
}

#' Stationary Distribution
#'
#' Gives a stationary probability vector of a given network.
#'
#' @param graph igraph or matrix object.
#' @param zero_cutoff Numeric threshold for zero value.
#'
#' @return A numeric \code{vector} corresponding to stationary distribution.
stationary <- function(graph, zero_cutoff = 1e-10) {
  A <- igraph::as_adj(graph, attr = "weight")

  rows <- NROW(A)

  I <- diag(nrow = rows)

  A <- rbind(t(as.matrix(I - A)), rep(1, rows))
  B <- c(rep(0, rows), 1)

  P <- stats::lm(B ~ 0 + A)$coefficients
  P[P < zero_cutoff] <- 0
  P[is.na(P)] <- 0

  if (sum(P) != 0 && sum(P) != 1) {
    P <- P / sum(P)
  }

  as.numeric(P)
}

#'  Update Markov Blanket
#'
#' @param blanket List of previous markov blanket.
#' @param removal Numeric vector for node removal.
update_blanket <- function(blanket, removal = NULL) {
  lapply(
    seq_along(blanket),
    function(i) setdiff(blanket[[i]], removal)
  )
}

#' Causal Emergence
#'
#' Given a microscale network, \code{G}, this function iteratively checks different
#' coarse-grainings to see if it finds one with higher effective information.
#'
#' @param x igraph or matrix object.
#' @param ... Span, and threshold parameters
#'
#' @return A list with letters and numbers.
#' \itemize{
#'   \item g_micro - Graph of original micro-scale network.
#'   \item g_macro - Graph of macro-scale network.
#'   \item mapping - \code{list} mapping from micro to macro scales giving
#' the largest increase in effective information.
#'   \item ei_macro - Effective information of macro scale network.
#'   \item ei_micro - Effective information of micro scale network.
#'   \item ce - Numerical value for causal emergence.
#' }
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
#' causal_emergence(graph)
#'
#' @export
causal_emergence <- function(x, ...) UseMethod("causal_emergence")

#' @export
causal_emergence.matrix <- function(x, ...) {
  assertthat::assert_that(is.matrix(x))

  graph <- igraph::graph.adjacency(x,  mode = "directed")

  causal_emergence.igraph(graph, ...)
}

#' @export
causal_emergence.list <- function(x, ...) {
  valid <- sapply(x, igraph::is_igraph)

  assertthat::assert_that(all(valid))

  lapply(x, causal_emergence.igraph)
}

#' @export
causal_emergence.igraph <- function(x,
                                    span            = -1,
                                    thresh          = 1e-4,
                                    types           = FALSE,
                                    max_iterations  = 1000,
                                    ...) {
  graph <- x
  assertthat::assert_that(igraph::is.igraph(graph))

  graph_micro <- check_network(graph)

  w_out <- graph_micro %>%
    igraph::as_adj(attr = "weight")

  nodes_left <- igraph::V(graph_micro)
  blanket <- mb(graph_micro, nodes_left)

  span <- ifelse(span > 0, span, length(nodes_left))
  shuffle <-  sample(seq_along(nodes_left), size = span)

  ei_micro <- effective_information.igraph(graph_micro)
  eff_micro <- effective_information.igraph(graph_micro, effectiveness = TRUE)

  ei_current <- ei_micro
  eff_current <- eff_micro

  checked_macros <- c()
  macro_types <- list()
  macro_types_tmp <- macro_types

  current_mapping <- nodes_left %>%
    setNames(nodes_left)

  for (i in seq_along(shuffle)) {
    node_i <- nodes_left[[shuffle[[i]]]]
    progress <- (i / length(shuffle)) * 100

    sprintf('[%.1f%%] Checking node %d\n', progress, node_i) %>%
      message

    macros_to_check <- update_blanket(blanket, checked_macros)[[node_i]]
    queue <- macros_to_check %>% sort

    if (length(macros_to_check) < 1) {
      next
    }

    node_i_macro <- ifelse(
      get_macro(current_mapping, node_i) == node_i,
      max_macro(current_mapping) + 1,
      get_macro(current_mapping, node_i)
    )

    iteration <- 0
    while (length(queue) > 0) {
      iteration <- iteration + 1

      queue <- queue[sample(seq_along(queue))]

      possible_macro <- utils::tail(queue, 1) %>%
        as.numeric

      queue <- utils::head(queue, -1)

      possible_mapping <- current_mapping %>%
        set_macro(c(possible_macro, node_i), node_i_macro)

      if (types) {

      } else {
        macro_types_tmp[[node_i_macro]] <- "spatem1"
        graph_macro <- create_macro(graph_micro, possible_mapping, macro_types)
      }

      graph_macro = check_network(graph_macro)
      ei_macro = effective_information(graph_macro)
      eff_macro <- effective_information.igraph(graph, effectiveness = TRUE)

      if (is.na(ei_macro)) {
        effective_information(graph_macro)
      }

      if (is.infinite(ei_macro)) {
        return(graph_macro)
      }

      if ((ei_macro - ei_current) > thresh) {
        ei_current <- ei_macro
        eff_current <- eff_macro

        sprintf(
          'Successful macro grouping found.  New effective information: %.4f',
          eff_current
        ) %>%
          message

        macro_mapping <- possible_mapping
        macro_types <- macro_types_tmp

        checked_macros <- checked_macros %>%
          append(c(as.numeric(node_i), possible_mapping))

        nodes_in_macro_i <- which(get_macro(macro_mapping) %in% node_i_macro) %>%
          macro_mapping[.] %>%
          as.numeric

        for (new_micro_i in seq_along(nodes_in_macro_i)) {
          neighbors_i_M <- mb(graph_micro, new_micro_i)[[1]] %>%
            as.numeric

          for (node_j_M in neighbors_i_M) {
            if (!(node_j_M %in% queue) && node_j_M != node_i) {
              queue <- append(queue, node_j_M)
            }
          }
        }
      }

      if (iteration > max_iterations) {
        break
      }
    }
  }

  structure(
    list(
      g_micro     = graph_micro,
      g_macro     = create_macro(graph, current_mapping, macro_types) %>%
        check_network(),
      mapping     = current_mapping,
      ei_macro    = ei_current,
      ei_micro    = ei_micro,
      ce          = (eff_micro - eff_micro) / log2(igraph::vcount(graph_micro))
    ),
    class = "CE"
  )
}
