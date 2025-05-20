#' plot the graph showing the connected components
#'
#' This is a simple wrapper for some tidygraph/ggraph functions
#' that will let you find the connected components of a graph in which
#' the related pairs are connected by edges.  It also makes
#' a plot of them.
#'
#' Note that it appears that the 'ggraph' package must be loaded
#' for the plot output of this function to print correctly.
#' @param Pairs the tibble that comes out of `compile_related_pairs()`.
#' For this function it must have, at least the
#' columns `id_1`, `id_2`, and `dom_relat`.
#' @return
#' This returns a list with two components:
#' - `conn_comps`: a tibble with three columns:
#'     - `name`: the name of the sample
#'     - `cluster`: the index of the connected component the sample belongs to
#'     - `cluster_size`: the number of samples belonging
#'        to that cluster
#' - `plot`: a ggraph/ggplot plot object showing the connected components as vertices with
#'   edges between them.
#' @export
#' @examples
#' # get a Pairs tibble from the stored data
#' Pairs <- compile_related_pairs(species_1_slurped_results_1gen$samples)
#' PCC <-  plot_conn_comps(Pairs)
#'
#' # look at the conn_comps:
#' head(PCC$conn_comps)
#'
#' # if you want to print the plot, that seems to require
#' # loading the ggraph library
#' library(ggraph)
#' PCC$plot
plot_conn_comps <- function(Pairs) {
  fi_graph <- Pairs %>%
    mutate(`dom_relat-max_hit` = paste(dom_relat, max_hit, sep = "-")) %>%
    rename(from = id_1, to = id_2) %>%
    igraph::graph_from_data_frame(directed = FALSE) %>%
    tidygraph::as_tbl_graph()

  conn_comps <- fi_graph %>%
    igraph::components() %>%
    .$membership %>%
    enframe() %>%
    rename(cluster = value) %>%
    arrange(cluster, name) %>%
    group_by(cluster) %>%
    mutate(cluster_size = n()) %>%
    ungroup()

  fi_graph2 <- fi_graph %>%
    tidygraph::activate(nodes) %>%
    tidygraph::left_join(conn_comps, by = "name")

  # now, let's plot that thing
  pofs_clusters_plot <- ggraph::ggraph(fi_graph2, layout = "kk") +
    ggraph::geom_edge_link(
      aes(
        colour = `dom_relat-max_hit`
      )
    ) +
    ggraph::geom_node_point()


  list(
    conn_comps = conn_comps,
    plot = pofs_clusters_plot
  )

}
