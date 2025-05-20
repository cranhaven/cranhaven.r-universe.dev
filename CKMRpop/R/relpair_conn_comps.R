#' Find connected components amongst the related pairs
#'
#' This is a little utility function that actually gets called from
#' within `compile_related_pairs()` so we leave it as an internal function.
#' This takes in the tibble Pairs, and then spits out a tibble that looks
#' just the same, except that after the id_1 and id_2 columns it now has a
#' column called `conn_comp` that gives the connected component index of the
#' two pair members.
#' @param Pairs a tibble with the related pairs.  It must have columns id_1 and
#' id_2
#' @keywords internal
relpair_conn_comps <- function(Pairs) {
  clusters <- Pairs %>%
    rename(to = id_1, from = id_2) %>%
    select(to, from) %>%
    igraph::graph_from_data_frame() %>%
    igraph::components() %>%
    .$membership %>%
    enframe() %>%
    rename(indiv = name, cluster = value) %>%
    arrange(cluster, indiv)

  # now get a data frame that has the pairs and cc's
  pairs_n_ccs <- Pairs %>%
    left_join(clusters %>% rename(cc_1 = cluster), by = c("id_1" = "indiv")) %>%
    left_join(clusters %>% rename(cc_2 = cluster), by = c("id_2" = "indiv"))

  # cc_1 and cc_2 should be the same.  If not we throw a warning.
  if(any(pairs_n_ccs$cc_1 != pairs_n_ccs$cc_2)) {
    warning("Weirdness, pair members with different cc_1 and cc_2.  Please inspect.")
  }

  # collapse those down to a single column called conn_comp
  P <- pairs_n_ccs %>%
    rename(conn_comp = cc_1) %>%
    select(-cc_2) %>%
    select(id_1, id_2, conn_comp, everything())

  # in the end, just return P
  P
}
