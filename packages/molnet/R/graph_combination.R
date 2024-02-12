inter_layer_edgelist_by_id <- function(annotation_1, annotation_2, connection, weight = 1) {
  #' Interlayer conntections by identifiers
  #'
  #' (INTERNAL) Returns an edge list defining the connections between two layers of the network.
  #'
  #' @param annotation_1,annotation_2 Data frames: Annotation tables specifying the identifiers of
  #' the nodes of a network
  #' @param connection String of identifier to connect on
  #' @param weight Integer or vector specifying the weight of the inter-layer connections.
  #' @export
  #' @return data.frame with columns from, to and weight
  #' @importFrom rlang .data

  inter_graph <- annotation_1 %>%
    dplyr::inner_join(annotation_2, by = connection) %>%
    dplyr::select(from=.data$node_id.x, to=.data$node_id.y) %>%
    tibble::add_column(weight = weight)

  return(inter_graph)
}

inter_layer_edgelist_by_table <- function(annotation_1, annotation_2, interaction_table,
                                          weight_column) {
  #' Interaction table to iGraph graph object
  #'
  #' (INTERNAL) Returns an edge list defining the connections between two layers of the network
  #' based on an
  #' interaction table supplied by the user.
  #'
  #' @param annotation_1,annotation_2 Data frames: Annotation tables specifying the identifiers of
  #' the nodes of a network
  #' @param interaction_table Table specifying the interaction / connections between the two layers
  #' @param weight_column Name of the column in 'interaction_table' giving the weight of the
  #' inter-layer edges.
  #' @export
  #' @return data.frame with columns from, to and weight
  #' @importFrom rlang .data

  id.x <- intersect(colnames(annotation_1), colnames(interaction_table))[1]
  id.y <- intersect(colnames(annotation_2), colnames(interaction_table))[1]

  inter_graph <- annotation_1 %>%
    dplyr::inner_join(interaction_table, by = id.x) %>%
    dplyr::inner_join(annotation_2, by = id.y) %>%
    dplyr::rename(weight = dplyr::all_of(weight_column)) %>%
    dplyr::select(from=.data$node_id.x, to=.data$node_id.y, .data$weight)

  return(inter_graph)
}

combine_graphs <- function(graphs, inter_layer_edgelists) {
  #' Combining graphs by adding inter-layer edges
  #'
  #' Creates the union of all graphs and adds the inter-layer edges.
  #'
  #' @param graphs List of iGraph objects
  #' @param inter_layer_edgelists List of data frames containing inter-layer edges
  #'
  #' @return iGraph object which is the union of the input graphs with isolated nodes removed.

  all_inter_layer_edges <- dplyr::bind_rows(inter_layer_edgelists)
  inter_layer_edges <- as.vector(t(as.matrix(all_inter_layer_edges[ , 1:2])))
  inter_layer_weights <- as.vector(all_inter_layer_edges[ , 3])

  union <- igraph::disjoint_union(graphs)
  union <- igraph::add_edges(graph = union,
                             edges = inter_layer_edges,
                             weight = inter_layer_weights)
  return(union)
}
