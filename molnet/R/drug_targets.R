target_edge_list <- function(graph, target_nodes, group) {
  #' Edges adjacent to target nodes
  #'
  #' @description (INTERNAL) Based on the supplied graph and target nodes this function returns a
  #' list of edges that are directly adjacent to target nodes. These edges can be used for further
  #' computation to find differential scores in the networks.
  #'
  #' @param graph Combined graph (iGraph graph object) for a specific group
  #' @param target_nodes Data frame. Has column `node_id` (unique node IDs in the iGraph graph
  #' object that are targeted by drugs) and columns `group1` and `group2` (boolean values specifying
  #'  whether the node is contained in the combined graph of the group)
  #' @param group Character. Indicates which group is analyzed.
  #'
  #' @export
  #'
  #' @return An edge list as a data frame.
  #' @importFrom rlang .data

  # filter for nodes contained in combined graph
  target_node_ids <- target_nodes[target_nodes[ , group], "node_id"]

  edge_list <- graph %>%
    igraph::as_data_frame('edges') %>%
    dplyr::filter(.data$from %in% target_node_ids | .data$to %in% target_node_ids)

  return(edge_list)
}



find_targets <- function(graphs, target_molecules, interaction_table, annotation, on) {
  #' @title Filter drug target nodes
  #'
  #' @description (INTERNAL) Based on the supplied target molecules, interaction table, graph and
  #' annotation this function returns a data frame containing nodes in the network targeted by a
  #' drug and a list containing the drug names as names and a vector of node IDs as keys.
  #'
  #' @param graphs List of two iGraph graph objects (one for each group)
  #' @param target_molecules Character string. Identifies the type of the target molecules (e.g.,
  #' `protein`). The string must be contained in the `type` column of the annotation data frame.
  #' @param interaction_table Data frame. Specifying the interaction of drugs and target molecules.
  #' Must contain a column `drug_name` containing drug names/identifiers and a column named like
  #' the character string given in the `on` argument, which must be an identifier for the targeted
  #' molecule.
  #' @param annotation Data frame. Contains the annotation for all the nodes contained in the
  #' combined network. Must contain a column `node_id` (vertex IDs in iGraph graph object) and a
  #' column named like the character string given in the `on` argument, which must be an identifier
  #' for the targeted molecule.
  #' @param on Character string. Defines the ID that is used to match drugs to their targets. Both
  #' supplied data frames (`annotation` and `interaction_table`) must contain a column named like
  #' this character string.
  #'
  #' @export
  #'
  #' @return A named list. Element `target_nodes` is a data frame with column `node_id` (unique node
  #'  IDs in the iGraph graph object that are targeted by drugs) and columns `group1` and `group2`
  #'  (boolean values specifying whether the node is contained in the combined graph of the group).
  #'  Element `drugs_to_target_nodes` contains a named list: elements are `drug_names` and contain a
  #'   vector of node IDs that are their specific targets.
  #' @importFrom rlang .data

  # filter annotation for components that appear in the interaction table
  target_annotation <-  annotation %>%
    dplyr::filter(.data$layer == target_molecules) %>% # filter annotation table for requested molecule type
    dplyr::inner_join(interaction_table, by = on) # join annotation table with interaction_table to return only matches on the specified identifier

  target_annotation <- target_annotation[!is.na(target_annotation$drug_name), ] # filter out rows with undefined drug_name

  # group by drug name attach node ids, returns a named list
  drugs_to_target_nodes <- split(target_annotation$node_id, target_annotation$drug_name)

  # get unique nodes that are drug targets
  target_nodes <- target_annotation %>%
    dplyr::select(.data$node_id) %>%
    unique()

  # check whether target nodes are contained in the combined graphs of the group
  # check is necessary as some nodes might be deleted due to network reduction
  target_nodes$group1 <- target_nodes$node_id %in% igraph::V(graphs[["group1"]])$name
  target_nodes$group2 <- target_nodes$node_id %in% igraph::V(graphs[["group2"]])$name

  return(list(target_nodes = target_nodes,
              drugs_to_target_nodes = drugs_to_target_nodes))
}



