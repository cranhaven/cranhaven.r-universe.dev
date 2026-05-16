target_edge_list <- function(graph, target_nodes, group) {
    #' @title [INTERNAL] Get edges adjacent to target nodes
    #'
    #' @description [INTERNAL] Based on the supplied graph and target nodes, this function returns a
    #' list of edges that are directly adjacent to target nodes. These edges can be used for further
    #' computation to compute the integrated interaction scores and differential scores in the networks.
    #'
    #' @param graph [igraph] Combined graph (iGraph graph object) for a specific group
    #' @param target_nodes [data.frame] Has column `node_id` (unique node IDs in the iGraph graph
    #' object that are targeted by drugs) and columns `groupA` and `groupB` (bool values specifying
    #' whether the node is contained in the combined graph of the group)
    #' @param group [string] Indicates which group `groupA` or `groupB` is analyzed
    #' 
    #' @return An edge list as a dataframe.
    #' @importFrom rlang .data
    #' 
    #' @keywords internal
    #' @noRd

    # filter for nodes contained in combined graph
    target_node_ids <- target_nodes[target_nodes[ , group], "node_id"]

    edge_list <- graph %>%
        igraph::as_data_frame('edges') %>%
        dplyr::filter(.data$from %in% target_node_ids | .data$to %in% target_node_ids)

    return(edge_list)
}



find_targets <- function(graphs, target_molecules, interaction_table, annotation, on) {
    #' @title [INTERNAL] Filter drug target nodes
    #'
    #' @description [INTERNAL] Based on the supplied target molecules, interaction table, graph, and
    #' annotation this function returns a dataframe containing nodes in the network targeted by a
    #' drug and a list containing the drug names as names and a vector of node IDs as keys.
    #'
    #' @param graphs [list] List of two iGraph graph objects (one for each group)
    #' @param target_molecules [string] Identifies the type of the target molecules (e.g.,
    #' `protein`). The string must be contained in the `type` column of the annotation dataframe.
    #' @param interaction_table [data.frame] Specifying the interaction of drugs and target molecules.
    #' Must contain a column `drug_name` containing drug names/identifiers and a column named like
    #' the character string given in the `on` argument, which must be an identifier for the targeted
    #' molecule.
    #' @param annotation [data.frame] Contains the annotation for all the nodes contained in the
    #' combined network. Must contain a column `node_id` (vertex IDs in the iGraph graph object) and a
    #' column named like the character string given in the `on` argument, which must be an identifier
    #' for the targeted molecule.
    #' @param on [string] Defines the ID that is used to match drugs to their targets. Both
    #' supplied dataframes (`annotation` and `interaction_table`) must contain a column named like
    #' this character string.
    #'
    #' @return A named list. Element `target_nodes` is a dataframe with column `node_id` (unique node
    #' IDs in the iGraph graph object that are targeted by drugs) and columns `groupA` and `groupB`
    #' (bool values specifying whether the node is contained in the combined graph of the group).
    #' Element `drugs_to_target_nodes` contains a named list: elements are `drug_names` and contain a
    #' vector of node IDs that are their specific targets.
    #' @importFrom rlang .data
    #' 
    #' @keywords internal
    #' @noRd

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
    target_nodes$groupA <- target_nodes$node_id %in% igraph::V(graphs[["groupA"]])$name

    if (is.null(graphs[["groupB"]])){target_nodes$groupB <- NULL}
    else {target_nodes$groupB <- target_nodes$node_id %in% igraph::V(graphs[["groupB"]])$name}

    return(list(target_nodes = target_nodes, drugs_to_target_nodes = drugs_to_target_nodes))
}



