get_layer <- function(name, layers) {
    #' @title [INTERNAL] Fetch layer by name from layer object
    #' 
    #' @description [INTERNAL] Get a layer by its name from a layer object 
    #' created with \code{\link[DrDimont]{make_layer}}, e.g., \code{\link{layers_example}}.
    #'
    #' @param name The layer to fetch
    #' @param layers A layers object \code{\link{layers_example}}
    #' 
    #' @return Returns the layer along with layer names
    #' 
    #' @keywords internal
    #' @noRd
    
    layer_names <- sapply(layers, function(l) l[['name']])
    return(layers[[which(layer_names == name)]])
}

create_unique_layer_node_ids <- function(identifiersA, identifiersB, layer_name) {
    #' @title [INTERNAL] Assign node IDs to the biological identifiers across a graph layer
    #'
    #' @description [INTERNAL] This function takes two dataframes of (biological) identifiers of nodes.
    #' Each dataframe corresponds to the identifiers of the components contained in the single-layer
    #' network of a sample group. This function outputs the same dataframes, with an added column (`node_id`)
    #' that contains node IDs which can later be used as `name` parameter for an iGraph graph.
    #' Node IDs begin with the defined `prefix` and an underscore. If a molecule is present in both
    #' groups, the node ID will be the same across the whole layer, allowing to easily combine the
    #' graphs of both groups in \code{\link[DrDimont]{generate_differential_score_graph}} to calculate
    #' differential scores of identical nodes in both sample groups.
    #' The function is used by the high-level wrapper \link[DrDimont]{generate_individual_graphs} to
    #' create annotations, which uniquely define nodes across the network layer.
    #'
    #' @param identifiersA [data.frame] Containing the biological identifiers of 
    #' group A of the same network layer.
    #' @param identifiersB [data.frame] Containing the biological identifiers of 
    #' group B of the same network layer.
    #' @param layer_name [string] Name of layer that the node ids are created for
    #'
    #' @return Returns an named list. Elements `groupA` and `groupB` contain the input
    #' dataframes with an additional column `node_id`. `both` contains all unique node IDs assigned
    #' across the network layer.
    #' 
    #' @keywords internal
    #' @noRd

    ##### check if only 1 group given
    if (is.null(identifiersB)){

        if (sum(duplicated(identifiersA)) > 0){
            ##### show warning if duplicated node ids given
            message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("WARNING: Duplicate node IDs given in layer ${layer_name}, this may cause ERRORS!"))
        }

        assigned_ids <- dplyr::union(identifiersA, identifiersA) %>%
            dplyr::mutate(node_id=paste0(layer_name, "_", dplyr::row_number()), layer=layer_name)

        identifiersA <- suppressMessages(dplyr::left_join(identifiersA, assigned_ids))

        return(list(groupA=identifiersA, groupB=NULL, both=assigned_ids))

    } else {

        if((sum(duplicated(identifiersA)) > 0)|(sum(duplicated(identifiersB)) > 0)){
            ##### show warning if duplicated node ids given
            message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("WARNING: Duplicate node IDs given in layer ${layer_name}, this may cause ERRORS!"))
        }
        assigned_ids <- dplyr::union(identifiersA, identifiersB) %>%
            dplyr::mutate(node_id=paste0(layer_name, "_", dplyr::row_number()), layer=layer_name)

        identifiersA <- suppressMessages(dplyr::left_join(identifiersA, assigned_ids))
        identifiersB <- suppressMessages(dplyr::left_join(identifiersB, assigned_ids))

        return(list(groupA=identifiersA, groupB=identifiersB, both=assigned_ids))
    }
}


graph_metrics <- function(graph, verbose = TRUE, return = FALSE) {
    #' @title [INTERNAL] Analysis of metrics of an iGraph object
    #'
    #' @description [INTERNAL] This helper function prints or returns multiple metrics of arbitrary
    #' iGraph graph object.
    #'
    #' @param graph [igraph] iGraph object to analyze.
    #' @param verbose [bool] If TRUE, graph information is printed.
    #' @param return [bool] If TRUE, graph information is returned.
    #'
    #' @return Named list of metrics including vertex count, edge count, number of components,
    #' size of largest component and the relative frequency of zero degree vertices.
    #' 
    #' @keywords internal
    #' @noRd

    metrics <- list('n_vertices' = igraph::gorder(graph),
                    'n_edges' = igraph::ecount(graph),
                    'components' = igraph::components(graph),
                    'degree_distribution' = igraph::degree_distribution(graph)
                    )
    ### print graph metrics
    if(verbose == TRUE) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Vertex count: ', metrics$n_vertices)
        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Edge count: ', metrics$n_edges)
        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Number of components: ', metrics$components$no)
        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Size of largest component: ', max(metrics$components$csize))
        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Relative frequency of zero degree vertices: ', metrics$degree_distribution[1])
    }

    if(return == TRUE) {return(metrics)}
}


set_cluster <- function(n_threads) {
    #' @title [INTERNAL] Create and register cluster
    #'
    #' @description [INTERNAL] Deprecated! This function will be removed in future versions. Helper function to create and register a cluster for parallel
    #' computation of p-value reduction
    #'
    #' @param n_threads [int] Number of nodes in the cluster
    #'
    #' @return No return value, called internally to create cluster
    #' 
    #' @keywords internal
    #' @noRd
    
    parallel::setDefaultCluster(parallel::makeCluster(n_threads))
}


shutdown_cluster <- function() {
    #' @title [INTERNAL] Shutdown cluster and remove corresponding connections
    #'
    #' @description [INTERNAL] Deprecated! This function will be removed in future versions. Run this if the pipeline fails during parallel 
    #' computation to clean the state. If a cluster is registered, this function 
    #' stops it and removes corresponding connections. Ignores errors. Has no effect 
    #' if no cluster is registered.
    #' 
    #' @return No return value, called internally to shutdown cluster
    #' 
    #' @keywords internal
    #' @noRd
    
    try(parallel::stopCluster(parallel::getDefaultCluster()), silent = TRUE)
    try(closeAllConnections(), silent = TRUE)
}
