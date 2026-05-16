write_interaction_score_input <- function(combined_graphs, drug_target_edgelists, saving_path) {
    #' @title [INTERNAL] Write edge lists and combined graphs to files
    #'
    #' @description [INTERNAL] Writes the combined graphs and the drug target edge lists to files for
    #' passing them to the python interaction score script.
    #' Graphs are saved as `gml` file. Edge lists are saved as `tsv` file.
    #'
    #' @param combined_graphs [list] A named list (elements `groupA` and `groupB`). Each element contains the
    #' entire combined network (layers + inter-layer connections) as iGraph graph object.
    #' @param drug_target_edgelists [list] A named list (elements `groupA` and `groupB`). Each element
    #' contains the list of edges to be considered in the interaction score calculation as dataframe (columns `from`, `to`, and `weight`)
    #' @param saving_path [string] Path to save intermediate output of DrDimont's functions.
    #' Default is current working directory.
    #'
    #' @return No return value, used internally
    #' 
    #' @keywords internal
    #' @noRd

    # iterate over groups
    for(group in c("groupA", "groupB")) {

        if (is.null(combined_graphs[[group]])){next}

        igraph::write_graph(combined_graphs[[group]], paste0(saving_path,"/", "combined_graph_", group, ".gml"), "gml")
        readr::write_tsv(drug_target_edgelists[[group]], paste0(saving_path,"/", "drug_target_edgelist_", group, ".tsv"))
    }
}


calculate_interaction_score <- function(max_path_length,
                                        total_edges,
                                        saving_path,
                                        conda=FALSE,
                                        script_path=NULL,
                                        num_cpus=1,
                                        int_score_mode="auto",
                                        cluster_address="auto",
                                        graphB_null=FALSE) {
    #' @title [INTERNAL] Calls a python script to calculate interaction score for combined graphs
    #'
    #' @description [INTERNAL] The interaction score is computed and saved in an additional `interaction_weight`
    #' edge attribute. This function expects the combined graphs for both groups along with their corresponding drug
    #' target and node lists to be saved at `saving_path`. Graphs and drug targets should be weighted edge lists
    #' in `gml` and `tsv` format, respectively. Node files should contain one node id per line.
    #' The script for calculating the interaction score is called with `python_executable`. An
    #' alternate script can be specified with `script_path`.
    #' The score for an edge is computed as the sum of the average product of weights along all simple
    #' paths of length l (over all path lengths up to `max_path_length`) between the source and target node of the edge.
    #'
    #' @param max_path_length [int] Integer of maximum length of simple paths to include in the
    #' \code{\link[DrDimont]{generate_interaction_score_graphs}} computation. (default: 3)
    #' @param total_edges Vector with total edges in each group
    #' @param saving_path [string] Path to save intermediate output of DrDimont's functions.
    #' Default is current working directory. Directory to use for writing intermediate data
    #' when passing input and  output between Python and R.
    #' @param conda [bool] Specifying if python is installed in a conda environment. Set TRUE if python is installed
    #' with conda, else python dependencies are assumed to be installed with pip. (default: FALSE)
    #' @param script_path [string] Path to the interaction score Python script. Set NULL to use package
    #' internal script (default).
    #' @param num_cpus [int] Number of CPUs to use for parallel computation. (default: 1)
    #' @param int_score_mode ["auto"|"sequential"|"ray"] Whether to compute interaction
    #' score in parallel using the Ray python library or sequentially. When `auto` it depends on the
    #' graph sizes. (default: "auto")
    #' @param cluster_address [string] Local node IP-address of Ray if executed on a cluster.
    #' On a cluster: Start ray with \code{ray start --head --num-cpus 32} on the console before DrDimont execution.
    #' It should work with "auto", if it does not specify IP-address given by the \code{ray start} command. (default: "auto")
    #' @param graphB_null [bool] Specifying if graphB of `groupB` is given (FALSE) or not (TRUE). (default: FALSE)
    #'
    #' @return Does not return anything, instead calls Python script which outputs `gml` files
    #' 
    #' @keywords internal
    #' @noRd

    py_script <- ifelse(is.null(script_path), system.file("python_igraph_interaction_score.py", package = "DrDimont"), script_path)
    graph_file_groupA <- paste0(saving_path, "/combined_graph_groupA.gml")
    graph_file_groupB <- paste0(saving_path, "/combined_graph_groupB.gml")
    edgelist_file_groupA <- paste0(saving_path, "/drug_target_edgelist_groupA.tsv")
    edgelist_file_groupB <- paste0(saving_path, "/drug_target_edgelist_groupB.tsv")
    group_is_large <- total_edges > 2000000
    use_ray <- ifelse(group_is_large, int_score_mode == "ray", !(int_score_mode == "sequential"))
    groups <- c("groupA", "groupB")

    for (i in 1:2) {
        group = groups[i]

        if (graphB_null && group=="groupB"){next}

        if (group_is_large[i]) {
            warning(stringr::str_interp("The graph for ${group} has more than 2 million edges. The computation will be slow. Consider reducing the network further."))
        }
        if (int_score_mode == "auto" && !use_ray[i]) {
            warning(stringr::str_interp("Interaction score calculation is run sequentially to avoid extensive memory use. To force parallel computation use `DrDimont::drdimont_settings(int_score_mode='ray')`"))
        }
    }

    if (conda) {
        python_executable=reticulate::conda_python(envname="r-DrDimont")
    } else {
        python_executable=reticulate::virtualenv_python(envname="r-DrDimont")
    }
    res <- system2(python_executable, args = c(py_script,
                                               graph_file_groupA,
                                               edgelist_file_groupA,
                                               max_path_length,
                                               "--output", paste0(saving_path, "/int_score_graph_groupA.gml"),
                                               "--num_cpus", num_cpus,
                                               "--cluster_address", cluster_address,
                                               ifelse(use_ray[1], "--distributed", "")))
    if (res == 0) {message(format(Sys.time(), "[%y-%m-%d %X] "), "Computation of interaction scores for groupA was successful!")}


    if (!graphB_null){
        res <- system2(python_executable, args = c(py_script,
                                                   graph_file_groupB,
                                                   edgelist_file_groupB,
                                                   max_path_length,
                                                   "--output", paste0(saving_path, "/int_score_graph_groupB.gml"),
                                                   "--num_cpus", num_cpus,
                                                   "--cluster_address", cluster_address,
                                                   ifelse(use_ray[2], "--distributed", "")
        ))
        if (res == 0) {message(format(Sys.time(), "[%y-%m-%d %X] "), "Computation of interaction scores for groupB was successful!")}
    } else {message(format(Sys.time(), "[%y-%m-%d %X] "), "Skipping computation of interaction scores for groupB!")}

}


load_interaction_score_output <- function(saving_path, graphB_null) {
    #' @title [INTERNAL] Loads output of python script for interaction score calculation
    #'
    #' @description [INTERNAL] Loads data generated by \code{\link[DrDimont]{calculate_interaction_score}}. Python output files are
    #' graphs in `gml` format for each of both groups.
    #'
    #' @param saving_path [string] Path to save intermediate output of DrDimont's functions. Default is current working directory.
    #' Directory to use for writing intermediate data when passing input and output between Python and R.
    #' Directory to load python output from
    #' @param graphB_null [bool] Specifying if graphB of `groupB` is given (FALSE) or not (TRUE). (default: FALSE)
    #'
    #' @return A named list (elements `groupA` and `groupB`). Each element contains an iGraph object
    #' containing the interaction score as edge attribute.
    #' 
    #' @keywords internal
    #' @noRd

    graphs <- list()

    for(group in c("groupA", "groupB")) {
        if (graphB_null && group == "groupB"){
            graphs[[group]] <- NULL
            message(format(Sys.time(), "[%y-%m-%d %X] "), group, "skipped. ")
            next
        }
        graphs[[group]] <- igraph::read.graph(paste0(saving_path, "/int_score_graph_", group, ".gml"), format = "gml")
        gc()
        message(format(Sys.time(), "[%y-%m-%d %X] "), group," done.")
    }
    return(graphs)
}
