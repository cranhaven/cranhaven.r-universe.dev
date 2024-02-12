write_interaction_score_input <- function(combined_graphs, drug_target_edgelists, saving_path) {
  #' Write edge lists and combined graphs to files
  #'
  #' @description (INTERNAL) Writes the combined graphs and the drug target edge lists to files for
  #' passing them to the python interaction score script.
  #' Graphs are saved as `gml` file. Edgelists are saved as `tsv` file.
  #'
  #' @param combined_graphs A named list (elements `group1` and `group2`). Each element contains the
  #'  entire combined network (layers + inter-layer connections) as iGraph graph object.
  #' @param drug_target_edgelists A named list (elements `group1` and `group2`). Each element
  #' contains the list of edges to be considered in the interaction score calculation as data frame (columns `from`, `to` and `weight`)
  #' @param saving_path Directory to write to
  #' @return Does not return value, but writes to .tsv.
  #'
  #' @export
  #'
  groups <- c("group1", "group2")
  # iterate over groups
  for(group in groups) {
    igraph::write.graph(combined_graphs[[group]], paste0(saving_path,"/", "combined_graph_", group, ".gml"),"gml")
    readr::write_tsv(drug_target_edgelists[[group]], paste0(saving_path,"/", "drug_target_edgelist_", group, ".tsv"))
  }
}


calculate_interaction_score <- function(max_path_length, total_edges, loading_path, python_executable = "python3", script_path = NULL, int_score_mode="auto") {
  #' Calls a python script to calculate interaction score for combined graphs
  #'
  #' (INTERNAL) The interaction score is computed and replaces the edge weight.
  #' This function expects the combined graphs for both groups along with their corresponding drug
  #' target and node lists to be
  #' present at `loading_path`. Graphs and drug targets should be weighted edge lists in tsv format.
  #'  Node files should contain one node id per line.
  #' The script for calculating the interaction score is called with `python_executable`. An
  #' alternate script can be specified with `script_path`.
  #' The score for an edge is computed as the sum of the average product of weights along all simple
  #'  paths of length l (over all path lengths
  #' up to `max_path_length`) between the source and target node of the edge.
  #'
  #' @param max_path_length The maximum length of simple paths to consider when computing the
  #' interaction score
  #' @param total_edges vector with total edges in each group
  #' @param loading_path Directory to use for writing intermediate data when passing input and
  #' output between Python and R
  #' @param python_executable Python command or path to Python executable to use
  #' @param script_path Path to the interaction score Python script. Set NULL to use package
  #'internal script (default).
  #' @param int_score_mode One of `auto`, `sequential` or `ray`. Whether to compute interaction
  #' score in parallel using the Ray python library or sequentially. When `auto` it depends on the
  #' graph sizes.
  #' @return Does not return anything, instead calls Python script which outputs .gml files
  #' @export
  #'
  py_script <- ifelse(is.null(script_path), system.file("python_igraph_interaction_score.py", package = "molnet"), script_path)
  graph_file_group1 <- paste0(loading_path, '/combined_graph_group1.gml')
  graph_file_group2 <- paste0(loading_path, '/combined_graph_group2.gml')
  edgelist_file_group1 <- paste0(loading_path, '/drug_target_edgelist_group1.tsv')
  edgelist_file_group2 <- paste0(loading_path, '/drug_target_edgelist_group2.tsv')
  group_is_large <- total_edges > 2000000
  use_ray <- ifelse(group_is_large, int_score_mode == "ray", !(int_score_mode == "sequential"))

  for (i in 1:2) {
    if (group_is_large[i]) {
      warning(stringr::str_interp("The graph for group ${i} has more than 2 million edges. The computation will be slow. Consider reducing the network further."))
    }
    if (int_score_mode == "auto" && !use_ray[i]) {
      warning(stringr::str_interp("Interaction score calculation is run sequentially to avoid extensive memory use. To force parallel computation use `molnet::molnet_settings(int_score_mode='ray')`"))
    }
  }

  system2(python_executable, args = c(py_script,
                                      graph_file_group1,
                                      edgelist_file_group1,
                                      max_path_length,
                                      '--output', paste0(loading_path, '/int_score_graph_group1.gml'),
                                      ifelse(use_ray[1], "--distributed", "")
                                      )
          )
  system2(python_executable, args = c(py_script,
                                      graph_file_group2,
                                      edgelist_file_group2,
                                      max_path_length,
                                      '--output', paste0(loading_path, '/int_score_graph_group2.gml'),
                                      ifelse(use_ray[2], "--distributed", "")
                                      )
          )
}


load_interaction_score_output <- function(loading_path) {
  #' Loads output of python script for interaction score calculation
  #'
  #' (INTERNAL) Loads data generated by \code{\link{calculate_interaction_score}}. Output files are
  #' graphs in gml format for both groups.
  #'
  #' @param loading_path Directory to load from
  #'
  #' @return A named list (elements `group1` and `group2`). Each element contains an iGraph object
  #' containing the interaction score as edge attribute.
  #' @export
  #'
  groups <- c("group1", "group2")
  graphs <- list()

  for(group in groups) {
    message(group, "...")
    graphs[[group]] <- igraph::read.graph(paste0(loading_path, '/int_score_graph_', group, '.gml'), format = 'gml')
    gc()
    message("done. ")
  }
  return(graphs)
}
