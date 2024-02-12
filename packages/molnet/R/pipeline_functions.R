make_layer <- function(name, data_group1, data_group2, identifier_group1, identifier_group2) {
  #' Creates individual molecular layers from raw data and unique identifiers
  #'
  #' Helper function to transform input data to required pipeline input format. Additionally, the
  #' supplied input is checked. Allows easy conversion of raw data into the structure accepted by
  #' \code{\link{start_pipeline}}.
  #'
  #' @param name Character string. Names the layer.
  #' @param data_group1,data_group2 Data frame containing raw molecular data of each group
  #' (each stratum). Analyzed components (e.g., genes) in columns, samples (e.g. patients) in rows.
  #' @param identifier_group1,identifier_group2 Data frame containing component identifiers
  #' (columns) of each component (rows) in the same order as the molecular data frame of each group.
  #'  These identifiers are used to (a) interconnect graphs and (b) match drugs to drug targets.
  #'  Must contain a column `type` which identifies the nature of the component (e.g., "protein")
  #'
  #' @return Named list containing the supplied data for each group (i.e., the dataset for one
  #' layer), that can be supplied to \code{\link{start_pipeline}} and `name` giving the name of the
  #' layer. Each sublist contains the `data` and the `identifiers`.
  #' @export
  #'
  #' @examples
  #' data(mrna_data)
  #'
  #' mrna_layer <- make_layer(name="mrna", data_group1=mrna_data$group1$data,
  #' data_group2=mrna_data$group2$data,
  #' identifier_group1=data.frame(gene_name=mrna_data$group1$identifiers),
  #' identifier_group2=data.frame(gene_name=mrna_data$group2$identifiers))
  #'

  layer <- list(group1 = list(data = data.frame(data_group1),
                              identifiers = data.frame(identifier_group1)),
                group2 = list(data = data.frame(data_group2),
                              identifiers = data.frame(identifier_group2)),
                name = name)
  return_errors(check_layer(layer))
  return(layer)
}


make_connection <- function(from, to, connect_on, weight = 1, group = "both") {
  #' Specify connection between two individual layers
  #'
  #' Helper function to transform input data to a required pipeline input format. This helper
  #' function creates a list that specifies the connection between two layers.
  #'
  #' The connection can be based on IDs present in the identifiers of both layer or an interaction
  #' table containing mapping the connections and edge weights.
  #' Additionally, the supplied input is checked. Allows easy conversion of raw data into the
  #' structure accepted by \code{\link{start_pipeline}}.
  #'
  #' __IMPORTANT:__ if a connection is established based on \code{id} this ID has to be present in
  #' the identifiers of both layers, have to be named identically and IDs have to be formatted
  #' identically as these are matched by an inner join operation (refer to \code{\link{make_layer}}).
  #'
  #' @param from Character string referring to the name of the layer **from** which the connection
  #' should be established
  #' @param to Character string referring to the name of the layer **to** which the connection
  #' should be established
  #' @param connect_on Specifies how the two layers should be connected. This can be based on a
  #' mutual ID or a table specifying interactions:
  #' * __Mutual ID__: Character string specifying the name of an identifier that is present in
  #' both layers (e.g., `NCBI ID` to connect proteins and mRNA).
  #' * __Interaction table__: A table mapping two identifiers of two layers. The columns have
  #' exactly the same names as the identifiers of the layers. Has to contain an additional column
  #' specifying the weight between two components/nodes (see `weight` argument)
  #' @param weight Specifies the edge weight between the layers. This can be supplied as a number
  #' applied to every connection or a column of the interaction table:
  #' * __Fixed weight__: number specifying the weight of every connection between the layers.
  #' * __Based on interaction table__: Character string specifying the name of a column in the
  #' table passed as the `by` parameter which is used as edge weight.
  #' @param group Group for which to apply the connection. One of `both`, `1` or `2`.
  #' @return A named list (i.e., an inter-layer connection), that can be supplied to
  #' \code{\link{start_pipeline}}.
  #' @export
  #'
  #' @examples
  #' data(metabolite_protein_interaction)
  #' inter_layer_connections = list(
  #' make_connection(from = 'mrna', to = 'protein', connect_on = 'gene_name'),
  #' make_connection(from = 'protein', to = 'phosphoprotein', connect_on = 'gene_name'),
  #' make_connection(from = 'protein', to = 'metabolite',
  #' connect_on = metabolite_protein_interaction,
  #' weight = 'combined_score'))
  #'
  con <- list(from = from, to = to, by = "", connect_on = connect_on, weight = weight, group = group)
  if (is.character(connect_on) & is.vector(connect_on) & length(connect_on) == 1) {
    con$by <- "id"
  }
  else {
    con$by <- "table"
  }
  return_errors(check_connection(con))
  return(con)
}

make_drug_target <- function(target_molecules, interaction_table, match_on) {
  #' @title Reformat drug-target-interaction data
  #'
  #' @description Function to transform input data to required input format for
  #' \code{\link{start_pipeline}}. Here the data needed to define drug-target interactions is
  #' formatted. When the reformatted output is passed to \code{\link{start_pipeline}} as
  #' \code{drug_target_interaction} argument, the differential drug response score will be
  #' calculated for all the supplied drugs in \code{interaction_table}.
  #'
  #' @param target_molecules Name of layer containing the drug targets. This name has to match the
  #' corresponding named item in the list of layers supplied to \code{\link{start_pipeline}}.
  #' @param interaction_table Data frame. Has to contain two columns. Additional columns will be
  #' ignored in the pipeline.
  #' * A column called `drug_name` containing names or identifiers of drugs
  #' * A column with a name that matches an identifier in the layer supplied in `target_molecules`.
  #' For example, if drugs target proteins and an identifier called `ncbi_id` was supplied in layer
  #' building of the protein layer (\code{\link{make_layer}}), this column should be called
  #' `ncbi_id` and contain the corresponding IDs of protein-drug targets. Any other ID present in
  #' the constructed layer can be used.
  #' @param match_on Column name of the data frame supplied in `interaction_table` that is used for
  #' matching drugs and target nodes in the graph (e.g. `ncbi_id`).
  #'
  #' @return Named list of the input parameters in input format of \code{\link{start_pipeline}}.
  #' @export
  #'
  #' @examples
  #' data(drug_gene_interactions)
  #' drug_target_interaction <- make_drug_target(target_molecules='protein',
  #' interaction_table=drug_gene_interactions, match_on='gene_name')
  #'
  res <- list(target_molecules = target_molecules,
              interaction_table = interaction_table,
              match_on = match_on)
  return_errors(check_drug_target(res))
  return(res)
}



start_pipeline <- function(layers, inter_layer_connections, drug_target_interaction, settings) {
  #' Execute all molnet-pipeline steps sequentially
  #'
  #' This wrapper function executes all necessary steps to generate differential drug response
  #' scores from the formatted input data. The following input data is required
  #' (and detailed below):
  #' * Layers of stratified molecular data.
  #' * Additional connections between the layers.
  #' * Interactions between drugs and nodes in the network.
  #' * Settings for pipeline execution.
  #'
  #' As this function runs through all steps of the molnet-pipeline it can take a long to complete,
  #' especially if the supplied molecular data is in large dimensions. Several prompts will be
  #' printed to supply information on how the pipeline is proceeding. Calculation of the interaction
  #' score by \code{\link{interaction_score}} requires saving large-scale graphs to file and calls
  #' a python script. This handover may take time.
  #'
  #' Eventually a data frame is returned containing the supplied drug name and its associated
  #' differential drug response score computed by molnet.
  #'
  #' @param layers Named list with different network layers containing data and identifiers for
  #'both groups. The required input format is a list with names corresponding to the content of
  #'the respective layer (e.g., "protein"). Each named element has to contain the molecular data
  #'and corresponding identifiers formatted by  \code{\link{make_layer}}.
  #' @param inter_layer_connections A list with specified inter-layer connections. This list
  #' contains one or more elements defining individual inter-layer connections created by
  #' \code{\link{make_connection}}.
  #' @param drug_target_interaction A list specifying drug-target interactions for drug response
  #' score computation. The required input format of this list is created by
  #' \code{\link{make_drug_target}}. The drug response score is calculated for all drugs contained
  #' in this object.
  #' @param settings A named list containing pipeline settings. The settings list has to be
  #'initialized by \code{\link{molnet_settings}}. Items in the named list can be adjusted as desired.
  #' @return Data frame containing drug name and associated differential drug response score. If no
  #' target is found for a specific drug, \code{NA} is returned as a score. If Python is not installed
  #' or the interaction score computation fails for some other reason, NULL is returned instead.
  #' @export
  #' @examples
  #' \dontshow{
  #' WGCNA::disableWGCNAThreads()
  #' }
  #' data(drug_gene_interactions)
  #' data(layers_example)
  #' inter_layer_connections = list(make_connection(from = 'mrna',
  #' to = 'protein',
  #' connect_on = 'gene_name'))
  #' drug_target_interaction <- make_drug_target(target_molecules='protein',
  #' interaction_table=drug_gene_interactions,
  #' match_on='gene_name')
  #' settings <- molnet_settings(handling_missing_data = list(default =
  #' "pairwise.complete.obs",mrna = "all.obs"),
  #' save_individual_graphs = FALSE,
  #' save_combined_graphs = FALSE,
  #' save_drug_targets = FALSE,
  #' python_executable = "python3")
  #'
  #' \donttest{
  #' start_pipeline(layers_example, inter_layer_connections, drug_target_interaction, settings)
  #' }
  #'
  message("### Pipeline started ###\nValidating input...\n")
  return_errors(check_input(layers, inter_layer_connections, drug_target_interaction))
  message("done.\n")
  if (any(settings$reduction_method_layers == "p_value") &&
      settings$n_threads > 1 &&
      is.null(parallel::getDefaultCluster())) {
    set_cluster(settings$n_threads)
  }
  message("### STEP 1: Generating individual graphs ###\n")
  individual_graphs <- generate_individual_graphs(layers, settings)
  if (settings$save_individual_graphs) {
    message("Saving individual graphs...")
    save(individual_graphs, file=paste0(settings$saving_path, "/individual_graphs.rda"))
    message("done.\n")
  }
  message("### STEP 2: Combining graphs ###\n")
  combined_graphs <- generate_combined_graphs(individual_graphs[["graphs"]],
                                              individual_graphs[["annotations"]],
                                              inter_layer_connections,
                                              settings)
  if (settings$save_combined_graphs) {
    message("Saving combined graphs...")
    save(combined_graphs, file=paste0(settings$saving_path, "/combined_graphs.rda"))
    message("done.\n")
  }
  message("### STEP 3: Filtering for drug targets ###\n")
  drug_targets <- determine_drug_targets(combined_graphs[["graphs"]],
                                         combined_graphs[["annotations"]],
                                         drug_target_interaction,
                                         settings)
  if (settings$save_drug_targets) {
    message("Saving drug targets...")
    save(drug_targets, file=paste0(settings$saving_path, "/drug_targets.rda"))
    message("done.\n")
  }
  message("Drug targets found.\n")
  message("### STEP 4: Calculating interaction score ###\n")
  interaction_score_graphs <- interaction_score(combined_graphs[["graphs"]],
                                                drug_targets[["edgelist"]],
                                                settings)
  if (is.null(interaction_score_graphs)) {
    message("Interaction core could not be computed. Maybe Python is not installed. Returning from pipeline early.")
    return(NULL)
  }
  message("### STEP 5: Calculating differential score ###\n")
  differential_score_graph <- differential_score(interaction_score_graphs)
  message("### STEP 6: Calculating drug response score ###\n")
  drug_response_score <- get_drug_response_score(differential_score_graph,
                                                 drug_targets[["targets"]],
                                                 drug_target_interaction$interaction_table)
  message("### Pipeline complete. ###\n")
  if (!is.null(parallel::getDefaultCluster())) {
    shutdown_cluster()
  }
  return(drug_response_score)

}


generate_individual_graphs <- function(layers, settings) {
  #' Builds graphs from specified network layers
  #'
  #' Constructs and returns two graphs for each network layer, where nodes
  #' correspond to the rows in the measurement data.
  #' Graphs are initially complete and edges are weighted by correlation of
  #' measurements across columns. The number of edges is then reduced by either a
  #' threshold on the p-value of the correlation or a minimum scale-free fit
  #' index.
  #' Each node is mapped to the biological
  #' identifiers given in the layer and the mapping table is returned as
  #' `annotations`.
  #'
  #' @param layers Named list with different network layers containing data and identifiers for both
  #'  groups (generated from \code{\link{make_layer}})
  #' @param settings A named list containing pipeline settings
  #'
  #' @return A nested named list with first-level elements `graphs` and `annotations`. The second
  #' level elements are `group1` and `group2`. These contain a list of iGraph objects (`graphs`)
  #' and data frames (`annotations`) mapping the graph node IDs to biological identifiers.
  #' @export
  #'
  #' @examples
  #' \dontshow{
  #' WGCNA::disableWGCNAThreads()
  #' }
  #' data(layers_example)
  #' layers <- layers_example
  #' settings <- molnet::molnet_settings(handling_missing_data="pairwise.complete.obs")
  #' individual_graphs <- molnet::generate_individual_graphs(layers, settings)
  #' molnet::graph_metrics(individual_graphs$graphs$group1$mrna)
  #' molnet::graph_metrics(individual_graphs$graphs$group2$mrna)
  #'
  groups <- c('group1', 'group2')

  # empty list to store igraph objects of individual layers and inter-layer connections
  graphs <- list()

  # empty list to store annotation data frames
  annotations <- list()

  # iterate over layers
  for(n_layer in c(1:length(layers))) {
    layer <- layers[[n_layer]][['name']]

    # create annotations which uniquely define nodes across the network layer
    layer_annotation <- create_unique_layer_node_ids(get_layer(layer, layers)[['group1']][['identifiers']],
                                                     get_layer(layer, layers)[['group2']][['identifiers']],
                                                     layer_name = layer)
    annotations[['group1']][[layer]] <- layer_annotation$group1
    annotations[['group2']][[layer]] <- layer_annotation$group2
    annotations[['all']][[layer]] <- layer_annotation$all

    handling_missing_data = get_layer_setting(layer, settings, "handling_missing_data")
    reduction_method = get_layer_setting(layer, settings, "reduction_method")

    for(group in groups) {
      message("Generating graph of layer ", layer, " for ", group, "\n")

      if (!is.null(settings$save_correlation_filename)) {
        save_correlation_filename = paste0(settings$saving_path, "/", settings$save_correlation_filename, "_", layer, ".rds")
      } else {
        save_correlation_filename = settings$save_correlation_filename
      }

      # generate graph for a specific layer with the data of a specific group
      layer_graph <- generate_reduced_graph(measurement_data = get_layer(layer, layers)[[group]][['data']],
                                            identifiers = annotations[[group]][[layer]],
                                            correlation_method = settings$correlation_method,
                                            reduction_method = reduction_method,
                                            save_correlation_filename = save_correlation_filename,
                                            handling_missing_data = handling_missing_data,
                                            p_value_adjustment_method = settings$p_value_adjust_method,
                                            reduction_alpha = settings$reduction_alpha,
                                            r_squared_cutoff = settings$r_squared_cutoff,
                                            cut_vector = settings$cut_vector,
                                            print_graph_info = settings$print_graph_info,
                                            n_threads = settings$n_threads,
                                            parallel_chunk_size = settings$parallel_chunk_size)


      # save graph in 'graphs' as named list
      graphs[[group]][[layer]] <- layer_graph

    }
  }
  return(list(graphs = graphs,
              annotations = annotations))
}


generate_combined_graphs <- function(graphs, annotations, inter_layer_connections, settings) {
  #' @title Combines individual layers to a single graph
  #' @description Individual graphs created by \code{\link{generate_individual_graphs}}
  #' are combined to a single graph per group according to
  #' `inter_layer_connections`. Returns a list of combined graphs
  #' along with their annotations.
  #'
  #' @param graphs A named list (elements `group1` and `group2`). Each element contains a list of
  #' iGraph objects (output of \code{\link{generate_individual_graphs}}).
  #' @param annotations A named list (elements `group1` and `group2`). Each element contains a
  #' list of data frames mapping each node IDs to identifiers (output of \code{\link{generate_individual_graphs}}).
  #' @param inter_layer_connections Named list with specified inter-layer connections. Names are
  #' layer names and elements are connections (\link{make_connection}).
  #' @param settings A named list containing pipeline settings
  #'
  #' @return A named list (elements `graphs` and `annotations` and sub-elements `$group1` and
  #' `$group2`). Contains the igraph objects of the combined network and their annotations for both
  #' groups.
  #' @export
  #'
  #' @examples
  #' \dontshow{
  #' WGCNA::disableWGCNAThreads()
  #' }
  #' data(individual_graphs_example)
  #' individual_graphs <- individual_graphs_example
  #' inter_layer_connections <- list(molnet::make_connection(from="mrna",
  #' to="protein", connect_on="gene_name", weight=1))
  #' settings <- molnet::molnet_settings() # defaults
  #' combined_graphs <- molnet::generate_combined_graphs(individual_graphs$graphs,
  #'                                                     individual_graphs$annotations,
  #'                                                     inter_layer_connections,
  #'                                                     settings)
  #'
  inter_layer_edges <- list()
  # iterate over specified connections
  for(n_connection in c(1:length(inter_layer_connections))) {
    connection <- inter_layer_connections[[n_connection]]
    message(stringr::str_interp("Connecting ${connection$from} and ${connection$to} "))
    if (connection$group == "both") {
      groups <- c("group1", "group2")
    } else {
      groups <- paste0("group", as.character(connection$group))
    }
    # connections can be established by shared IDs ('id') between networks or by a given interaction
    table ('table')
    if(connection$by == 'id') {
      message("by id.\n")
      # iterate over groups
      for(group in groups) {
        message(stringr::str_interp("${group}: "))

        layer1 <- connection$from
        layer2 <- connection$to
        id <- connection$connect_on
        weight <- connection$weight

        # generate inter-layer graph
        message("generating inter-layer edgelist...")
        inter_layer_edgelist <- inter_layer_edgelist_by_id(annotations[[group]][[layer1]],
                                                        annotations[[group]][[layer2]],
                                                        connection = id,
                                                        weight = weight)
        message("done.\n")
        # save inter-layer graph to list of graph objects
        inter_layer_edges[[group]] <- append(inter_layer_edges[[group]], list(inter_layer_edgelist))
      }
    } else if(connection$by == 'table') {
      message("by table.\n")
      # iterate over groups
      for(group in groups) {
        message(stringr::str_interp("${group}: "))

        layer1 <- connection$from
        layer2 <- connection$to
        interaction_table <- connection$connect_on
        weight_column <- connection$weight
        message("generating inter-layer edgelist...")
        inter_layer_edgelist <- inter_layer_edgelist_by_table(annotations[[group]][[layer1]],
                                                           annotations[[group]][[layer2]],
                                                           interaction_table = interaction_table,
                                                           weight_column = weight_column)
        message("done.\n")
        # save inter-layer graph to list of graph objects
        inter_layer_edges[[group]] <- append(inter_layer_edges[[group]], list(inter_layer_edgelist))
      }
    }
  }

  # empty list to store combined graphs for both groups
  message("Combining graphs.\n")
  combined_graphs <- list()
  combined_annotations <- list()

  # iterate over groups
  for(group in c("group1", "group2")) {
    message(stringr::str_interp("${group}: "))
    combined_graphs[[group]] = combine_graphs(graphs[[group]], inter_layer_edges[[group]])
    combined_annotations[[group]] <- dplyr::bind_rows(annotations[[group]])
    message("done.\n")
  }
  combined_annotations[['all']] <- dplyr::bind_rows(annotations[['all']])
  return(list(graphs = combined_graphs,
              annotations = combined_annotations))
}


determine_drug_targets <- function(graphs, annotations, drug_target_interaction, settings) {
  #' Determine drug target nodes in network
  #'
  #' @description Finds node IDs of network nodes in `graphs` that are targeted by a drug in
  #' `drug_target_interaction`. Returns list of node ids and list of adjacent edges.
  #'
  #' @param graphs A named list with elements `group1` and `group2` containing the combined graphs
  #' of each group as iGraph object.
  #' @param annotations List of data frames that map node IDs to identifiers. Contains `all`
  #' (unique identifiers across the whole data) and `group1` and `group2` containing identifiers
  #' specific to the strata.
  #' @param drug_target_interaction Named list specifying drug target interactions for drug response
  #'  score computation
  #' @param settings A named list containing pipeline settings
  #'
  #' @return A named list with elements `drug_targets` and `drug_target_edge_list`.
  #' * `targets` is a named list with elements `target_nodes` and `drugs_to_target_nodes`.
  #' `target_nodes` is a data frame with column `node_id` (unique node IDs in the iGraph object
  #' targeted by drugs) and columns `group1` and `group2` (boolean values specifying whether the
  #' node is contained in the combined graph of the group). Element `drugs_to_target_nodes` contains
  #'  a named list mapping drug names to a vector of their target node IDs.
  #' * `drug_target_edge_list` contains elements `group1` and `group2` containing each a list of
  #' edges adjacent to drug target nodes.
  #' @export
  #'
  #' @examples
  #' data(drug_gene_interactions)
  #' data(combined_graphs_example)
  #' combined_graphs <- combined_graphs_example
  #' settings <- molnet_settings()
  #' drug_target_interaction <- make_drug_target(target_molecules='protein',
  #' interaction_table=drug_gene_interactions,
  #' match_on='gene_name')
  #' drug_targets <- determine_drug_targets(combined_graphs[["graphs"]],
  #' combined_graphs[["annotations"]],
  #' drug_target_interaction,
  #' settings)
  #'
  groups <- c("group1", "group2")
  drug_target_edge_list <- list()
  drug_targets <- list()

  drug_targets <- find_targets(graphs = graphs,
                               target_molecules = drug_target_interaction$target_molecules,
                               interaction_table = drug_target_interaction$interaction_table,
                               annotation = annotations[['all']],
                               on = drug_target_interaction$match_on)

  # iterate over groups
  for(group in groups) {

    drug_target_edge_list[[group]] = target_edge_list(graphs[[group]],
                                                      drug_targets$target_nodes,
                                                      group = group)
  }
  return(list(targets = drug_targets,
              edgelist = drug_target_edge_list))
}

interaction_score <- function(graphs, drug_target_edgelists, settings) {
  #' @title Computes interaction score for combined graphs
  #' @description  Writes the input data (combined graphs for both groups in gml format and
  #' lists of edges adjacent to drug targets for both groups) to files and calls
  #' a python script for calculating the score.
  #' Output files written by the python script are two graphs in gml format containing the
  #' interaction score as weight.
  #' These are loaded and returned in a named list.
  #' ATTENTION: Data exchange via files is mandatory and takes a long for large data. Interaction
  #' score computation is expensive and slow because it involves finding all
  #' simple paths up to a certain length between source and target node of the
  #' drug target edges. Don't set `max_path_length` in settings to a large value and only consider
  #' this step if your graphs have up to approximately 2 million edges.
  #' Computation is initiated by \code{\link{calculate_interaction_score}}. The
  #' python script is parallelized using Ray. Use the setting `int_score_mode` to
  #' force sequential or parallel computation. Refer to the Ray documentation
  #' if you encounter problems with running the python script in parallel.
  #' DISCLAIMER: Depending on the operating system Python comes pre-installed or has to be installed
  #'  manually.
  #' Please pay attention to the version and the executable used (python/python3 or homebrew
  #' python). You can use the `python_executable` setting to specify the command or path.
  #'
  #' @param graphs A named list (elements `group1` and `group2`). Each element contains the combined
  #'  graph for its group.
  #' @param drug_target_edgelists A named list (elements `group1` and `group2`). Each element
  #' contains the list of edges adjacent to drug targets as a data frame (columns `from`, `to` and
  #' `weight`)
  #' @param settings A named list containing pipeline settings
  #'
  #' @return A named list (elements `group1` and `group2`). Each element contains an iGraph object
  #'  containing the interaction score as weight.
  #' @export
  #'
  #' @examples
  #' data(combined_graphs_example)
  #' data(drug_targets_example)
  #' settings <- molnet_settings()
  #' \donttest{
  #' interaction_score_graphs <- interaction_score(combined_graphs_example[["graphs"]],
  #' drug_target_edgelists=drug_targets_example[["edgelist"]],
  #' settings=settings)
  #' }
  #'
  tryCatch({
    total_edges <- sapply(graphs, igraph::ecount)
    message("Writing data...")
    write_interaction_score_input(graphs, drug_target_edgelists, settings$saving_path)
    rm(graphs)
    rm(drug_target_edgelists)
    gc()
    message("done.\nRunning python script for interaction score computation.\n")
    calculate_interaction_score(settings$max_path_length, total_edges, settings$saving_path, settings$python_executable, settings$script_path, settings$int_score_mode)
    message("Loading data...")
    return(load_interaction_score_output(settings$saving_path))
  }, error = function(e) {
    message("Interaction score cannot be computed. Perhaps python executable could not be run.")
    return(NULL)
  })
}

differential_score <- function(interaction_score_graphs, score_name="weight"){

  #'  The absolute difference of interaction score of two groups
  #'
  #' Computes the absolute difference of interaction score between
  #' two groups. Returns a single graph with the differential score as only edge
  #' attribute. The interaction score is computed by \code{\link{interaction_score}}.
  #'
  #' @param interaction_score_graphs Named list with elements `group1` and
  #' `group2` containing iGraph objects with score as edge attribute. Output of
  #' \code{\link{interaction_score}}.
  #' @param score_name Character string specifying the name of the edge
  #' attribute (default: `weight`).
  #'
  #' @return iGraph object with `differential_score` as only edge attribute
  #' @export
  #'
  #' @examples
  #' data(interaction_score_graphs_example)
  #' interaction_score_graphs <- interaction_score_graphs_example
  #' differential_score_graph <- differential_score(interaction_score_graphs, score_name = "weight")
  #'

  joined_graph <- igraph::union(interaction_score_graphs$group1, interaction_score_graphs$group2)
  joined_data_frame <- igraph::as_data_frame(joined_graph, "edges")
  score_1 <- paste0(score_name, '_1')
  score_2 <- paste0(score_name, '_2')

  joined_data_frame[[score_1]] <- joined_data_frame[[score_1]] %>% tidyr::replace_na(replace = 0)
  joined_data_frame[[score_2]] <- joined_data_frame[[score_2]] %>% tidyr::replace_na(replace = 0)

  reduced_joined_data <- joined_data_frame %>% dplyr::filter(is.finite(!!dplyr::sym(score_1)) & is.finite(!!dplyr::sym(score_2)))
  reduced_joined_data$differential_score <- abs(reduced_joined_data[[score_1]] - reduced_joined_data[[score_2]])
  joined_graph <- igraph::set_edge_attr(joined_graph, "differential_score", value=reduced_joined_data$differential_score)
  joined_graph <- igraph::delete_edge_attr(joined_graph, score_1)
  joined_graph <- igraph::delete_edge_attr(joined_graph, score_2)

  return(joined_graph)

}

get_drug_response_score <- function(differential_graph, drug_targets, interaction_table) {
  #' Calculate drug response score
  #'
  #' @description This function takes the differential graph (generated in
  #' \code{\link{differential_score}}), the a drug targets object (containing target node names and
  #' drugs and their targets; generated in \code{\link{determine_drug_targets}}) and the supplied
  #' drug-target interaction table (formatted in \code{\link{make_drug_target}}) to calculate the
  #' differential drug response score. The score is the median of all differential scores of the
  #' edges adjacent to all drug target nodes of a particular drug.
  #'
  #' @param differential_graph iGraph graph object containing differential scores for all edges.
  #' Output of \code{\link{differential_score}}
  #' @param drug_targets Named list containing two elements (`target_nodes` and
  #' `drugs_to_target_nodes`). Output of \code{\link{determine_drug_targets}}. `target_nodes` is a
  #' vector containing network node names of the nodes that are targeted by the available drugs.
  #' `drugs_to_target_nodes` is a dictionary-like list that maps drugs to the nodes that they
  #' target.
  #' @param interaction_table Data frame. Element `interaction_table` of `drug_target_interaction`
  #' created by \code{\link{make_drug_target}}. Contains at least two columns: `drug_name`
  #' containing names of drugs and a column named with an identifier present in the targeted layer.
  #'
  #' @export
  #'
  #' @return Data frame containing drug name and associated differential drug response score
  #' @examples
  #' data(drug_gene_interactions)
  #' drug_target_interaction <- make_drug_target(target_molecules='protein',
  #' interaction_table=drug_gene_interactions, match_on='gene_name')
  #'
  #'
  #' data(drug_targets_example)
  #' data(differential_score_graph_example)
  #' drug_response_score <- get_drug_response_score(differential_score_graph_example,
  #' drug_targets_example[["targets"]], drug_target_interaction$interaction_table)
  #' @importFrom rlang .data

  # vector of target node ids
  target_nodes <- drug_targets$target_nodes
  # dictionary-like list of drug names matched to their target nodes
  drugs_to_target_nodes <- drug_targets$drugs_to_target_nodes

  # filter target_nodes contained in differential graph
  target_node_ids <- target_nodes$node_id[(target_nodes$group1 | target_nodes$group2)]

  # get directly adjacent edges of target nodes
  adjacent_edges <- igraph::incident_edges(differential_graph, as.character(target_node_ids))

  # group the differential scores of the edges directly adjacent to target nodes per node
  scores_per_vertex <- lapply(adjacent_edges, function(x) x$differential_score)

  # combine all differential scores of the edges adjacent to all target nodes of each drug
  scores_per_drug <- lapply(drugs_to_target_nodes, function(x) scores_per_vertex[as.character(x)])

  # apply median on all differential edge scores per drug
  drug_response_per_drug <- lapply(scores_per_drug, function(x) stats::median(unname(unlist(x))))


  # get all drug names contained in interaction table
  all_drug_names <- interaction_table %>%
    dplyr::select(.data$drug_name) %>%
    unique()

  # create final output data frame, NAs are added for the drugs which do not have a target in the graph
  drug_response_score <- data.frame(drug_name = names(drug_response_per_drug),
                                    drug_response_score = unname(unlist(drug_response_per_drug)))  %>%
    dplyr::right_join(all_drug_names) %>%
    dplyr::select(.data$drug_name, .data$drug_response_score) %>%
    dplyr::arrange(.data$drug_name)


  return(drug_response_score)
}
