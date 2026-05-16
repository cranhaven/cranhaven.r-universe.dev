install_python_dependencies <- function(package_manager="pip") {
    #' @title Installs python dependencies needed for interaction score computation
    #'
    #' @description Uses pip (default) or conda as specified to 
    #' install all required Python modules. The Python packages are installed 
    #' into a virtual Python or conda environment called 'r-DrDimont'. 
    #' The following requirements are installed: numpy, tqdm, python-igraph and ray.
    #' The environment is created with reticulate.
    #'
    #' @param package_manager ["pip"|"conda"] Package manager to use (default: pip)
    #'
    #' @return No return value, called to install python dependencies
    #' 
    #' @export
    
    if (package_manager=="pip") {
        reticulate::virtualenv_create("r-DrDimont")
        reticulate::virtualenv_install(packages=list("numpy", "tqdm", "igraph", "ray"), envname="r-DrDimont", ignore_installed=TRUE)
    }
    else if (package_manager=="conda") {
        reticulate::conda_create("r-DrDimont", python_version=3.9, packages="pip")
        reticulate::py_install(packages=list("numpy", "tqdm", "igraph", "ray"), envname="r-DrDimont", method="conda", pip=TRUE)
    }
    else{
        stop(message(format(Sys.time(), "[%y-%m-%d %X] "), 
                     "ERROR: Either use `package_manager=\"pip\"` or `package_manager=\"conda\"` to create python environment and install required python dependencies."))
    }
}


make_layer <- function(name, data_groupA, data_groupB, identifiers_groupA, identifiers_groupB) {
    #' @title Creates individual molecular layers from raw data and unique identifiers
    #'
    #' @description Helper function to transform input data to required pipeline input format. Additionally, the
    #' supplied input is checked. Allows easy conversion of raw data into the structure accepted by
    #' \code{\link[DrDimont]{run_pipeline}}.
    #'
    #' @param name [string] Name of the layer.
    #' @param data_groupA,data_groupB [data.frame] Dataframe containing raw molecular data of each group
    #' (each stratum). Analyzed components (e.g. genes) in columns, samples (e.g. patients) in rows.
    #' @param identifiers_groupA,identifiers_groupB [data.frame] Dataframe containing component identifiers
    #' (columns) of each component (rows) in the same order as the molecular dataframe of each group.
    #' These identifiers are used to (a) interconnect graphs and (b) match drugs to drug targets.
    #' Must contain a column `type` which identifies the nature of the component (e.g., "protein")
    #'
    #' @return Named list containing the supplied data for each group (i.e., the data set for one
    #' layer), that can be supplied to \code{\link[DrDimont]{run_pipeline}} and `name` giving the name of the
    #' layer. Each sub-list contains the `data` and the `identifiers`.
    #'
    #' @examples
    #' data(protein_data)
    #'
    #' example_protein_layer <- make_layer(
    #'                              name="protein",
    #'                              data_groupA=protein_data$groupA[, c(-1,-2)],
    #'                              data_groupB=protein_data$groupB[, c(-1,-2)],
    #'                              identifiers_groupA=data.frame(
    #'                                  gene_name=protein_data$groupA$gene_name,
    #'                                  ref_seq=protein_data$groupA$ref_seq),
    #'                              identifiers_groupB=data.frame(
    #'                                  gene_name=protein_data$groupB$gene_name,
    #'                                  ref_seq=protein_data$groupB$ref_seq))
    #'
    #' @export

    ### if group2 data and identifiers not given set it to NULL
    if ((is.null(data_groupB)) & (is.null(identifiers_groupB))){
        layer <- list(groupA = list(data = t(data.frame(data_groupA)), identifiers = data.frame(identifiers_groupA)),
                      groupB = NULL,
                      name = name)
    } else {
        layer <- list(groupA = list(data = t(data.frame(data_groupA)), identifiers = data.frame(identifiers_groupA)),
                      groupB = list(data = t(data.frame(data_groupB)), identifiers = data.frame(identifiers_groupB)),
                      name = name)
    }
    return_errors(check_layer(layer))
    return(layer)
}


make_connection <- function(from, to, connect_on, weight=1, group="both") {
    #' @title Specify connection between two individual layers
    #'
    #' @description Helper function to transform input data to the required pipeline input format. This helper
    #' function creates a list that specifies the connection between two layers.
    #' The connection can be based on IDs present in the identifiers of both layer or an interaction
    #' table containing a mapping of the connections and edge weights.
    #' Additionally, the supplied input is checked. Allows easy conversion of raw data into the
    #' structure accepted by \code{\link[DrDimont]{run_pipeline}}.
    #'
    #' __IMPORTANT:__ If a connection is established based on \code{id} this ID has to be present in
    #' the identifiers of both layers, they have to be named identically and the IDs have to be formatted
    #' identically as these are matched by an inner join operation (refer to \code{\link[DrDimont]{make_layer}}).
    #'
    #' @param from [string] Name of the layer from which the connection should be established
    #' @param to [string] Name of the layer to which the connection should be established
    #' @param connect_on [string|table] Specifies how the two layers should be connected. This can be based on a
    #' mutual ID or a table specifying interactions. Mutual ID: Character string specifying the name of an identifier
    #' that is present in both layers (e.g., `NCBI ID` to connect proteins and mRNA). Interaction table: A table mapping
    #' two identifiers of two layers. The columns have exactly the same names as the identifiers of the layers. The table has to
    #' contain an additional column specifying the weight between two components/nodes (see `weight` argument)
    #' @param weight [int|string] Specifies the edge weight between the layers. This can be supplied as a number
    #' applied to every connection or a column name of the interaction table.
    #' Fixed weight: A umber specifying the weight of every connection between the layers.
    #' Based on interaction table: Character string specifying the name of a column in the
    #' table passed as the `by` parameter which is used as edge weight. (default: 1)
    #' @param group ["A"|"B"|"both"] Group for which to apply the connection. One of `both`, `A` or `B`. (default: "both")
    #' 
    #' @return A named list (i.e., an inter-layer connection), that can be supplied to
    #' \code{\link[DrDimont]{run_pipeline}}.
    #'
    #' @examples
    #' data(metabolite_protein_interactions)
    #' 
    #' example_inter_layer_connections = list(make_connection(from='mrna', to='protein',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='phosphosite',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='metabolite',
    #'                                            connect_on=metabolite_protein_interactions,
    #'                                            weight='combined_score'))
    #'
    #' @export

    inter_layer_connections <- list(from=from, to=to, by="", connect_on=connect_on, weight=weight, group=group)
    if (is.character(connect_on) & is.vector(connect_on) & length(connect_on) == 1) { inter_layer_connections$by <- "id" }
    else if (is.data.frame(connect_on)) { inter_layer_connections$by <- "table" }
    else { inter_layer_connections$by <- "none" }

    return_errors(check_connection(inter_layer_connections))
    return(inter_layer_connections)
}

make_drug_target <- function(target_molecules, interaction_table, match_on) {
    #' @title Reformat drug-target-interaction data
    #'
    #' @description Function to transform input data to required input format for
    #' \code{\link[DrDimont]{run_pipeline}}. Here the data that is needed to define drug-target interactions is
    #' formatted. When the reformatted output is passed to \code{\link[DrDimont]{run_pipeline}} as
    #' \code{drug_target_interactions} argument, the differential integrated drug response score can be
    #' calculated for all the supplied drugs in \code{interaction_table}.
    #'
    #' @param target_molecules [string] Name of layer containing the drug targets. This name has to match the
    #' corresponding named item in the list of layers supplied to \code{\link[DrDimont]{run_pipeline}}.
    #' @param interaction_table [data.frame] Has to contain two columns. A column called `drug_name` containing
    #' names or identifiers of drugs. And a column with a name that matches an identifier in the layer supplied
    #' in `target_molecules`. Additional columns will be ignored in the pipeline.
    #' For example, if drugs target proteins and an identifier called `ncbi_id` was supplied in layer creation of
    #' the protein layer (see \code{\link[DrDimont]{make_layer}}), this column should be called
    #' `ncbi_id` and contain the corresponding IDs of protein-drug targets. Any other ID present in
    #' the constructed layer could also be used.
    #' @param match_on [string] Column name of the dataframe supplied in `interaction_table` that is used for
    #' matching drugs and target nodes in the graph (e.g. `ncbi_id`).
    #'
    #' @return Named list of the input parameters in input format of \code{\link[DrDimont]{run_pipeline}}.
    #'
    #' @examples
    #' data(drug_gene_interactions)
    #' 
    #' example_drug_target_interactions <- make_drug_target(target_molecules='protein',
    #'                                         interaction_table=drug_gene_interactions,
    #'                                         match_on='gene_name')
    #' 
    #' @export

    res <- list(target_molecules=target_molecules, interaction_table=interaction_table, match_on=match_on)
    return_errors(check_drug_target(res))
    return(res)
}



run_pipeline <- function(layers, inter_layer_connections, drug_target_interactions, settings) {
    #' @title Execute all DrDimont pipeline steps sequentially
    #'
    #' @description This wrapper function executes all necessary steps to generate differential integrated
    #' drug response scores from the formatted input data. The following input data is required
    #' (and detailed below):
    #' 
    #' * Layers of stratified molecular data.
    #' 
    #' * Additional connections between the layers.
    #' 
    #' * Interactions between drugs and nodes in the network.
    #' 
    #' * Settings for pipeline execution.
    #'
    #' As this function runs through all steps of the DrDimont pipeline it can take a long time to complete,
    #' especially if the supplied molecular data is rather large. Several prompts will be printed to supply
    #' information on how the pipeline is proceeding. Calculation of the interaction score by
    #' \code{\link[DrDimont]{generate_interaction_score_graphs}} requires saving large-scale graphs to file and calling
    #' a Python script. This handover may take time.
    #'
    #' Eventually a dataframe is returned containing the supplied drug name and its associated
    #' differential drug response score computed by DrDimont.
    #'
    #' @param layers [list] Named list with different network layers containing data and identifiers for
    #' both groups. The required input format is a list with names corresponding to the content of
    #' the respective layer (e.g., "protein"). Each named element has to contain the molecular data
    #' and corresponding identifiers formatted by \code{\link[DrDimont]{make_layer}}.
    #' @param inter_layer_connections [list] A list with specified inter-layer connections. This list
    #' contains one or more elements defining individual inter-layer connections created by
    #' \code{\link[DrDimont]{make_connection}}.
    #' @param drug_target_interactions [list] A list specifying drug-target interactions for drug response
    #' score computation. The required input format of this list is created by
    #' \code{\link[DrDimont]{make_drug_target}}. The drug response score is calculated for all drugs contained
    #' in this object.
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return Dataframe containing drug name and associated differential integrated drug response score.
    #' If Python is not installed or the interaction score computation fails for some other reason, NULL
    #' is returned instead.
    #' 
    #' @examples
    #' \dontshow{
    #' WGCNA::disableWGCNAThreads()
    #' }
    #' data(drug_gene_interactions)
    #' data(metabolite_protein_interactions)
    #' data(layers_example)
    #'
    #' example_inter_layer_connections = list(make_connection(from='mrna', to='protein',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='phosphosite',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='metabolite',
    #'                                            connect_on=metabolite_protein_interactions,
    #'                                            weight='combined_score'))
    #'
    #' example_drug_target_interactions <- make_drug_target(target_molecules='protein',
    #'                                         interaction_table=drug_gene_interactions,
    #'                                         match_on='gene_name')
    #'
    #' example_settings <- drdimont_settings(
    #'                         handling_missing_data=list(
    #'                             default="pairwise.complete.obs",
    #'                             mrna="all.obs"),
    #'                         reduction_method="pickHardThreshold",
    #'                         r_squared=list(default=0.65, metabolite=0.1),
    #'                         cut_vector=list(default=seq(0.2, 0.65, 0.01)))
    #'
    #' \donttest{
    #' run_pipeline(
    #'     layers=layers_example, 
    #'     inter_layer_connections=example_inter_layer_connections, 
    #'     drug_target_interactions=example_drug_target_interactions, 
    #'     settings=example_settings)
    #' }
    #' 
    #' @export

    message(format(Sys.time(), "[%y-%m-%d %X] "), "### Pipeline started ###")
    message(format(Sys.time(), "[%y-%m-%d %X] "), "Validating input...")
    return_errors(check_input(layers, inter_layer_connections, drug_target_interactions))
    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 1: Computing correlation matrices ###")
    correlation_matrices <- compute_correlation_matrices(layers, settings)

    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 2: Generating individual graphs ###")
    individual_graphs <- generate_individual_graphs(correlation_matrices, layers, settings)


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 3: Combining graphs ###")
    combined_graphs <- generate_combined_graphs(individual_graphs$graphs,
                                                individual_graphs$annotations,
                                                inter_layer_connections,
                                                settings)


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 4: Identifying drug targets ###")
    drug_targets <- determine_drug_targets(combined_graphs$graphs,
                                           combined_graphs$annotations,
                                           drug_target_interactions,
                                           settings)


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 5: Calculating integrated interaction scores ###")
    interaction_score_graphs <- generate_interaction_score_graphs(combined_graphs$graphs,
                                                                  drug_targets$edgelist,
                                                                  settings)
    if (is.null(interaction_score_graphs)) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "ERROR: Interaction score was not computed. Maybe Python could not be run. Returning from pipeline early. Please check if Python is properly installed and `install_python_dependencies` as been called.")
        return(NULL)
    }


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 6: Generating differential graph ###")
    differential_score_graph <- generate_differential_score_graph(interaction_score_graphs, settings)


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### STEP 7: Computing differential drug response scores ###")
    drug_response_scores <- compute_drug_response_scores(differential_score_graph,
                                                         drug_targets$targets,
                                                         settings)


    message(format(Sys.time(), "[%y-%m-%d %X] "), "### Pipeline completed. ###")

    return(drug_response_scores)

}

compute_correlation_matrices <- function(layers, settings) {
    #' @title  Computes correlation matrices for specified network layers
    #'
    #' @description Constructs and returns a correlation/adjacency matrices for each network layer
    #' and each group. The adjacency matrix of correlations is computed using \link[WGCNA]{cor}.
    #' The handling of missing data can be specified. Optionally, the adjacency matrices of the
    #' correlations can be saved. Each node is mapped to the biological identifiers given in the
    #' layers and the mapping table is returned as `annotations`.
    #'
    #' @param layers [list] Named list with different network layers containing data and identifiers for both
    #'  groups (generated from \code{\link[DrDimont]{make_layer}})
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return A nested named list with first-level elements `correlation_matrices` and `annotations`. The second
    #' level elements are `groupA` and `groupB` (and `both` at `annotations`). These contain a named list of matrix
    #' objects (`correlation_matrices`) and dataframes (`annotations`) mapping the graph node IDs to biological
    #' identifiers. The third level elements are the layer names given by the user.
    #' @export
    #'
    #' @examples
    #' \dontshow{
    #' WGCNA::disableWGCNAThreads()
    #' }
    #'
    #' example_settings <- drdimont_settings(
    #'                         handling_missing_data=list(
    #'                             default="all.obs"))
    #' 
    #' # mini example with reduced mRNA layer for shorter runtime:
    #' data(mrna_data)
    #' reduced_mrna_layer <- make_layer(name="mrna",
    #'                           data_groupA=mrna_data$groupA[1:5,2:6],
    #'                           data_groupB=mrna_data$groupB[1:5,2:6],
    #'                           identifiers_groupA=data.frame(gene_name=mrna_data$groupA$gene_name[1:5]),
    #'                           identifiers_groupB=data.frame(gene_name=mrna_data$groupB$gene_name[1:5]))
    #' 
    #' example_correlation_matrices <- compute_correlation_matrices(
    #'                                     layers=list(reduced_mrna_layer), 
    #'                                     settings=example_settings)
    #' 
    #' # to run all layers use layers=layers_example from data(layers_example) 
    #' # in compute_correlation_matrices()
    #'

    ### empty list to store correlation matrices of individual layers
    adjacency_matrices <- list()

    ### empty list to store annotation dataframes
    layer_annotations <- list()

    ### iterate over layers
    for (n_layer in c(1:length(layers))){
        layer <- layers[[n_layer]][['name']]

        ### create annotations which uniquely define nodes across the network layer
        layer_annotation <- create_unique_layer_node_ids(get_layer(layer, layers)[['groupA']][['identifiers']],
                                                         get_layer(layer, layers)[['groupB']][['identifiers']],
                                                         layer_name = layer)

        layer_annotations[['groupA']][[layer]] <- layer_annotation$groupA
        layer_annotations[['groupB']][[layer]] <- layer_annotation$groupB
        layer_annotations[['both']][[layer]] <- layer_annotation$both

        handling_missing_data <- get_layer_setting(layer, "", settings, "handling_missing_data")
        reduction_method <- get_layer_setting(layer, "", settings, "reduction_method")

        for (group in c('groupA', 'groupB')){

            message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing correlation of layer ", layer, " for ", group, "...")

            ### if group not given skip it
            if (is.null(layers[[n_layer]][[group]])){
                message(format(Sys.time(), "[%y-%m-%d %X] "), "Skipping computation of correlation of layer ", layer, " for ", group, "")
                next
            }

            adjacency_matrix <- WGCNA::cor(get_layer(layer, layers)[[group]][['data']],
                                           method=settings$correlation_method,
                                           use=handling_missing_data)
            
            ### add row and column names
            rownames(adjacency_matrix) <- paste0("X", 1:nrow(adjacency_matrix))
            colnames(adjacency_matrix) <- paste0("X", 1:ncol(adjacency_matrix))
            
            message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

            ### save adjacency_matrix in 'adjacency_matrices' as named list
            adjacency_matrices[[group]][[layer]] <- adjacency_matrix
        }
    }

    correlation_matrices <- list(correlation_matrices=adjacency_matrices, annotations=layer_annotations)

    ### save correlation matrices as RData file if choosen
    if (settings$save_data){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving correlation matrices...")
        save(correlation_matrices, file=paste0(settings$saving_path, "/correlation_matrices.rda"))
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }

    return(correlation_matrices)
}

generate_individual_graphs <- function(correlation_matrices, layers, settings) {
    #' @title Builds graphs from specified network layers
    #'
    #' @description Constructs and returns two graphs for each network layer, where nodes
    #' correspond to the rows in the measurement data. Graphs are initially complete and
    #' edges are weighted by correlation values of the measurements across columns. The
    #' number of edges is then reduced by either a threshold on the p-value of the
    #' correlation or a minimum scale-free fit index.
    #'
    #' @param correlation_matrices [list] List of correlation matrices generated with
    #' \code{\link[DrDimont]{compute_correlation_matrices}}
    #' @param layers [list] Named list with different network layers containing data and
    #' identifiers for both groups (generated from \code{\link[DrDimont]{make_layer}})
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return A nested named list with first-level elements `graphs` and `annotations`. The second
    #' level elements are `groupA` and `groupB` (and `both` at `annotations`). These contain a list of
    #' iGraph objects (`graphs`) and dataframes (`annotations`) mapping the graph node IDs to biological
    #' identifiers. The third level elements are layer names given by the user.
    #'
    #' @examples
    #' \dontshow{
    #' WGCNA::disableWGCNAThreads()
    #' }
    #'
    #' data(layers_example)
    #' data(correlation_matrices_example)
    #'
    #' example_settings <- drdimont_settings(
    #'                         handling_missing_data=list(
    #'                             default="pairwise.complete.obs",
    #'                             mrna="all.obs"),
    #'                         reduction_method="pickHardThreshold",
    #'                         r_squared=list(default=0.65, metabolite=0.1),
    #'                         cut_vector=list(default=seq(0.2, 0.5, 0.01)))
    #'
    #' example_individual_graphs <- generate_individual_graphs(
    #'                                  correlation_matrices=correlation_matrices_example,
    #'                                  layers=layers_example, 
    #'                                  settings=example_settings)
    #'
    #' 
    #' @export


    ### empty list to store igraph objects of individual layers and inter-layer connections
    graphs <- list()

    ### empty list to store annotation dataframes
    annotations <- list()

    ### iterate over layers
    for (n_layer in c(1:length(layers))){

        layer <- layers[[n_layer]][['name']]

        ### for current layer get handling_missing_data and reduction_method from settings
        handling_missing_data <- get_layer_setting(layer, "", settings, "handling_missing_data")
        reduction_method <- get_layer_setting(layer, "", settings, "reduction_method")

        ### for both groups reduce receptive network of current layer
        for (group in c('groupA', 'groupB')){

            message(format(Sys.time(), "[%y-%m-%d %X] "), "Generating graph of layer ", layer, " for ", group, "...")

            ### for current layer if pickHardThreshold get r_squared_cutoff and cut_vector from settings
            if (reduction_method=="pickHardThreshold"){

                r_squared_cutoff <- get_layer_setting(layer, group, settings, "r_squared_cutoff")
                cut_vector <- get_layer_setting(layer, group, settings, "cut_vector")
                mean_number_edges <- get_layer_setting(layer, group, settings, "mean_number_edges")
                edge_density <- get_layer_setting(layer, group, settings, "edge_density")

                if (is.null(mean_number_edges)) { mean_number_edges <- NULL }
                else if (is.na(mean_number_edges)) { mean_number_edges <- NULL }
                else if (mean_number_edges == 0) { mean_number_edges <- NULL }

                if (is.null(edge_density)) { edge_density <- NULL }
                else if (is.na(edge_density)) { edge_density <- NULL }
                else if (edge_density == 0) { edge_density <- NULL }


            } else {
                r_squared_cutoff = NULL
                cut_vector = NULL
                mean_number_edges = NULL
            }

            ### if group not given skip it
            if (is.null(layers[[n_layer]][[group]])){
                message(format(Sys.time(), "[%y-%m-%d %X] "), "Skipping layer ", layer, " for ", group, ".")
                next
            }

            ### generate graph for a specific layer with the data of a specific group
            layer_graph <- generate_reduced_graph(adjacency_matrix=correlation_matrices[['correlation_matrices']][[group]][[layer]],
                                                  measurement_data=get_layer(layer, layers)[[group]][['data']],
                                                  identifiers=correlation_matrices[['annotations']][[group]][[layer]],
                                                  handling_missing_data=handling_missing_data,
                                                  reduction_method=reduction_method,
                                                  r_squared_cutoff=r_squared_cutoff,
                                                  cut_vector=cut_vector,
                                                  mean_number_edges=mean_number_edges,
                                                  edge_density=edge_density,
                                                  p_value_adjustment_method=settings$p_value_adjust_method,
                                                  reduction_alpha=settings$reduction_alpha)

            ### save graph in 'graphs' as named list
            graphs[[group]][[layer]] <- layer_graph

            message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
        }
    }

    individual_graphs <- list(graphs=graphs, annotations=correlation_matrices[['annotations']])

    ### save individual graphs as RData file if chosen
    if (settings$save_data) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving individual graphs...")
        save(individual_graphs, file=paste0(settings$saving_path, "/individual_graphs.rda"))
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }

    return(individual_graphs)
}


generate_combined_graphs <- function(graphs, annotations, inter_layer_connections, settings) {
    #' @title Combines individual layers to a single graph
    #'
    #' @description Individual graphs created by \code{\link[DrDimont]{generate_individual_graphs}}
    #' are combined to a single graph per group according to `inter_layer_connections`. Returns a
    #' list of combined graphs along with their annotations.
    #'
    #' @param graphs [list] A named list (elements `groupA` and `groupB`). Each element contains a list of
    #' iGraph objects (`graphs` from output of \code{\link[DrDimont]{generate_individual_graphs}}).
    #' @param annotations [list] A named list (elements `groupA`, `groupB` and `both`). Each element contains a
    #' list of dataframes mapping each node IDs to identifiers. `both` contains unique identifiers across the
    #' whole data. (`annotations` from output of \code{\link[DrDimont]{generate_individual_graphs}})
    #' @param inter_layer_connections [list] Named list with specified inter-layer connections. Names are
    #' layer names and elements are connections (\link[DrDimont]{make_connection}).
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return A named list (elements `graphs` and sub-elements `$groupA` and
    #' `$groupB`, and `annotations` and sub-element `both`). Contains the igraph objects of the combined
    #' network and their annotations for both groups.
    #'
    #' @examples
    #' \dontshow{
    #' WGCNA::disableWGCNAThreads()
    #' }
    #'
    #' data(individual_graphs_example)
    #' data(metabolite_protein_interactions)
    #'
    #' example_inter_layer_connections = list(make_connection(from='mrna', to='protein',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='phosphosite',
    #'                                            connect_on='gene_name', weight=1),
    #'                                        make_connection(from='protein', to='metabolite',
    #'                                            connect_on=metabolite_protein_interactions,
    #'                                            weight='combined_score'))
    #'
    #' example_settings <- drdimont_settings()
    #'
    #' example_combined_graphs <- generate_combined_graphs(
    #'                                graphs=individual_graphs_example$graphs,
    #'                                annotations=individual_graphs_example$annotations,
    #'                                inter_layer_connections=example_inter_layer_connections,
    #'                                settings=example_settings)
    #' 
    #' @export

    inter_layer_edges <- list()

    ### if more than one layer given
    if (length(graphs$groupA) > 1){
        # iterate over specified connections
        for (n_connection in c(1:length(inter_layer_connections))) {

            connection <- inter_layer_connections[[n_connection]]

            if (connection$group == "both") { groups <- c("groupA", "groupB")
            } else {groups <- paste0("group", as.character(connection$group))}

            # connections can be established by shared IDs ('id') between networks or by a given interaction
            table ('table')
            if (connection$by == 'id'){
                message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("Connecting ${connection$from} and ${connection$to} "), "by id.")

                # iterate over groups
                for (group in groups){

                    layer1 <- connection$from
                    layer2 <- connection$to
                    id <- connection$connect_on
                    weight <- connection$weight

                    # generate inter-layer graph
                    message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("${group}: "), "Generating inter-layer edgelist...")

                    inter_layer_edgelist <- inter_layer_edgelist_by_id(annotations[[group]][[layer1]],
                                                                       annotations[[group]][[layer2]],
                                                                       connection = id,
                                                                       weight = weight)

                    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.")

                    # save inter-layer graph to list of graph objects
                    inter_layer_edges[[group]] <- append(inter_layer_edges[[group]], list(inter_layer_edgelist))
                }
            } else if (connection$by == 'table'){

                message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("Connecting ${connection$from} and ${connection$to} "), "by table.")

                # iterate over groups
                for (group in groups){

                    layer1 <- connection$from
                    layer2 <- connection$to
                    interaction_table <- connection$connect_on
                    weight_column <- connection$weight

                    message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("${group}: "), "Generating inter-layer edgelist...")
                    inter_layer_edgelist <- inter_layer_edgelist_by_table(annotations[[group]][[layer1]],
                                                                          annotations[[group]][[layer2]],
                                                                          interaction_table = interaction_table,
                                                                          weight_column = weight_column)

                    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.")
                    # save inter-layer graph to list of graph objects
                    inter_layer_edges[[group]] <- append(inter_layer_edges[[group]], list(inter_layer_edgelist))
                }
            }
        }
    }

    # empty list to store combined graphs for both groups
    graphs_combined <- list()
    annotations_combined <- list()

    ### if only one layer given, skip combinig layers and return combined_graph for the given layer
    if (length(graphs$groupA) == 1){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Only one layer given: Skipping combining the graphs.")

        layer = names(graphs$groupA)[[1]]

        # iterate over groups
        for (group in c("groupA", "groupB")){

            ### if group not given skip it
            if (is.null(graphs[[group]])){
                message(format(Sys.time(), "[%y-%m-%d %X] "), "Skipping combining the graphs of ", group, "")
                next
            }
            graphs_combined[[group]] <- graphs[[group]][[layer]]
        }
    } else {

        message(format(Sys.time(), "[%y-%m-%d %X] "), "Combining graphs...")

        # iterate over groups
        for (group in c("groupA", "groupB")){

            ### if group not given skip it
            if (is.null(graphs[[group]])){
                message(format(Sys.time(), "[%y-%m-%d %X] "), "Skipping combining the graphs of ", group, "")
                next
            }
            message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("${group}"), "...")
            graphs_combined[[group]] <- combine_graphs(graphs[[group]], inter_layer_edges[[group]])
        }
        message(format(Sys.time(), "[%y-%m-%d %X] "), stringr::str_interp("${group} "), "done.\n")
    }

    annotations_combined[['both']] <- dplyr::bind_rows(annotations[['both']])

    combined_graphs <- list(graphs = graphs_combined, annotations = annotations_combined)

    ### save combined graphs as RData file if choosen
    if (settings$save_data){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving combined graphs...")
        save(combined_graphs, file=paste0(settings$saving_path, "/combined_graphs.rda"))
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }

    return(combined_graphs)
}


determine_drug_targets <- function(graphs, annotations, drug_target_interactions, settings) {
    #' @title Determine drug target nodes in network
    #'
    #' @description Finds node IDs of network nodes in `graphs` that are targeted by a drug in
    #' `drug_target_interactions`. Returns list of node ids and list of adjacent edges.
    #'
    #' @param graphs [list] A named list with elements `groupA` and `groupB` containing the combined graphs
    #' of each group as iGraph object (`graphs` from output of \code{\link[DrDimont]{generate_combined_graphs}})
    #' @param annotations [list] List of dataframes that map node IDs to identifiers. Contains `both`
    #' with unique identifiers across the whole data (output of \code{\link[DrDimont]{generate_combined_graphs}})
    #' @param drug_target_interactions [list] Named list specifying drug target interactions for drug response
    #'  score computation
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return A named list with elements `targets` and `edgelists`.
    #' `targets` is a named list with elements `target_nodes` and `drugs_to_target_nodes`.
    #' `target_nodes` is a dataframe with column `node_id` (unique node IDs in the iGraph object
    #' targeted by drugs) and columns `groupA` and `groupB` (bool values specifying whether the
    #' node is contained in the combined graph of the group). Element `drugs_to_target_nodes` contains
    #' a named list mapping drug names to a vector of their target node IDs.
    #' `edgelists` contains elements `groupA` and `groupB` containing each a list of
    #' edges adjacent to drug target nodes.
    #'
    #' @examples
    #' data(drug_gene_interactions)
    #' data(combined_graphs_example)
    #'
    #' example_settings <- drdimont_settings()
    #'
    #' example_drug_target_interactions <- make_drug_target(target_molecules='protein',
    #'                                         interaction_table=drug_gene_interactions,
    #'                                         match_on='gene_name')
    #'
    #' example_drug_target_edges <- determine_drug_targets(
    #'                                  graphs=combined_graphs_example$graphs,
    #'                                  annotations=combined_graphs_example$annotations,
    #'                                  drug_target_interactions=example_drug_target_interactions,
    #'                                  settings=example_settings)
    #'
    #' @export

    drug_target_edge_list <- list()
    drug_targets_list <- list()

    message(format(Sys.time(), "[%y-%m-%d %X] "), "Determining drug targets...")

    drug_targets_list <- find_targets(graphs = graphs,
                                      target_molecules = drug_target_interactions$target_molecules,
                                      interaction_table = drug_target_interactions$interaction_table,
                                      annotation = annotations[['both']],
                                      on = drug_target_interactions$match_on)

    if(!length(drug_targets_list$target_nodes$node_id)){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "ERROR: Drug targets were not found in the networks. Please try a different set of data!")
        stop("Drug targets were not found in the networks. Please try a different set of data!")
    }

    ### iterate over groups
    for (group in c("groupA", "groupB")){
        ### if group not given skip it
        if (is.null(graphs[[group]])){next}
        drug_target_edge_list[[group]] <- target_edge_list(graphs[[group]], drug_targets_list$target_nodes, group = group)
    }

    drug_target_edges <- list(targets = drug_targets_list, edgelists = drug_target_edge_list)
    message(format(Sys.time(), "[%y-%m-%d %X] "), length(drug_targets_list$target_nodes$node_id), " drug targets found.")
    message(format(Sys.time(), "[%y-%m-%d %X] "), length(drug_targets_list$drugs_to_target_nodes), " drugs with targets found.")
    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

    ### save drug targets as RData file if choosen
    if (settings$save_data) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving drug targets...")
        save(drug_target_edges, file=paste0(settings$saving_path, "/drug_target_edges.rda"))
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }
    return(drug_target_edges)
}

generate_interaction_score_graphs <- function(graphs, drug_target_edgelists, settings) {
    #' @title Computes interaction score for combined graphs
    #'
    #' @description  Writes the input data (combined graphs for both groups in `gml` format and
    #' lists of edges adjacent to drug targets for both groups in `tsv` format) to files and calls a Python script
    #' for calculating the interaction scores. Output files written by the Python script are two graphs in `gml`
    #' format containing the interaction score as an additional `interaction_weight` edge attribute.
    #' These are loaded and returned in a named list.
    #' ATTENTION: Data exchange via files is mandatory and takes a long time for large data. Interaction
    #' score computation is expensive and slow because it involves finding all simple paths up to a
    #' certain length between source and target node of the drug target edges. Don't set the parameter `max_path_length`
    #' in \code{\link[DrDimont]{drdimont_settings}} to a large value and only consider this step if your graphs have approximately
    #' 2 million edges or less. 
    #' The Python script is parallelized using Ray. Use the \code{\link[DrDimont]{drdimont_settings}} parameter `int_score_mode` to force sequential
    #' or parallel computation. Refer to the Ray documentation if you encounter problems with running
    #' the Python script in parallel. DISCLAIMER: Depending on the operating system Python comes
    #' pre-installed or has to be installed manually. Use DrDimont's \code{\link[DrDimont]{install_python_dependencies}} 
    #' to install a virtual Python or conda environment containing the required Python packages. 
    #' You can use the parameter `conda` in \code{\link[DrDimont]{drdimont_settings}} to specify if Python packages 
    #' were installed with conda (`conda=TRUE`), else a virtual environment installed with pip is 
    #' assumed (default: `conda=FALSE`).
    #'
    #' @param graphs [list] A named list with elements `groupA` and `groupB` containing the combined graphs
    #' of each group as iGraph object (`graphs` from output of \code{\link[DrDimont]{generate_combined_graphs}})
    #' @param drug_target_edgelists [list] A named list (elements `groupA` and `groupB`). Each element
    #' contains the list of edges adjacent to drug targets as a dataframe (columns `from`, `to` and
    #' `weight`). `edgelists` from output of \code{\link[DrDimont]{determine_drug_targets}}
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return A named list (elements `groupA` and `groupB`). Each element contains an iGraph object
    #' containing the interaction scores as interaction_weight attributes.
    #'
    #' @examples
    #' data(combined_graphs_example)
    #' data(drug_target_edges_example)
    #' 
    #' example_settings <- drdimont_settings()
    #'
    #' \donttest{
    #' example_interaction_score_graphs <- generate_interaction_score_graphs(
    #'                                         graphs=combined_graphs_example$graphs,
    #'                                         drug_target_edgelists=drug_target_edges_example$edgelists,
    #'                                         settings=example_settings)
    #' }
    #' 
    #' @export


    ### write files for Python into savings_path from settings
    message(format(Sys.time(), "[%y-%m-%d %X] "), "Writing data...")
    write_interaction_score_input(graphs, drug_target_edgelists, settings$saving_path)
    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.")

    ### get total number of edges and remove unneeded data
    total_edges <- sapply(graphs, igraph::ecount)
    
    graphB_null <- is.null(graphs$groupB)
    rm(graphs)
    rm(drug_target_edgelists)
    gc()

    ### run the Python script for interaction score calculation
    tryCatch({
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Running Python script for interaction score computation.")
        calculate_interaction_score(settings$max_path_length, total_edges, settings$saving_path, settings$conda, 
                                    settings$script_path, settings$num_cpus, settings$int_score_mode, 
                                    settings$cluster_address, graphB_null)

        message(format(Sys.time(), "[%y-%m-%d %X] "), "Loading data...")

        interaction_score_graphs <- load_interaction_score_output(settings$saving_path, graphB_null)

        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

        ### save interaction score graphs as RData file if chosen
        if (settings$save_data) {
            message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving interaction score graphs...")
            save(interaction_score_graphs, file=paste0(settings$saving_path, "/interaction_score_graphs.rda"))
            message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
        }
        return(interaction_score_graphs)

    }, error = function(e){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "ERROR: Interaction score was not computed. Maybe Python could not be run. Please check if Python is properly installed and `install_python_dependencies` as been called.")
        return(NULL)
    })
}


generate_differential_score_graph <- function(interaction_score_graphs, settings){
    #' @title Compute difference of interaction score of two groups
    #'
    #' @description Computes the absolute difference of interaction scores between
    #' the two groups. Returns a single graph with the differential score and the
    #' differential interaction score as edge attributes. The interaction score
    #' is computed by \code{\link[DrDimont]{generate_interaction_score_graphs}}.
    #'
    #' @param interaction_score_graphs [list] Named list with elements `groupA` and
    #' `groupB` containing iGraph objects with weight and interaction_weight as edge attributes (output of
    #' \code{\link[DrDimont]{generate_interaction_score_graphs}})
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return iGraph object with `differential_score` and `differential_interaction_score` as edge attributes
    #' 
    #' @examples
    #' data(interaction_score_graphs_example)
    #'
    #' example_settings <- drdimont_settings()
    #'
    #' example_differential_score_graph <- generate_differential_score_graph(
    #'                                         interaction_score_graphs=interaction_score_graphs_example,
    #'                                         settings=example_settings)
    #'
    #' @importFrom rlang .data
    #' 
    #' @export

    message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing differential networks...")

    ### skip differential network computation if only one group given
    if (is.null(interaction_score_graphs$groupB)){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Only one group given. Skipping computation of differential networks.\n")
        return(interaction_score_graphs$groupA)
        joined_graph <- interaction_score_graphs$groupA
        joined_df <- igraph::as_data_frame(joined_graph, "edges")
        joined_df[['weight']] <- joined_df[['weight']] %>% tidyr::replace_na(replace = 0)
        joined_df[['interactionweight']] <- joined_df[['interactionweight']] %>% dplyr::na_if(Inf) %>% tidyr::replace_na(replace = 0)
        joined_graph <- igraph::set_edge_attr(joined_graph, "differential_score", value=joined_df$weight)
        joined_graph <- igraph::set_edge_attr(joined_graph, "differential_interaction_score", value=joined_df$interactionweight)
        joined_graph <- igraph::delete_edge_attr(joined_graph, 'weight')
        joined_graph <- igraph::delete_edge_attr(joined_graph, 'interactionweight')

        differential_score_graph <- joined_graph
        ### save differential score graphs as RData file if choosen
        if (settings$save_data) {
            message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving differential score graphs...")
            save(differential_score_graph, file=paste0(settings$saving_path, "/differential_score_graph.rda"))
            message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
        }
        return(differential_score_graph)
    }

    ### join the combined graphs of groupA and groupB and compute the differential scores
    joined_graph <- igraph::union(interaction_score_graphs$groupA, interaction_score_graphs$groupB)
    joined_df <- igraph::as_data_frame(joined_graph, "edges")

    joined_df[['weight_1']] <- joined_df[['weight_1']] %>% tidyr::replace_na(replace = 0)
    joined_df[['weight_2']] <- joined_df[['weight_2']] %>% tidyr::replace_na(replace = 0)
    joined_df[['interactionweight_1']] <- joined_df[['interactionweight_1']] %>% dplyr::na_if(Inf) %>% tidyr::replace_na(replace = 0)
    joined_df[['interactionweight_2']] <- joined_df[['interactionweight_2']] %>% dplyr::na_if(Inf) %>% tidyr::replace_na(replace = 0)

    ###
    reduced_joined_df <- joined_df %>% dplyr::filter(is.finite(!!dplyr::sym('weight_1')) & is.finite(!!dplyr::sym('weight_2')))

    ### calculate absolute differences
    reduced_joined_df$differential_score <- reduced_joined_df[['weight_1']] - reduced_joined_df[['weight_2']]
    reduced_joined_df$differential_interaction_score <- reduced_joined_df[['interactionweight_1']] - reduced_joined_df[['interactionweight_2']]

    ### replace differential interaction scores with NA if NA/Inf given in both groups
    joined_df <- igraph::as_data_frame(joined_graph, "edges")
    joined_df[['weight_1']] <- joined_df[['weight_1']]
    joined_df[['weight_2']] <- joined_df[['weight_2']]
    reduced_joined_df[['interactionweight_1']] <- joined_df[['interactionweight_1']] %>% dplyr::na_if(Inf)
    reduced_joined_df[['interactionweight_2']] <- joined_df[['interactionweight_2']] %>% dplyr::na_if(Inf)
    reduced_joined_df <- reduced_joined_df %>%
                        dplyr::mutate(differential_interaction_score=ifelse(!(is.na(.data$interactionweight_1)&is.na(.data$interactionweight_2)),
                                                                            .data$differential_interaction_score, NA)) %>%
                        dplyr::mutate(differential_score=ifelse(!(is.na(.data$weight_1)&is.na(.data$weight_2)),
                                                                .data$differential_score, NA))

    ### add new edge atttributes to graph
    joined_graph <- igraph::set_edge_attr(joined_graph, "differential_score", value=reduced_joined_df$differential_score)
    joined_graph <- igraph::set_edge_attr(joined_graph, "differential_interaction_score", value=reduced_joined_df$differential_interaction_score)
    joined_graph <- igraph::delete_edge_attr(joined_graph, 'weight_1')
    joined_graph <- igraph::delete_edge_attr(joined_graph, 'weight_2')
    joined_graph <- igraph::delete_edge_attr(joined_graph, 'interactionweight_1')
    joined_graph <- igraph::delete_edge_attr(joined_graph, 'interactionweight_2')

    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

    differential_score_graph <- joined_graph
    ### save differential score graphs as RData file if choosen
    if (settings$save_data) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Saving differential score graphs...")
        save(differential_score_graph, file=paste0(settings$saving_path, "/differential_score_graph.rda"))
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }
    return(differential_score_graph)
}


compute_drug_response_scores <- function(differential_graph, drug_targets, settings) {
    #' @title Calculate drug response score
    #'
    #' @description This function takes the differential graph (generated in
    #' \code{\link[DrDimont]{generate_differential_score_graph}}), the a drug targets object (containing target node names and
    #' drugs and their targets; generated in \code{\link[DrDimont]{determine_drug_targets}}) and the supplied
    #' drug-target interaction table (formatted in \code{\link[DrDimont]{make_drug_target}}) to calculate the
    #' differential drug response score. The score is the mean or median of all differential scores of the
    #' edges adjacent to all drug target nodes of a particular drug.
    #'
    #' @param differential_graph iGraph graph object containing differential scores for all edges.
    #' (output of \code{\link[DrDimont]{generate_differential_score_graph}})
    #' @param drug_targets [list] Named list containing two elements (`target_nodes` and
    #' `drugs_to_target_nodes`). `targets` from output of \code{\link[DrDimont]{determine_drug_targets}}. `target_nodes` is a
    #' vector containing network node names of the nodes that are targeted by the available drugs.
    #' `drugs_to_target_nodes` is a dictionary-like list that maps drugs to the nodes that they
    #' target.
    #' @param settings [list] A named list containing pipeline settings. The settings list has to be
    #' initialized by \code{\link[DrDimont]{drdimont_settings}}. Items in the named list can be
    #' adjusted as desired.
    #'
    #' @return Dataframe containing drug name and associated differential (integrated) drug response score
    #' 
    #' @examples
    #' data(drug_target_edges_example)
    #' data(differential_graph_example)
    #'
    #' example_settings <- drdimont_settings()
    #'
    #' example_drug_response_scores <- compute_drug_response_scores(
    #'                                     differential_graph=differential_graph_example,
    #'                                     drug_targets=drug_target_edges_example$targets,
    #'                                     settings=example_settings)
    #' 
    #' @importFrom rlang .data
    #' 
    #' @export


    ### mean drug response else median drug response
    median_drug_response <- settings$median_drug_response
    absolute_difference <- settings$absolute_difference

    ### compute the median or mean score
    if (median_drug_response & absolute_difference) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing drug response scores based on the median of the absolute differential scores ...\n")
        }
    else if (median_drug_response & !absolute_difference) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing drug response scores based on the median of the differential scores ...\n")
        }
    else if (!median_drug_response & absolute_difference) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing drug response scores based on the mean of the absolute differential scores ...\n")
        }
    else if (!median_drug_response & !absolute_difference) {
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Computing drug response scores based on the mean of the differential scores ...\n")
        }

    if(!length(drug_targets$target_nodes$node_id)){
        message(format(Sys.time(), "[%y-%m-%d %X] "), "ERROR: Drug targets are not given. Drug response score was not computed!")
        stop("ERROR: Drug targets are not given. Drug response score was not computed!")
    }


    ### dictionary-like list of drug names matched to their target nodes
    drugs_to_target_nodes <- drug_targets$drugs_to_target_nodes

    differential_graph_df <- igraph::as_data_frame(differential_graph)

    drug_response_scores <- data.frame(drug_name=names(drugs_to_target_nodes),
                                       drug_response_score=rep(NA, length(names(drugs_to_target_nodes))))

    ### for each drug
    for(i in 1:length(drugs_to_target_nodes)){
        ### get the drug target nodes
        targets <- drugs_to_target_nodes[[i]]
        drug <- names(drugs_to_target_nodes[i])[[1]]
        ### get the edge weights of the nodes
        targets_edges <- differential_graph_df[((differential_graph_df$from %in% targets)|(differential_graph_df$to %in% targets)),]

        ### compute the median or mean score
        if (median_drug_response & absolute_difference) { score <- stats::median(abs(targets_edges$differential_interaction_score)) }
        else if (median_drug_response & !absolute_difference) { score <- stats::median(targets_edges$differential_interaction_score) }
        else if (!median_drug_response & absolute_difference) { score <- mean(abs(targets_edges$differential_interaction_score)) }
        else if (!median_drug_response & !absolute_difference) { score <- mean(targets_edges$differential_interaction_score) }

        ### save the drug's response score in dataframe
        drug_response_scores["drug_response_score"][drug_response_scores["drug_name"] == drug] <- abs(score)
    }

    drug_response_scores <- drug_response_scores %>% dplyr::arrange(.data$drug_name)

    message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")

    if (settings$save_data) {
        ### save drug response score in csv-file
        message(format(Sys.time(), "[%y-%m-%d %X] "), "Writing drug response scores to csv file...")
        utils::write.table(drug_response_scores, paste0(settings$saving_path, "/drug_response_scores.tsv"), sep='\t', row.names=FALSE, quote = FALSE)
        message(format(Sys.time(), "[%y-%m-%d %X] "), "done.\n")
    }

    return(drug_response_scores)
}
