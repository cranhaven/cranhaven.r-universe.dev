create_unique_layer_node_ids <- function(identifiers1, identifiers2, layer_name) {
  #' Assigns node IDs to the biological identifiers across a graph layer
  #'
  #' (INTERNAL) This function takes two data frames of (biological) identifiers of nodes. Each data
  #' frame corresponds to the identifiers of the components contained in the single-layer network of
  #'  a sample group.
  #' This function outputs the same data frames, with an added column (`node_id`) that contains node
  #'  IDs which can later be used as `name` parameter for an iGraph graph.
  #' Node IDs begin with the defined `prefix` and an underscore. If a molecule is present in both
  #' groups, the node ID will be the same across the whole layer, allowing to easily combine the
  #' graphs of both groups in `differential_score()` to calculate differential scores of identical
  #' nodes in both sample groups.
  #' The function is used by the high-level wrapper \link[molnet]{generate_individual_graphs} to
  #' create annotations, which uniquely define nodes across the network layer.
  #'
  #' @param identifiers1,identifiers2 Data frames containing the biological identifiers of each
  #' group of the same network layer.
  #' @param layer_name Name of layer node ids are created for
  #'
  #' @export
  #' @return Returns an named list. Elements `identifiers1` and `identifiers2` contain the input
  #' data frames with an additional column `node_id`. `all` contains all unique node IDs assigned
  #' across the network layer.

  assigned_ids <- dplyr::union(identifiers1, identifiers2) %>%
    dplyr::mutate(node_id = paste0(layer_name, "_", dplyr::row_number()),
                  layer = layer_name)

  identifiers1 <- dplyr::left_join(identifiers1, assigned_ids)
  identifiers2 <- dplyr::left_join(identifiers2, assigned_ids)

  return(list(group1 = identifiers1,
              group2 = identifiers2,
              all = assigned_ids))

}

generate_reduced_graph <- function(measurement_data,
                                   identifiers,
                                   correlation_method = 'spearman',
                                   reduction_method = 'p_value',
                                   save_correlation_filename = NULL,
                                   handling_missing_data = "all.obs",
                                   p_value_adjustment_method = "BH",
                                   reduction_alpha = 0.05,
                                   r_squared_cutoff =  0.85,
                                   cut_vector = seq(0.2, 0.8, by = 0.05),
                                   print_graph_info = FALSE,
                                   n_threads = parallel::detectCores() - 1,
                                   parallel_chunk_size = 10^6
                                   ) {
  #' Generate a reduced iGraph
  #'
  #' (INTERNAL) A wrapper functions that calls the functions to generate a network from raw data and
  #'  reduce the network by a given method. Graph generation is using
  #'  \code{\link[igraph]{graph.adjacency}} internally.
  #' Methods implemented are \link[molnet]{network_reduction_by_p_value} (reduction by statistical
  #' significance of correlation) and \link[molnet]{network_reduction_by_pickHardThreshold} (using
  #' WGCNA function \link[WGCNA]{pickHardThreshold.fromSimilarity} that finds a suitable cutoff
  #' value to get a scale-free network).
  #' If no method is given, no reduction will be performed. When using the reduction method
  #' `p_value` the user can specify an alpha significance value and a method for p-value adjustment.
  #'  When using the reduction by `pickHardthreshold` a R-Squared Cutoff can be specified and a cut
  #'  vector can be supplied.
  #' The adjacency matrix of correlations is computed using \link[WGCNA]{cor}. The handling of
  #' missing data can be specified. Both the adjacency of correlations and the graph object can be
  #' saved optionally.
  #'
  #'
  #' @param measurement_data Data frame containing raw data (e.g. mRNA expression data, protein
  #' abundance, etc.). Analyzed components (e.g. genes) in rows, samples (e.g. patients) in columns.
  #' @param identifiers Data frame containing  biological identifiers and the corresponding node ID
  #' created in \link[molnet]{create_unique_layer_node_ids}. The column containing node IDs has to
  #' be named `node_id`.
  #' @param correlation_method A character string specifying the method to be used for correlation
  #' calculation by \link[WGCNA]{cor}. Can be any of "spearman", "pearson" or "kendall".
  #' @param reduction_method A character string specifying the method to be used for network
  #' reduction. `p_value` for hard thresholding based on the statistical significance of the
  #' computed correlation. `pickHardThreshold` for a cutoff based on the scale-freeness criterion
  #' (calls `WGCNA::pickHardThreshold`).
  #' @param save_correlation_filename (optional) Set a name for saving the matrix of correlations.
  #' Will be saved as .rds file.
  #' @param handling_missing_data A character string specifying the handling of missing data. Use
  #' "all.obs" (default) or "pairwise.complete.obs". Argument is passed to `WGCNA::cor()`.
  #' @param p_value_adjustment_method String of the correction method applied to p-values. Passed to
  #'  \link[stats]{p.adjust}.
  #' @param reduction_alpha A number indicating the alpha value applied for thresholding.
  #' @param r_squared_cutoff A number indicating the desired minimum scale free topology fitting
  #' index R^2.
  #' @param cut_vector A vector of hard threshold cuts for which the scale free topology fit indices
  #'  are to be calculated.
  #' @param print_graph_info A boolean value specifying if a summary of the reduced graph should be
  #' printed.
  #' @param n_threads Number of threads for parallel computation of p-values during p-value
  #' reduction.
  #' @param parallel_chunk_size Number of p-values in smallest work unit when computing in parallel
  #' during network reduction with method `p_value`.
  #' @export
  #' @return iGraph graph object of the reduced network.

  adjacency_matrix <- WGCNA::cor(measurement_data,
                                 method = correlation_method,
                                 use = handling_missing_data)

  message("Correlation calculated.\n")

  if(!is.null(save_correlation_filename)){
    saveRDS(adjacency_matrix, file = save_correlation_filename)
  }

  # network reduction
  if (reduction_method == 'p_value') {
    message("Reducing network by p-values.\n")

    # get number of samples
    message("Computing sample size.\n")
    number_of_samples <- sample_size(measurement_data, handling_missing_data)
    message("Sample size computed. Starting p-value computation.\n")
    reduced_adjacency_matrix <- network_reduction_by_p_value(adjacency_matrix,
                                                             number_of_samples,
                                                             reduction_alpha,
                                                             p_value_adjustment_method,
                                                             parallel_chunk_size)
  } else if (reduction_method == 'pickHardThreshold' | reduction_method == 'pickHardThreshold_alternative') {
    reduced_adjacency_matrix <- network_reduction_by_pickHardThreshold(adjacency_matrix,
                                                                       r_squared_cutoff,
                                                                       cut_vector,
                                                                       method = reduction_method)
  } else {
    message('No network reduction.')
    reduced_adjacency_matrix <- adjacency_matrix
  }


  # add node identifiers
  colnames(reduced_adjacency_matrix) <- identifiers$node_id

  # generate iGraph object
  graph <- igraph::graph.adjacency(adjmatrix = reduced_adjacency_matrix,
                                   weighted = TRUE,
                                   diag = FALSE,
                                   mode = 'undirected')

  graph <- igraph::delete_edges(graph, which(is.na(igraph::E(graph)$weight)))

  if (print_graph_info == TRUE) {
    graph_metrics(graph)
  }

  # remove adjacency matrix from environment
  rm(adjacency_matrix)

  return(graph)
}

sample_size <- function(x, use) {
  #' Sample size for correlation computation
  #'
  #' (INTERNAL) Depending on how missing data is handled in correlation matrix computation, the
  #' number of samples used is returned. If `all.obs` is used the number of rows (i.e. samples)
  #' of the orignal data is returned. If `pairwise.complete.obs` is used the crossproduct of a
  #' matrix indicating the non-NA values is returned as matrix. This implementation was adopted
  #' from \code{\link[WGCNA]{corAndPvalue}}.
  #'
  #' @param x matrix of measurement data used for correlation computation
  #' @param use string indicating handling of missing data. Can be 'all.obs' for all observations
  #' or 'pairwise.complete.obs' for pairwise observations
  #'
  #' @return For 'all.obs' returns an integer indicating the number of samples in the supplied
  #' matrix (i.e. number of rows). For 'pairwise.complete.obs' returns a matrix in the same size
  #' of the correlation matrix indicating the number of samples for each correlation calculation.
  #' @export
  #' @source Method to calculate samples in `pairwise.complete.obs` adopted and improved from
  #' \code{\link[WGCNA]{corAndPvalue}}

  if (use == 'all.obs') {
    n_samples <- dim(x)[1]
  } else if (use == 'pairwise.complete.obs') {
    finMat <- !is.na(x)
    n_samples <- Rfast::Crossprod(finMat, finMat)
  }

  return(n_samples)
}
