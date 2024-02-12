molnet_settings <- function(
  #network generation
  correlation_method = "pearson",
  print_graph_info = TRUE,
  reduction_method = "p_value",
  handling_missing_data = "all.obs",

  # network reduction
  p_value_adjust_method = "BH",
  reduction_alpha = 0.05,
  r_squared_cutoff = 0.6,
  cut_vector = seq(0.2, 0.8, by = 0.05),
  n_threads = 1,
  parallel_chunk_size = 10^6,

  # saving
  saving_path = tempdir(),
  save_individual_graphs = TRUE,
  save_combined_graphs = TRUE,
  save_drug_targets = TRUE,
  save_correlation_filename = NULL,

  # interaction_score
  python_executable = "python3",
  max_path_length = 3,
  int_score_mode = "auto",
  ...){

  #' Create global settings variable for molnet pipeline
  #'
  #' Function that allows creating a global `settings` variable used in the
  #' \link[molnet]{start_pipeline} function. Default parameters can be changed within the function
  #' call.
  #'
  #' @param correlation_method Correlation method used for graph generation. One of (`pearson`,
  #' `spearman`, `kendall`).
  #' @param print_graph_info Boolean. Print a summary of the reduced graph to console after
  #' generation?
  #' @param reduction_method Reduction method for reducing networks. One of `p_value`,
  #''\link[WGCNA]{pickHardThreshold}' or `pickHardThreshold_alternative`. Can be a single character
  #'string if the same for all layers, else a named list mapping layer names to methods. Layers may
  #'be omitted if a method is mapped to `default`.
  #' @param handling_missing_data Specifying the handling of missing data during correlation
  #' computation. Use "all.obs" or "pairwise.complete.obs". Argument is passed to
  #' \code{\link[WGCNA]{cor}}. Can be a single character string if the same for all layers, else a
  #' named list mapping layer names to methods. Layers may be omitted if a method is mapped to
  #' `default`.
  #' @param p_value_adjust_method String of the correction method applied to p-values. Passed to
  #' \link[stats]{p.adjust}. ("holm", "hochberg", "hommel", "bonferroni",
  #                                  "BH", "BY", "fdr", "none")
  #' @param reduction_alpha A number indicating the significance value for correlation p-values
  #' during reduction. Not-significant edges are dropped.
  #' @param r_squared_cutoff A number indicating the desired minimum scale free topology fitting
  #' index R^2 for reduction using \code{\link[WGCNA]{pickHardThreshold}}.
  #' @param cut_vector A vector of hard threshold cuts for which the scale free topology fit indices
  #'  are to be calculated during reduction with \code{\link[WGCNA]{pickHardThreshold}}.
  #' @param n_threads Number of threads for parallel computation of p-values during p-value
  #' reduction.
  #' @param parallel_chunk_size Number of p-values in smallest work unit when computing in parallel
  #' during network reduction with method `p_value`.
  #' @param saving_path Path to save outputs of `molnet` functions. Default is a temporary
  #' directory.
  #' @param save_individual_graphs Boolean specifying if individual graphs should be saved during
  #' \code{\link{start_pipeline}}
  #' @param save_combined_graphs Boolean specifying if combined graphs should be saved during
  #' \code{\link{start_pipeline}}
  #' @param save_drug_targets Boolean specifying if drug targets should be saved during
  #' \code{\link{start_pipeline}}
  #' @param save_correlation_filename File name for saving correlation adjacency matrices
  #' in \code{\link{generate_individual_graphs}}.
  #' @param python_executable Path to Python executable used for computing simple paths.
  #' @param max_path_length Integer of maximum length of simple paths to include in computation.
  #' @param int_score_mode One of `auto`, `sequential` or `ray`. Whether to compute interaction
  #' score in parallel using the Ray python library or sequentially. When `auto` it depends on the
  #' graph sizes.
  #' @param ... Supply additional settings.
  #'
  #' @return Named list of settings
  #' @export
  #'
  #' @examples
  #' settings <- molnet::molnet_settings(correlation_method = "spearman", max_path_length = 3,
  #'                                     handling_missing_data = list(
  #'                                       default = "pairwise.complete.obs",
  #'                                       mrna = "all.obs"
  #'                                     ),
  #'                                     reduction_method = "p_value"
  #'                                    )
  #'
  settings <- c(as.list(environment()), list(...))

  return(settings)


}

get_layer_setting <- function(layer, settings, setting_name) {
  #' Get layer settings
  #'
  #' Returns specified setting for a specific network layer.
  #'
  #' @param layer A network layer created by \code{\link{make_layer}}
  #' @param settings Named list of settings created by \code{\link{molnet_settings}}
  #' @param setting_name String indicating the setting to return.
  #' @return Setting value(s) for this layer
  #' @export
  if (!is.list(settings[[setting_name]])) {
    return(settings[[setting_name]])
  }
  if (!is.null(settings[[setting_name]][[layer]])) {
    return(settings[[setting_name]][[layer]])
  }
  if (!is.null(settings[[setting_name]][['default']])) {
    return(settings[[setting_name]][['default']])
  }
  stop(stringr::str_interp("Neither was a setting ${setting_name} given for layer ${layer} nor any default."))
}
