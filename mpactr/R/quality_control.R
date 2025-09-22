#' Summary of Filtering
#'
#' @description
#' Parses an mpactr object and extracts a summary of all applied filters.
#' Specifically, the fate of each input ion is reported as ion status. Status
#' options are: Passed, mispicked, group, replicability, and insouce. A status
#' of Passed ions is returned for ions that passed all applied filters and
#' therefore are expected to be high quality ions. Ions tagged as group,
#' mispicked, replicability, or ionsource were removed during the corresponding
#' filter.
#'
#' @param mpactr_object an `mpactr_object`.
#'
#' @return a `data.table` reporting the number of high quality ions
#' ("Passed") or the filter in which they were removed.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_mispicked_ions(data,
#'   ringwin = 0.5,
#'   isowin = 0.01,
#'   trwin = 0.005,
#'   max_iso_shift = 3,
#'   merge_peaks = TRUE
#' )
#'
#' summary <- qc_summary(data_filter)
#' summary
qc_summary <- function(mpactr_object) {
  graph_pactr_object <- graph_qc_pactr$new(mpactr_object)
  graph_pactr_object$generate_QC_Summary()
  return(graph_pactr_object$get_summarized_dt())
}

#' Visualize Filtering Summary as Tree Map
#'
#' @description
#' `plot_qc_tree()` visualizes the filtering summary as a treemap. Ion
#' status (see [qc_summary()]) is reported here as percentage of all
#' pre-filtered ions.
#'
#' @param mpactr_object an `mpactr_object`.
#'
#' @return a tree map plot of class `ggplot`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_mispicked_ions(data,
#'   ringwin = 0.5,
#'   isowin = 0.01,
#'   trwin = 0.005,
#'   max_iso_shift = 3,
#'   merge_peaks = TRUE
#' )
#'
#' plot_qc_tree(data_filter)
plot_qc_tree <- function(mpactr_object) {
  graph_pactr_object <- graph_qc_pactr$new(mpactr_object)
  graph_pactr_object$generate_QC_Summary()
  return(graph_pactr_object$plot_QC_Tree())
}
