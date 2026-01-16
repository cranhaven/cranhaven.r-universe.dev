#' Filter Hierarchical Clusters Based on Size and Dominance
#'
#' This function filters the results of hierarchical clustering by retaining only clusters
#' that contain at least `n_members` unique lineages. To avoid excluding potentially dominant but small clusters,
#' the user may also provide a minimum average frequency threshold to retain small
#' clusters that include a dominant member.
#'
#' @param series_filtered A data frame preprocessed using `filterData()`, containing lineage frequencies and metadata.
#' @param clusters A data frame containing hierarchical clustering assignments (e.g., from `cutree()`), possibly across multiple thresholds.
#' @param n_members An integer specifying the minimum number of members (lineages) required for a cluster to be retained.
#' @param min_freq_ignored_clusters Optional. A numeric value specifying the minimum average frequency required to retain
#'        small clusters (i.e., those with fewer than `n_members`). If `NULL`, small clusters are not rescued.
#'
#' @return A data frame containing the filtered clusters, including both large clusters and optionally small clusters with at least
#' one dominant member (based on the `min_freq_ignored_clusters` threshold).
#' @import dplyr
#' @export
#' @name filterHC
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Filter data to retain dominant and persistent barcodes
#' filtered_df <- filterData(
#'   input_df = input_dataframe,
#'   freq_threshold = 0.00005,
#'   time_threshold = 5,
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )
#'
#' # Perform hierarchical clustering using Pearson correlation
#' cluster_assignments <- performHClustering(
#'   filtered_data = filtered_df,
#'   agglomeration_method = "average",
#'   similarity_metric = "pearson",
#'   output_directory = tempdir(),
#'   input_name = "demo",
#'   missing_values = "pairwise.complete.obs",
#'   dtw_norm = NULL
#' )
#' 
#' # Filter clusters: keep only clusters with at least 8 members.
#' filtered_clusters <- filterHC(
#'   series_filtered = filtered_df,
#'   clusters = cluster_assignments,
#'   n_members = 8,
#'   min_freq_ignored_clusters = 0.0001
#' )

filterHC <- function(series_filtered, clusters, n_members, min_freq_ignored_clusters = NULL){

  # Add rank to clusters
  nRank = nrow(clusters)
  clusters$rank = seq(1:nRank)
  clusters_long=reshape2::melt(clusters,id.vars = c("rank"))
  colnames(clusters_long)=c("rank","cutoff","cluster")

  # Prepare series_filtered
  series_filtered$points = NULL
  series_filtered$rank=seq(1:nRank)
  series_filtered_long=reshape2::melt(series_filtered,id.vars = c('ID','rank','mean'), variable.name = "Time", value.name = "Frequency")

  # Merge clustering and lineage time series
  series_reshaped = merge(series_filtered_long,clusters_long,by.x = "rank",by.y = "rank", all = TRUE)
  series_reshaped$Time = as.numeric(as.character(series_reshaped$Time))

  # Keep clusters with enough members
  series_reshaped_1=series_reshaped %>%  dplyr::group_by(cluster,cutoff) %>% dplyr::filter(length(unique(ID)) >= n_members)
  clusters_filtered = series_reshaped_1

  # Handle potentially ignored dominant clusters
  if (nrow(series_reshaped_1) != nrow(series_reshaped)) {
    warning(paste("By ignoring clusters with fewer than", n_members, "members, you are potentially ignoring dominant clusters."))
    
    if (!is.null(min_freq_ignored_clusters)) {
      series_reshaped_2 <- series_reshaped %>%
        dplyr::group_by(cluster, cutoff) %>%
        dplyr::filter(length(unique(ID)) < n_members) %>%
        mutate(mean_freq = mean(Frequency)) %>%
        dplyr::filter(mean_freq >= min_freq_ignored_clusters)
      
      series_reshaped_2$mean_freq <- NULL
      clusters_filtered <- rbind(clusters_filtered, series_reshaped_2)
    } else {
      message("No frequency threshold provided for rescuing small but dominant clusters. Skipping this step.")
    }
  }

  return(clusters_filtered)
}
