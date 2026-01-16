#' Quantify and Visualize Hierarchical Clustering Results
#'
#' This script contains several functions to help quantify and visualize the results of hierarchical clustering
#' on barcode time-series data. The main function is `plotHCQuantification()`, which computes a LOESS-smoothed
#' average of barcode frequencies per cluster and evaluates inter-cluster distances across different clustering thresholds.
#'
#'
#' @name plotHCQuantification
#' @param clusters_filtered A data frame output from `filterHC()`, containing barcode frequencies with cluster labels across time points.
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1").
#' 
#' @import dplyr
#' @import ggplot2
#' @return No return value. This function saves a plot and a CSV file containing the smallest inter-cluster distances per threshold.
#' @export
#'  
#' @examples
#' \donttest{ 
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
#' # Filter clusters to retain only those with at least 8 members,
#' # unless they contain a dominant lineage
#' filtered_clusters <- filterHC(
#'   series_filtered = filtered_df,
#'   clusters = cluster_assignments,
#'   n_members = 8,
#'   min_freq_ignored_clusters = 0.0001
#' )
#'
#' # Quantify and visualize clustering quality across thresholds
#' plotHCQuantification(
#'   clusters_filtered = filtered_clusters,
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )
#' }

plotHCQuantification <- function(clusters_filtered,
                                 output_directory,
                                 input_name){

  clusters_dataframe = applyLOESS(clusters_filtered)

  clusters_dataframe$cluster=as.factor(as.integer(clusters_dataframe$cluster))
  clusters_dataframe$cutoff=as.numeric(as.character(clusters_dataframe$cutoff))
  cutoff=unique(clusters_dataframe$cutoff)

  ## Calculate the smallest distance between clones depending on threshold.
  ## TSDistances: computes distances between pairs of time series. (TSdist::)
  if (!proxy::pr_DB$entry_exists("TSDistances")){
    proxy::pr_DB$set_entry(FUN = TSdist::TSDistances, names=c("TSDistances"), loop = TRUE, type = "metric", distance = TRUE)
  }
  tf = clusters_dataframe %>% split(.$cutoff)  %>%
    purrr::map(~{
      tidyr::spread(.x, key = cluster, value = model)
    }) %>%  purrr::map(~{
      proxy::dist(t(.x[c(-1,-2)]), method="TSDistances", distance="euclidean")
    })

  distance_pairwise=lapply(tf, function(x) melt_dist(as.matrix(x)))
  distance_pairwise=do.call(rbind, (purrr::imap(distance_pairwise, ~mutate(.x, cutoff = .y))))

  smallest_distance<- distance_pairwise %>%
    dplyr::mutate(cluster= as.numeric(iso1)) %>%
    dplyr::group_by(cutoff) %>%
    dplyr::mutate(cluster=max(cluster),id=paste(iso1,iso2 ,sep="_")) %>%
    dplyr::summarise(dist_small=min(dist),cluster=max(cluster))

  readr::write_csv(smallest_distance,file = paste(output_directory, "/", input_name, "_threshold_selection.csv",sep=""),col_names = TRUE)

  rm(distance_pairwise)

  scale=max(smallest_distance$dist_small)/max(smallest_distance$cluster)

  ## Plot the quantification

  choose_threshold = ggplot(smallest_distance,aes(as.numeric(as.character(cutoff)),dist_small))+ geom_line(color="black", size=2.5) +
    theme_Publication() + geom_line(aes(as.numeric(as.character(cutoff)), as.integer(as.character(cluster))*scale), size = 2, color = "#56B4E9") +
    scale_y_continuous(sec.axis = sec_axis(~./scale,name="Number of clusters")) + scale_x_reverse() +xlab("Threshold") + ylab("Distance between clusters")

  tryCatch(
    {
      ggsave(choose_threshold,filename =  paste(output_directory, "/", input_name, "_threshold_selection", ".eps",sep=""),width = 9,height = 8)
    },
    error = function(e) {
      if(grepl("Transformation for secondary axes must be monotonic", e$message)) {
        custom_error_message <- "Error: You are not keeping enough lineages to properly perform a hierarchical clustering. The '-c' argument might be too strict."
        stop(custom_error_message)
      } else {
        # If it's not the specific error you're expecting, re-raise the error
        stop(e)
      }
    }
  )


}

#################
#' @param dist A distance matrix (typically a result of a distance computation).
#' @param order Optional character vector indicating the order of row/column names
#'  to rearrange the matrix before melting.
#' @param dist_name A string naming the distance variable in the resulting data frame.
#'  Default is "dist".
#' @description The melt_dist() function takes a distance matrix and converts it
#'  into a long-format data frame where each row corresponds to a unique pair of
#'   elements and their associated distance. It essentially "melts" the lower 
#'   triangle of the matrix into a tidy format, which is useful for plotting or 
#'   further analysis.
#' @return A data frame with columns: `iso1`, `iso2`, and the specified distance column.
#' @export
#' @rdname plotHCQuantification

melt_dist <- function(dist, order = NULL, dist_name = 'dist') {
  if(!is.null(order)){
    dist <- dist[order, order]
  } else {
    order <- row.names(dist)
  }
  diag(dist) <- NA
  dist[upper.tri(dist)] <- NA
  dist_df <- as.data.frame(dist)
  dist_df$iso1 <- row.names(dist)
  dist_df <- dist_df %>%
    tidyr::gather_(key = "iso2", value = lazyeval::interp("dist_name", dist_name = as.name(dist_name)), order, na.rm = T)
  return(dist_df)
}

#################
#' Apply LOESS Smoothing to Clustered Time-Series Data
#'
#' Applies LOESS smoothing to barcode frequencies within each cluster over time,
#' using only the persistent barcodes (those present at the last time point).
#' Clusters are re-ranked within each threshold based on their average final frequency.
#'
#' @param clusters_filtered A data frame filtered by `filterHC()`, containing `cluster`, `cutoff`, `Time`, and `Frequency` columns.
#'
#' @return A data frame with smoothed values for each cluster and time point: columns include `cluster`, `cutoff`, `model`, and `time`.
#' @export
#' @rdname plotHCQuantification


applyLOESS <- function(clusters_filtered){

  ## Keep only the persistent barcodes
  series_order=subset(clusters_filtered,clusters_filtered$Time==max(unique(clusters_filtered$Time)))

  series_order=series_order %>%
    dplyr::group_by(cluster,cutoff) %>%
    dplyr::summarise(average = mean(Frequency)) %>% dplyr::ungroup() %>%
    dplyr::group_by(cutoff) %>%
    dplyr::arrange(desc(average), .by_group = TRUE)  %>% dplyr::mutate(cluster2=as.factor(dplyr::row_number()))

  series_order$cluster2=as.integer(as.character(series_order$cluster2))
  clusters_filtered=merge(clusters_filtered,series_order,by=c("cluster","cutoff"))
  clusters_filtered$cluster=NULL
  names(clusters_filtered)[8]="cluster"

  max.range = max(clusters_filtered$Time)-min(clusters_filtered$Time)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(clusters_filtered$Time), to=max(clusters_filtered$Time),length.out = loess.range)

  grouped_df=clusters_filtered %>%  dplyr::group_by(cluster,cutoff)
  spanned_grouped_df = grouped_df %>% dplyr::summarise(model=stats::predict(adjust_span(Time, Frequency, span = 0.2),xx,se = FALSE))
  grouped_span_df = spanned_grouped_df %>% dplyr::group_by(cluster,cutoff) %>% dplyr::mutate(time=xx)

  return(grouped_span_df)
}


