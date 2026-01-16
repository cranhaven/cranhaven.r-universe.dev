#' Plot the log10-transformed barcode frequencies and the moving averages (LOESS)
#'
#' This file contains multiple functions. The main function is: plot_clusters_and_loess()
#' and it uses plotClusterLog10() and apply_LOESS(). In plot_clusters_and_loess(),
#' we plot the log10-transformed barcode frequencies contained in all selected clusters,
#' we compute a moving average per cluster and group them in a plot. We also write the files associated with
#' these two plots.
#'
#' @name plotClustersAndLoess
#' @param selected_clusters  A dataframe containing the clusters from a hierarchical clustering
#'  for a specific threshold
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1").
#' 
#' @import dplyr
#' @import ggplot2
#' @return No return value. This function saves plots and CSV files related to barcode cluster dynamics.
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
#'
#' # Plot log10-transformed barcode frequencies and smoothed LOESS average per cluster
#' plotClustersAndLoess(
#'   selected_clusters = filtered_clusters,
#'   output_directory = tempdir(),
#'   input_name = "demo"
#' )
#' }


plotClustersAndLoess <- function(selected_clusters,
                                 output_directory,
                                 input_name){

#######################################################################
# Clusters are ordered according to two criteria:
# 1) Clusters with a non-zero mean frequency at the last time point are ranked in descending order;
# 2) Clusters with an average frequency of zero at the last time point are ranked in descending order of disappearance: from the last extinct to the first extinct.


# 1)
  series.ordered=subset(selected_clusters,selected_clusters$Time==max(unique(selected_clusters$Time)))

  series.ordered=series.ordered %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(average = mean(Frequency))

  series.ordered=series.ordered[order(series.ordered$average,decreasing = TRUE), ]

# 2)
  series.reshaped <- arrange(selected_clusters, cluster, Time)

  # cumulative frequency per time point (for each cluster)
  series.reshaped <- series.reshaped %>%
    group_by(cluster,Time) %>%
    mutate(cumulative_freq = sum(Frequency))

  unique_rows <- distinct(series.reshaped, cluster, Time, cumulative_freq)

  first_occurrence_rows <- unique_rows %>% group_by(cluster) %>% filter(cumulative_freq == 0) %>% slice(1)

  df_ordered <- arrange(first_occurrence_rows, desc(Time))

# Merging 1) & 2)
  persistent_clusters <- subset(series.ordered, average > 0)
  nonpersistent_clusters <- subset(series.ordered, average == 0)

  index <- match(nonpersistent_clusters$cluster, df_ordered$cluster)
  nonpersistent_matched <- nonpersistent_clusters[order(index), ]

  series.ordered = rbind(persistent_clusters, nonpersistent_matched)
  series.reshaped$cumulative_freq = NULL
#####################################################################################

  series.ordered$cluster_order = seq(1:nrow(series.ordered))
  series.ordered$average = NULL

  clustered_series = merge(series.reshaped, series.ordered, by="cluster")
  clustered_series$cluster = NULL
  clustered_series$rank = NULL
  clustered_series$cutoff = NULL
  colnames(clustered_series)[5] = "cluster"
  colnames(clustered_series)[4] = "frequency"
  colnames(clustered_series)[3] = "time"

  ## Write clustered_series
  readr::write_csv(clustered_series,file = paste(output_directory, "/", input_name, "_clustered_series_log10.csv",sep=""),col_names = TRUE)

  clusters_dataframe = apply_LOESS(clustered_series, output_directory, input_name)
  ## loess
  clusters_dataframe$cluster=paste("C",clusters_dataframe$cluster,sep="")
  clusters_dataframe$time=clusters_dataframe$time*10
  clusters.loess <- tidyr::spread(clusters_dataframe, cluster, value)

  # csv of loess plot
  readr::write_csv(clusters_dataframe,file=paste0(output_directory, "/", input_name, "_loess_clusters.csv"),col_names = TRUE)
  # plot loess
  effective.breaks <- sort(c(unique(clustered_series$time)))
  effective.labels = as.character(effective.breaks)
  effective.limits = c(min(effective.breaks), max(effective.breaks))


  clusters_dataframe$cluster <- factor(clusters_dataframe$cluster, levels = unique(clusters_dataframe$cluster))
  loess.plot = ggplot(clusters_dataframe) + geom_line(aes(x=time/10,y=10^(value),group=cluster,color=cluster),size=1) + scale_x_continuous(limits = effective.limits) +
    theme_Publication() + scale_color_manual(values = cluster.colors,name="cluster") + ylab("Clone frequency") + xlab("Time") +
    scale_y_log10(limits=c(min(10^clusters_dataframe$value)+1e-7,1e0))+ coord_cartesian(expand = FALSE)
  ggsave(loess.plot,filename = paste(output_directory, "/", input_name, "_loess_clusters_log10.eps", sep=""),width = 8.25,height = 6)

  # write loess file
  readr::write_csv(clusters.loess,file = paste(output_directory, "/", input_name, "_clustered_loess_log10.csv",sep=""),col_names = TRUE)
  message("DONE")

}

#################
#' Plot individual barcode frequencies (log10) for a single cluster
#'
#' Plots all barcodes in a cluster on a log10 y-scale, along with the LOESS-smoothed average trajectory.
#'
#' @inheritParams plotClustersAndLoess
#' @name plotClusterLog10
#' @param df A dataframe containing barcode frequencies in a single cluster.
#' @param cluster The cluster ID (numeric or character).
#' @param color A color code to use for the cluster.
#' @param tf A dataframe containing the LOESS-smoothed trajectory for the cluster.
#' @param effective.breaks A vector of time points used as breaks on the x-axis.
#' @return A ggplot object showing the log10-transformed barcode frequencies and the LOESS-smoothed average trajectory for a single cluster.
#' @export

# plot clusters
plotClusterLog10 <- function(df,cluster,color,tf, effective.breaks, output_directory, input_name){

  effective.labels = as.character(effective.breaks)
  effective.limits = c(min(effective.breaks), max(effective.breaks))

  p = ggplot(df,aes(x=time,y=frequency)) +
    geom_line(aes(group=ID),color=color) + theme_Publication(base_size = 18) +
    scale_y_log10(limits=c(min(df$frequency)+1e-7,1e0)) +
    labs(x = "Time",y="Barcode frequency") + guides(color = FALSE) +
    scale_x_continuous(limits = effective.limits) +
    coord_cartesian(expand = FALSE) + geom_line(data=tf,aes(time,10^value),color="black") +
    annotate("text", y=-0.75, x = 3,label=paste("n",length(unique(df$ID)),sep=" = "),hjust=0,size=5) +
    ggtitle(paste("Cluster",cluster,sep=" "))

  # graphics::plot(p)
  # grDevices::dev.off()

  ggsave(p,filename =  paste(output_directory, "/", input_name, "_cluster", cluster, "_log10.eps", sep=""),width = 5.5,height = 4)

  return(p)
}

#################
#' Apply LOESS smoothing to barcode trajectories
#'
#' @inheritParams plotClustersAndLoess
#' @name apply_LOESS
#' @param c_series A dataframe containing `time`, `frequency`, and `cluster` columns.
#' @return A dataframe containing the LOESS-smoothed trajectories for each cluster, with columns `cluster`, `value`, and `time`.
#' @export

apply_LOESS <- function(c_series, output_directory, input_name){

  max.range = max(c_series$time)-min(c_series$time)
  loess.range = (max.range*10)+1

  ## Get moving average of barcode frequencies for each cluster USING LOESS
  xx <- seq(from=min(c_series$time), to=max(c_series$time),length.out = loess.range)

  cluster.df=c_series %>%  dplyr::group_by(cluster) %>%
    dplyr::summarise(value=stats::predict(adjust_span(time, frequency, span = 0.2),xx,se = FALSE))  %>%
    dplyr::group_by(cluster) %>% dplyr::mutate(time=xx)

  ## Plot log10-transformed barcode frequencies
  plotList = list()

  effective.breaks = sort(c(unique(c_series$time)))

  for(i in seq_along(unique(c_series$cluster))){
    l = plotClusterLog10(c_series[c_series$cluster==i,], i, cluster.colors[i], cluster.df[cluster.df$cluster==i,], effective.breaks, output_directory, input_name)
    plotList[[i]]=l

  }

  return(cluster.df)
}

