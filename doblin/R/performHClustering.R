#' Perform Hierarchical Clustering on Barcoded Lineages
#'
#' This function performs hierarchical clustering on time-series data representing barcoded lineages.
#' A distance matrix is computed using either Pearson correlation or Dynamic Time Warping (DTW),
#' and hierarchical clustering is applied using a specified agglomeration method.
#' A dendrogram and heatmap are generated for visual inspection. If no threshold is specified,
#' clusters are computed for all possible thresholds between 0.1 and the maximum tree height.
#'
#' @param filtered_data A data frame preprocessed with `filterData()`, containing filtered lineage frequencies.
#' @param agglomeration_method A character string specifying the agglomeration method (e.g., `"ward.D"`, `"complete"`).
#' @param similarity_metric A character string specifying the similarity metric (`"pearson"` or `"dtw"`).
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1")
#' @param missing_values Optional. A character string specifying how missing values should be handled in Pearson correlation (e.g., `"pairwise.complete.obs"`).
#' @param dtw_norm Optional. A character string specifying the norm to use with DTW distance ("L1" for Manhattan, "L2" for Euclidean).
#' Required if `similarity_metric = "dtw"`.
#'
#' @return A data frame with clustering assignments at multiple thresholds (columns named by height).
#' @export
#' @name performHClustering
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


performHClustering <- function(filtered_data,
                               agglomeration_method,
                               similarity_metric,
                               output_directory,
                               input_name,
                               missing_values = NULL,
                               dtw_norm = NULL){

  filtered_dataf=filtered_data[,!(colnames(filtered_data) %in% c("ID","mean","points"))]
  filtered_dataf[filtered_dataf == 0] <- NA

  # Color palette for heatmap
  color_palette <- grDevices::colorRampPalette(c("red", "white", "blue"))(n = nrow(filtered_dataf))

  ## Compute the distance matrix according to the chosen similarity metric and perform
  ## a hierarchical clustering
  if (similarity_metric=="pearson") {

    mat=log10(filtered_dataf)
    distmat=(as.matrix(1 - stats::cor(t(mat), use = missing_values, method = similarity_metric)))

    tryCatch({
      clust <- stats::hclust(stats::as.dist(distmat), method = agglomeration_method)
    }, error = function(e) {
      if (grepl("NA/NaN/Inf", e$message)) {
        stop("Error in hierarchical clustering: NA/NaN/Inf values found in the distance matrix.\n",
             "Try a higher time point cut-off (i.e. '-c' parameter of cmd line).")
      } else {
        stop(e)
      }
    })


  } else if (similarity_metric=="dtw") {

    ## Linear interpolation if threshold is NA value
    linear_interpolation= sapply(data.table::data.table(t(filtered_dataf)), imputeTS::na_interpolation)
    mat=log10(linear_interpolation)
    if(proxy::pr_DB$entry_exists("dtw_basic") == FALSE){
      # Just a way to add dtwclust functions to the registry
      proxy::pr_DB$set_entry(FUN=(dtwclust::dtw_basic), names=c("dtw_basic_3"))
    }

    if (is.null(dtw_norm)) {
      stop("You must provide a value for 'dtw_norm' when using similarity_metric = 'dtw'. Choose either 'L1' or 'L2'.")
    }

    distmat = proxy::dist(t(mat), method = "dtw_basic", normalize = TRUE, norm=dtw_norm)
    clust <- stats::hclust(stats::as.dist(distmat),method=agglomeration_method )
    tryCatch({
      clust <- stats::hclust(stats::as.dist(distmat), method = agglomeration_method)
    }, error = function(e) {
      if (grepl("NA/NaN/Inf", e$message)) {
        stop("Error in hierarchical clustering: NA/NaN/Inf values found in the distance matrix.\n",
             "Try a higher time point threshold (i.e. '-c' parameter of cmd line).")
      } else {
        stop(e)
      }
    })
  }

  ## Plot dendrogram:
  stats::as.dendrogram(clust) -> dend

  grDevices::postscript(paste(output_directory, "/", input_name,"_", similarity_metric, ".eps",sep=""),width = 5.5,height = 5)
  #output_filename <- paste(output_directory, input_name, "_", similarity_metric, ".png", sep = "")
  #png(output_filename, width = 5.5, height = 5)
  
  # Storing old par()
  oldpar <- graphics::par(no.readonly = TRUE)
  # Restore old par() before exiting the function
  on.exit(graphics::par(oldpar))
  
  
  graphics::par(mar = c(2,2,2,2))
  
  

  ## Plot heatmap:
  gplots::heatmap.2(distmat,Rowv = dend,Colv = dend,col=rev(color_palette),density.info = "none",trace = "none",
                    key.xlab="(1 - r)",cexRow = 0.5,cexCol = 0.5, labRow = FALSE, labCol = FALSE)
  grDevices::dev.off()

  rm(distmat)

  ## For all thresholds between 0.1 and max height of hierarchical clustering, we extract the relative
  ## clusters. This step allows the user to visualize the possible clusters and
  ## make an informed choice for the threshold.
  range<- seq(from=0.1, to=max(clust$height), by=0.01)
  cluster_file=list()
  for( i in 1:length(range)){
    cut_avg <- as.data.frame(stats::cutree(clust, h=range[i]))
    names(cut_avg)[1]=range[i]
    cluster_file[[i]]=cut_avg
  }

  cluster_file=do.call(cbind,cluster_file)

  return(cluster_file)
}
