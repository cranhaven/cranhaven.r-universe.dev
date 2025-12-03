#' Create a CDF for the pgRNA normalized counts
#' @description This function uses pivot_longer to rearrange the data for plotting
#'  and then plots a CDF of the normalized counts
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function
#' which contains the transformed data
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer unite
#' @importFrom ggplot2 ggplot labs
#' @return counts_cdf a ggplot
#' @export
#' @examples \dontrun{
#'
#' gimap_dataset <- get_example_data("gimap")
#' qc_cdf(gimap_dataset)
#'}
qc_cdf <- function(gimap_dataset, wide_ar = 0.75) {
  long_form <-
    tidyr::pivot_longer(data.frame(gimap_dataset$transformed_data$log2_cpm),
      everything(),
      names_to = "sample",
      values_to = "count_normalized"
    )

  counts_cdf <- ggplot(long_form, aes(x = count_normalized, color = sample)) +
    stat_ecdf() +
    labs(
      x = "-log10(count/total_count)",
      y = "Expected_pgRNAs",
      color = "Sample"
    ) +
    plot_options() +
    plot_theme() +
    theme(aspect.ratio = wide_ar)

  return(counts_cdf)
}

#' Create a histogram for the pgRNA log2 CPMs, faceted by sample
#' @description This function uses pivot_longer to rearrange the data for plotting and then
#' plots sample specific histograms of the pgRNA cpm's
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function
#' which contains the transformed data
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @return sample_cpm_histogram a ggplot
#' @export
#' @examples \dontrun{
#' gimap_dataset <- get_example_data("gimap")
#' qc_sample_hist(gimap_dataset)
#' }
qc_sample_hist <- function(gimap_dataset, wide_ar = 0.75) {
  long_form <-
    tidyr::pivot_longer(data.frame(gimap_dataset$transformed_data$log2_cpm),
      everything(),
      names_to = "sample",
      values_to = "log2_cpm"
    )

  sample_cpm_histogram <- ggplot(long_form, aes(x = log2_cpm, fill = sample)) +
    geom_histogram(color = "black", binwidth = 0.5) +
    plot_options() +
    plot_theme() +
    theme(
      aspect.ratio = wide_ar,
      legend.position = "none"
    ) +
    facet_wrap(~sample, scales = "free_y", ncol = ceiling(ncol(gimap_dataset$raw_counts) / 2))

  return(sample_cpm_histogram)
}

#' Create a histogram for the variance within replicates for each pgRNA
#' @description This function uses pivot_longer to rearrange the data for plotting,
#'  finds the variance for each pgRNA construct (using row number as a proxy) and then
#'  plots a histogram of these variances
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function
#' which contains the transformed data
#' @param filter_replicates_target_col default is NULL; Which sample columns are
#' replicates whose variation you'd like to analyze; If NULL, the last 3 sample columns are used
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom stats var
#' @import ggplot2
#' @import dplyr
#' @return a ggplot histogram
#' @export
#' @examples \dontrun{
#' gimap_dataset <- get_example_data("gimap")
#' qc_variance_hist(gimap_dataset)
#' }
qc_variance_hist <- function(gimap_dataset, filter_replicates_target_col = NULL, wide_ar = 0.75) {
  if (is.null(filter_replicates_target_col)) {
    filter_replicates_target_col <- c(
      (ncol(gimap_dataset$transformed_data$log2_cpm) - 2):ncol(gimap_dataset$transformed_data$log2_cpm)
    )
  } # last 3 columns of the data

  return(
    gimap_dataset$transformed_data$log2_cpm[, filter_replicates_target_col] %>%
      as.data.frame() %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_longer(-row) %>%
      group_by(row) %>%
      dplyr::summarize(var = var(value)) %>%
      ggplot(aes(x = var)) +
      geom_histogram(binwidth = 0.1) +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        aspect.ratio = wide_ar
      ) +
      xlab("variance") +
      ylab("pgRNA construct count")
  )
}

#' Create a bar graph that shows the number of replicates with a zero count for pgRNA constructs flagged by the zero count filter
#' @description This bar graph first uses the specified `filter_zerocount_target_col` columns to flag pgRNA constructs
#'  that have a raw count of 0 in any one of those columns/samples of interest.
#' Then, it looks at the specified columns for the final day/sample replicates
#' (`filter_replicates_target_col`) to see for pgRNAs that were flagged by the filter,
#' how many of those replicate samples had raw counts of zeros. And it produces a bar plot reporting on this.
#' Note, if you select samples/columns to check with the filter that don't have the
#'  replicate samples, this graph won't be informative. So you want there to be overlap
#'   between the columns for the two target_col parameters to have an informative graph
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function
#' which contains the transformed data
#' @param filter_zerocount_target_col default is NULL; Which sample column(s)
#' should be used to check for counts of 0? If NULL and not specified, downstream analysis will select all sample columns
#' @param filter_replicates_target_col default is NULL; Which sample columns are
#' replicates whose variation you'd like to analyze; If NULL, the last 3 sample columns are used
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#' @return a ggplot barplot
#' @export
#' @examples \dontrun{
#' gimap_dataset <- get_example_data("gimap")
#' qc_constructs_countzero_bar(gimap_dataset)
#'
#' # or if you want to select a specific column(s) for
#' # looking at where/which samples zero counts are present for
#' qc_constructs_countzero_bar(gimap_dataset, filter_zerocount_target_col = 3:5)
#'
#' # or if you want to select a specific column(s) for the final day/sample replicates
#' qc_constructs_countzero_bar(gimap_dataset, filter_replicates_target_col = 3:5)
#'
#' # or some combination of those
#' qc_constructs_countzero_bar(gimap_dataset,
#'   filter_zerocount_target_col = 3:5,
#'   filter_replicates_target_col = 3:5
#' )
#' }
qc_constructs_countzero_bar <- function(gimap_dataset,
                                        filter_zerocount_target_col = NULL,
                                        filter_replicates_target_col = NULL,
                                        wide_ar = 0.75) {
  if (is.null(filter_zerocount_target_col)) {
    filter_zerocount_target_col <- c(1:ncol(gimap_dataset$raw_counts))
  }

  if (!all(filter_zerocount_target_col %in% 1:ncol(gimap_dataset$raw_counts))) {
    stop("The columns selected do not exist. `filter_zerocount_target_col` needs to correspond to the index of the columns in `gimap_dataset$raw_counts` that you need to filter by")
  }

  qc_filter_output <- qc_filter_zerocounts(gimap_dataset, filter_zerocount_target_col = filter_zerocount_target_col)

  if (is.null(filter_replicates_target_col)) {
    filter_replicates_target_col <- c((ncol(gimap_dataset$transformed_data$log2_cpm) - 2):ncol(gimap_dataset$transformed_data$log2_cpm))
  } # last 3 columns of the data

  if (!all(filter_replicates_target_col %in% 1:ncol(gimap_dataset$transformed_data$log2_cpm))) {
    stop("The columns selected do not exist. `filter_replicates_target_col` needs
         to correspond to the index of the columns in `gimap_dataset$transformed_data$log2_cpm` that you need to filter by")
  }


  return(
    gimap_dataset$raw_counts[qc_filter_output$filter, filter_replicates_target_col] %>%
      as.data.frame() %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_longer(colnames(gimap_dataset$raw_counts)[filter_replicates_target_col],
        values_to = "counts"
      ) %>%
      group_by(row) %>%
      summarize(numzero = sum(counts == 0)) %>%
      group_by(numzero) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = numzero, y = count)) +
      geom_bar(stat = "identity") +
      theme_classic() +
      ylab("Number of pgRNAs") +
      xlab("Number of replicates with a zero") +
      geom_text(aes(label = count, group = numzero), vjust = -0.5, size = 2)
  )
}

#' Create a correlation heatmap for the pgRNA CPMs
#' @description This function uses the `cor` function to find correlations between the
#'  sample CPM's and then plots a heatmap of these
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @importFrom magrittr %>%
#' @importFrom pheatmap pheatmap
#' @importFrom stats cor
#' @return `sample_cor_heatmap` a pheatmap
#' @export
#' @examples \dontrun{
#' gimap_dataset <- get_example_data("gimap")
#' qc_cor_heatmap(gimap_dataset)
#' }
qc_cor_heatmap <- function(gimap_dataset) {
  cpm_cor <- gimap_dataset$transformed_data$cpm %>%
    cor() %>%
    round(2) %>%
    data.frame()

  sample_cor_heatmap <-
    pheatmap::pheatmap(cpm_cor,
      border_color = "white",
      cellwidth = 20,
      cellheight = 20,
      treeheight_row = 20,
      treeheight_col = 20
    )

  return(sample_cor_heatmap)
}

#' Create a histogram with plasmid log2 CPM values and ascertain a cutoff for low values
#' @description Find the distribution of plasmid (day0 data) pgRNA log2 CPM values, and ascertain a cutoff or filter for low log2 CPM values.
#' Assumes the first column of the dataset is the day0 data; do I need a better
#' method to tell, especially if there are reps?
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @param cutoff default is NULL, the cutoff for low log2 CPM values for the plasmid time period; if not specified, The lower outlier (defined by taking the difference of the lower quartile and 1.5 * interquartile range) is used
#' @param filter_plasmid_target_col default is NULL, and if NULL, will select the first column only; this parameter specifically should be used to specify the plasmid column(s) that will be selected
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @export
#' @import ggplot2
#' @return a ggplot histogram
#' @examples \dontrun{
#'
#' gimap_dataset <- get_example_data("gimap")
#'
#' qc_plasmid_histogram(gimap_dataset)
#'
#' # or to specify a "cutoff" value that will be displayed as a dashed vertical line
#' qc_plasmid_histogram(gimap_dataset, cutoff = 1.75)
#'
#' # or to specify a different column (or set of columns) to select
#' qc_plasmid_histogram(gimap_dataset, filter_plasmid_target_col = 1:2)
#'
#' # or to specify a "cutoff" value that will be displayed as a dashed vertical
#' # line as well as to specify a different column (or set of columns) to select
#' qc_plasmid_histogram(gimap_dataset, cutoff = 2, filter_plasmid_target_col = 1:2)
#' }
qc_plasmid_histogram <- function(gimap_dataset,
                                 cutoff = NULL,
                                 filter_plasmid_target_col = NULL,
                                 wide_ar = 0.75) {
  if (is.null(filter_plasmid_target_col)) {
    filter_plasmid_target_col <- c(1)
  }

  if (!all(filter_plasmid_target_col %in% 1:ncol(gimap_dataset$transformed_data$log2_cpm))) {
    stop("The columns selected do not exist. `filter_plasmid_target_col` needs to correspond to the index of the columns in `gimap_dataset$transformed_data$log2_cpm` that you need to filter by")
  }

  to_plot <- data.frame(gimap_dataset$transformed_data$log2_cpm[, filter_plasmid_target_col]) %>%
    `colnames<-`(rep(c("plasmid_log2_cpm"), length(filter_plasmid_target_col))) %>%
    clean_names()

  if (length(filter_plasmid_target_col > 1)) { # if more than one column was selected, collapse all of the columns into the same vector and store in a df to plot
    to_plot <- data.frame(unlist(to_plot %>% select(starts_with("plasmid_log2_cpm")), use.names = FALSE)) %>% `colnames<-`(c("plasmid_log2_cpm"))
  }

  quantile_info <- quantile(to_plot$plasmid_log2_cpm)

  if (is.null(cutoff)) {
    cutoff <- quantile_info["25%"] - (1.5 * (quantile_info["75%"] - quantile_info["25%"]))
  }
  # if cutoff is null, suggest a cutoff and plot with suggested

  return(
    ggplot(to_plot, aes(x = plasmid_log2_cpm)) +
      geom_histogram(binwidth = 0.2, color = "black", fill = "gray60") +
      plot_options() +
      plot_theme() +
      theme(aspect.ratio = wide_ar) +
      geom_vline(xintercept = cutoff, linetype = "dashed")
  )
}


#' Standardized plot theme
#' @description this is a ggplot2 standardized plot theme for this package
#' @export
#' @return A ggplot2 theme that can be used on the plots.
plot_theme <- function() {
  theme(
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(color = "black")
  )
}
