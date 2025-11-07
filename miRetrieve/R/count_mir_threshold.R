#' Count occurrence of miRNA names above threshold
#'
#' Count occurrence of miRNA names above a threshold.
#'
#' Count occurrence of miRNA names above a threshold. This threshold can either
#' be an absolute value, e.g. 3, or a float between 0 and 1, e.g. 0.2.
#' If `threshold` is an absolute value, number of distinct miRNA names mentioned
#' in at least `threshold` abstracts is returned.
#' If `threshold` is a float between 0 and 1, number of distinct miRNA names
#' mentioned in at least `threshold` abstracts
#' of all abstracts in `df` is returned.
#'
#' @param df Data frame containing miRNA names and
#' PubMed-IDs.
#' @param threshold Integer or float. If `threshold >= 1`, counts number of
#' miRNA names in at least `threshold` abstracts.
#' If `threshold` is between 0 and 1, counts number of miRNA names mentioned
#' in at least `threshold` abstracts of all abstracts in `df`.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Integer with the number of distinct miRNA names in `df`.
#'
#' @seealso [plot_mir_count_threshold()], [count_mir()], [plot_mir_count()]
#'
#' @family count functions
#'
#' @export
#'
#' @importFrom magrittr %>%
count_mir_threshold <- function(df,
                                threshold = 1,
                               col.mir = miRNA,
                               col.pmid = PMID) {

  if(threshold < 0){
    stop("Threshold must be equal or greater than 0.")
  }

  n_row <- df %>%
    dplyr::select({{col.pmid}}) %>%
    dplyr::pull() %>%
    unique() %>%
    length()

  df_count <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::mutate(total = n_row) %>%
    dplyr::mutate(perc = n / n_row)

  if(threshold < 0.9999 & threshold > 0.0000001) {
    df_count <- count_mir_perc_threshold(df = df_count,
                                         threshold = threshold,
                                         col.mir = {{col.mir}}) # Helper function miRetrieve

  } else {
    df_count <- count_mir_total_threshold(df = df_count,
                                          threshold = threshold,
                                          col.mir = {{col.mir}}) # Helper function miRetrieve
  }
  return(df_count)
}

#' Plot occurrence count of miRNA names over different thresholds
#'
#' Plot occurrence count of distinct miRNA names over different thresholds.
#'
#' Plot occurrence of distinct miRNA names over different thresholds.
#' These thresholds can either be absolute values or floating values between 0
#' and 1.
#' If the thresholds are absolute values, number of distinct miRNA names
#' mentioned in at least n abstracts are plotted, where n
#' is the range of thresholds defined by `start` and `end`.
#' If the thresholds are floating values, `bins` must be specified as well.
#' Then the umber of distinct miRNA names
#' mentioned in at least n abstracts over `bins` are plotted, where n is the
#' range of thresholds
#' between `start` and `end`.
#' Overall, plotting can help in identifying if the abstracts
#' at hand mention different miRNAs in a balanced way, or if there are few miRNAs
#' dominating the field.
#'
#' @param df Data frame containing columns with miRNAs and PubMed-IDs.
#' @param start Integer or float. Must be greater than 0 and smaller than
#' `end`.
#' @param end Integer or float. Must be greater than 0 and greater than
#' `start`.
#' If `start` >= 1, `plot_mir_count_threshold()` plots
#' number of miRNAs above different absolute thresholds, ranging from
#' `start` to `end`.
#' If `start` >= 0 and `end` <= 1, `bins` must be specified. If `bins` is
#' not specified, `bins` is automatically set to `10`.
#' `plot_mir_count_threshold()` then plots number of miRNAs above different
#' thresholds, ranging from `start` to `end` in n `bins`.
#' If `start` >= 0 and `end` <= 1 and the value of `start` is too low for
#' the number of miRNAs to be plotted, `plot_mir_count_threshold()` raises
#' a warning, suggesting a more appropriate `start` value.
#' @param bins Integer. Optional. Only necessary if `start` >=0
#' and `end` <=1. Specifies number of bins between `start` and
#' `end`. If `start` >= 0, `end` <= 1, and `bins` is
#' not specified, `bins` is automatically set to `10`.
#' @param colour String. Colour of bar plot.
#' @param col.mir Symbol. Column containing miRNAs.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Bar plot counting the occurrence of miRNA names above different
#' thresholds.
#'
#' @seealso [count_mir_threshold()], [count_mir()], [plot_mir_count()]
#'
#' @family count functions
#'
#' @export
#'
#' @importFrom magrittr %>%
plot_mir_count_threshold <- function(df,
                                     start = 1,
                                     end = 5,
                                     bins = NULL,
                                    colour = "steelblue3",
                                    col.mir = miRNA,
                                    col.pmid = PMID,
                                    title = NULL) {

  if(is.null(title)) {
    title <- "Number of miRNAs/count threshold"
  }

  if(start < 0){
    stop("Threshold must be equal or greater than 0.")
  }

  if(start > end) {
    stop("'start' must be smaller than 'end'.")
  }

  if(start >= 0 & start < 1 & end > 1) {
    stop("'end' must be <= 1 if 'start' is between 0 and 1.")
  }

  if(start >=1 & !is.null(bins)) {
    warning("'bins' cannot be specified if 'start' >= 1. Ignoring 'bins'.")
    bins <- NULL
  }

  if(start >= 0 & end <= 1 & is.null(bins)) {
    bins <- 10
  }

  if(!is.null(bins)) {
    if(bins < 1) {
      stop("'bins' must be >= 1")
    }
  }


  n_row <- df %>%
    dplyr::select({{col.pmid}}) %>%
    dplyr::pull() %>%
    unique() %>%
    length()

  df_count <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::mutate(total = n_row) %>%
    dplyr::mutate(perc = n / total)

  max_perc <- df_count %>%
    dplyr::select(perc) %>%
    dplyr::pull() %>%
    max()

  if(end < 1 & start > max_perc) {
    warning(paste0("The most frequently mentioned miRNA is found in ",
                   round(max_perc, 2),
                   "% of all abstracts, while 'start' was set to ", start,
                   ". It is recommended to set 'start' to at least ",
                   round(max_perc, 2), " to obtain reliable results."))
  }

  number_mirnas <- c()
  thresholds <- c()

  if(start >= 0 & end < 1) {
    spacings <- ((end - start) / bins)
    for (i in seq(start, end, spacings)) {
      number_mirna_threshold <- count_mir_perc_threshold(df_count,
                                                         threshold = i,
                                                         col.mir = {{col.mir}}) # Helper function miRetrieve

      number_mirnas <- c(number_mirnas, number_mirna_threshold)
      thresholds <- c(thresholds, i) }
  } else {
    for (i in seq(start, end, 1)) {
      number_mirna_threshold <- count_mir_total_threshold(df_count,
                                                          threshold = i,
                                                          col.mir = {{col.mir}}) # Helper function miRetrieve

      number_mirnas <- c(number_mirnas, number_mirna_threshold)
      thresholds <- c(thresholds, i) }
  }
  df_count <- data.frame("number_mirnas" = number_mirnas,
                         "threshold" = thresholds) %>%
    dplyr::as_tibble()

  plot <- ggplot(df_count, aes(x = threshold,
                               y = number_mirnas)) +
    geom_col(fill = colour) +
    xlab("Threshold, # of abstracts mentioning at least # of miRNAs") +
    ylab("# of distinct miRNAs") +
    theme_classic() +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_count$number_mirnas)

  if(start >= 0 & end < 1) {
    plot <- plot +
      scale_x_continuous(labels = scales::percent) +
      xlab("Threshold, % of abstracts mentioning at least # of miRNAs")
  }

  return(plot)
}
