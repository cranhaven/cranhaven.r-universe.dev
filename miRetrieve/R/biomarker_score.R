#' Keywords - biomarkers.
#'
#' Keywords to identify abstracts reporting about miRNAs as biomarkers.
#' @export
biomarker_keywords <- c('circulating', 'biomarker', 'bio-marker',
                        'extracellular vesicles','urine',
                        'urinary', 'exosomes', 'exosomal',
                        'body fluid', "bodyfluid", 'diagnostic', 'biological marker',
                        'biomarker', 'bio-marker', 'biomarker',
                        'bio-marker', 'serum', 'plasma')

#' Calculate biomarker scores for abstracts
#'
#' Calculate biomarker score for each abstract to indicate possible
#' use of miRNAs as biomarker.
#'
#' Calculate biomarker score for each abstract to indicate possible
#' use of miRNAs as biomarker. This score is added to the data frame as an additional
#' column `Biomarker_score`, containing the calculated biomarker score.
#' To decide which abstracts are considered to contain use of miRNAs as biomarker, a threshold
#' can be set via the `threshold` argument. Furthermore, an additional
#' column can be added, verbally indicating the general use of miRNAs as biomarker in
#' an abstract.
#' Choosing the right threshold can be facilitated using `plot_score_biomarker()`.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The score is
#' calculated based on these keywords. How much weight a keyword in `keywords`
#' carries is determined by how often it is present in `keywords`, e.g. if
#' a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' The predefined keywords can be accessed via `miRetrieve::biomarker_keywords`.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param threshold Integer. Optional. Threshold to decide if use of miRNAs as
#' biomarker are present in an abstract or not. If `indicate = TRUE` or `discard = TRUE`
#' and `threshold` not specified, `threshold` is automatically set to `1`.
#' @param indicate Boolean. If `indicate = TRUE`, an extra column is added. This
#' extra column contains "Yes" or "No", indicating the use of miRNAs as biomarker
#' in abstracts.
#' @param discard Boolean. If `TRUE`, only abstracts are kept where
#' miRNAs as biomarker.
#' @param col.abstract Symbol. Column containing abstracts.
#'
#' @return Data frame with calculated biomarker scores.
#' If `discard = FALSE`, adds extra columns
#' to the original data frame with calculated biomarker scores.
#' If `discard = TRUE`, only abstracts are with miRNAs as biomarker
#' are kept.
#'
#' @seealso [plot_score_biomarker()]
#'
#' @family score functions
#'
#' @export
calculate_score_biomarker<- function(df,
                                    keywords = biomarker_keywords,
                                    case = FALSE,
                                    threshold = NULL,
                                    indicate = FALSE,
                                    discard = FALSE,
                                    col.abstract = Abstract) {
  if(!is.null(threshold) & !is.numeric(threshold)) {
    stop("'threshold' must be an integer >= 0")
  }

  if(indicate == TRUE & is.null(threshold)) {
    threshold <- 1
  }

  if(discard == TRUE & is.null(threshold)) {
    threshold <- 1
  }

  df_biomarker <- df %>%
    dplyr::mutate(Biomarker_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                         keywords = keywords,
                                                                         case = case)))

  if (indicate == TRUE) {
    df_biomarker <- df_biomarker %>%
      dplyr::mutate(Biomarker_p = ifelse(Biomarker_score >= threshold, "Yes", "No"))
  }

  if (discard == TRUE) {
    df_biomarker <- df_biomarker %>%
      dplyr::filter(Biomarker_score >= threshold)

    return(df_biomarker)
  } else {
    return(df_biomarker)
  }
}

#' Plot frequency of biomarker scores in abstracts
#'
#' Plot frequency of biomarker scores in abstracts.
#'
#' Plots a frequency distribution of biomarker scores in abstracts of a
#' data frame. The biomarker score is influenced by the choice of
#' terms in `keywords`.
#' Plotting the distribution can help deciding if the
#' terms are well-chosen, or in choosing the right threshold to decide
#' which abstracts are considered to contain use of miRNAs as biomarker.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The biomarker
#' score is calculated based on these keywords. How much weight a keyword
#' in `keywords` carries is determined how often it is present in `keywords`,
#' e.g. if a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param bins Integer. Specifies how many bins are used to plot
#' the distribution. If `bins = NULL`, bins are calculated over the whole
#' range of scores, with one bin per score.
#' @param colour String. Colour of histogram.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Histogram displaying the distribution of biomarker scores in abstracts.
#'
#' @seealso [calculate_score_biomarker()]
#'
#' @family score functions
#'
#' @export
plot_score_biomarker <- function(df,
                                 keywords = biomarker_keywords,
                                 case = FALSE,
                                 bins = NULL,
                                 colour = "steelblue3",
                                 col.abstract = Abstract,
                                 col.pmid = PMID,
                                 title = NULL) {

  if(is.null(title)) {
    title <- "Biomarker score distribution"
  }

  df_biomarker <- df %>%
    dplyr::mutate(Biomarker_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                         keywords = keywords,
                                                                         case = case)))
  if (is.null(bins)) {
    bins <- max(df_biomarker$Biomarker_score) - min(df_biomarker$Biomarker_score)
  }

  df_biomarker <- df_biomarker %>%
    dplyr::select({{col.pmid}}, Biomarker_score) %>%
    dplyr::distinct()

  plot <- ggplot(df_biomarker, aes(x = Biomarker_score)) +
    geom_histogram(bins = bins,
                   fill = colour,
                   center = 0,
                   binwidth = 1) +
    theme_classic() +
    xlab("Biomarker score") +
    ylab("# of abstracts") +
    ggtitle(title) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

  return(plot)
}
