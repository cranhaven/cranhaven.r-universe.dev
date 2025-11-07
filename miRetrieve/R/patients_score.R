#' Keywords - patients.
#'
#' Keywords to identify abstracts investigating miRNAs in patients.
#'
#' @export
patients_keywords <- c('patients', 'individuals', 'subjects',
                      'biopsies', 'biopsy', 'participant',
                      'children', 'trial', 'humans', 'women')


#' Calculate patients scores for abstracts
#'
#' Calculate patients score for each abstract to indicate possible
#' use of patient material.
#'
#' Calculate patient score for each abstract to indicate possible
#' use of patient material. This score is added to the data frame as an additional
#' column `Patient_score`, containing the calculated patients score.
#' To decide which abstracts are considered to contain patient material, a threshold
#' can be set via the `threshold` argument. Furthermore, an additional
#' column can be added, verbally indicating the general use of patient material.
#' Choosing the right threshold can be facilitated using `plot_score_patients()`.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The score is
#' calculated based on these keywords. How much weight a keyword in `keywords`
#' carries is determined by how often it is present in `keywords`, e.g. if
#' a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' The predefined keywords can be accessed via `miRetrieve::patients_keywords`.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param threshold Integer. Optional. Threshold to decide if use of patient tissue is
#' present in an abstract or not. If `indicate = TRUE` or `discard = TRUE`
#' and `threshold` not specified, `threshold` is automatically set to `1`.
#' @param indicate Boolean. If `indicate = TRUE`, an extra column is added. This
#' extra column contains "Yes" or "No", indicating the use of patient tissue
#' in abstracts.
#' @param discard Boolean. If `discard = TRUE`, only abstracts are kept where
#' use of patient tissue is present.
#' @param col.abstract Symbol. Column containing abstracts.
#'
#' @return Data frame with calculated patient scores.
#' If `discard = FALSE`, adds extra columns
#' to the original data frame with the calculated patient tissue scores.
#' If `discard = TRUE`, only abstracts with use of patient tissue
#' are kept.
#'
#' @seealso [plot_score_patients()]
#'
#' @family score functions
#'
#' @export
calculate_score_patients <- function(df,
                                     keywords = patients_keywords,
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
  df_patient <- df %>%
    dplyr::mutate(Patient_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                       keywords = keywords,
                                                                       case = case)))

  if (indicate == TRUE) {
    df_patient <- df_patient %>%
      dplyr::mutate(Patient_p = ifelse(Patient_score >= threshold, "Yes", "No"))
  }

  if (discard == TRUE) {
    df_patient <- df_patient %>%
      dplyr::filter(Patient_score >= threshold)

    return(df_patient)
  } else {
    return(df_patient)
  }
}

#' Plot frequency of patient scores in abstracts
#'
#' Plot frequency of patient scores in abstracts.
#'
#' Plots a frequency distribution of patient scores in abstracts of a
#' data frame. The patient score is influenced by the choice of
#' terms in `keywords`.
#' Plotting the distribution can help deciding if the
#' terms are well-chosen, or in choosing the right threshold to decide
#' which abstracts are considered to contain patient material
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The score is
#' calculated based on these keywords. How much weight a keyword in `keywords`
#' carries is determined how often it is present in `keywords`, e.g. if
#' a keyword is mentioned twice in `keywords` and it is mentioned only once in
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
#' @return Histogram displaying the distribution of patient scores in abstracts.
#'
#' @seealso [calculate_score_patients()]
#'
#' @family score functions
#'
#' @export
plot_score_patients <- function(df,
                                keywords = patients_keywords,
                                case = FALSE,
                                bins = NULL,
                                colour = "steelblue3",
                                col.abstract = Abstract,
                                col.pmid = PMID,
                                title = NULL) {

  if(is.null(title)) {
    title <- "Patient score distribution"
  }

  df_patients <- df %>%
    dplyr::mutate(Patient_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                       keywords = keywords,
                                                                       case = case)))
  if (is.null(bins)) {
    bins <- max(df_patients$Patient_score) - min(df_patients$Patient_score)
  }

  df_patients <- df_patients %>%
    dplyr::select({{col.pmid}}, Patient_score) %>%
    dplyr::distinct()

  plot <- ggplot(df_patients, aes(x = Patient_score)) +
    geom_histogram(bins = bins,
                   fill = colour,
                   center = 0,
                   binwidth = 1) +
    theme_classic() +
    xlab("Patient score") +
    ylab("# of abstracts")+
    ggtitle(title) +
    scale_x_continuous(expand = c(0,0),
                       breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0,0))

  return(plot)
}
