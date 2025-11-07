#' Keywords - animals.
#'
#' Keywords to identify abstracts using animal models.
#' @export
animal_keywords <- c('mice', 'mouse', ' rats ',
                      '. rats ', ' rat ', 'elegans',
                      'zebrafish', 'horse', 'rabbit',
                      'porcine', 'piglet', 'monkey')


#' Calculate animal model scores for abstracts
#'
#' Calculate animal model score for each abstract to indicate possible
#' use of animal models.
#'
#' Calculate animal model score for each abstract to indicate possible
#' use of animal models. This score is added to the data frame as an additional
#' column `Animal_score`, containing the calculated animal model score.
#' To decide which abstracts are considered to contain animal models, a threshold
#' can be set via the `threshold` argument. Furthermore, an additional
#' column can be added, verbally indicating the use of animal models in
#' an abstract.
#' Choosing the right threshold can be facilitated using `plot_score_animals()`.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The score is
#' calculated based on these keywords. How much weight a keyword in `keywords`
#' carries is determined by how often it is present in `keywords`, e.g. if
#' a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' The predefined keywords can be accessed via `miRetrieve::animal_keywords`.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param threshold Integer. Optional. Threshold to decide if an abstract is
#' considered to use animal models or not. If `indicate = TRUE` or `discard = TRUE`
#' and `threshold` is not specified, `threshold` is automatically set to `1`.
#' @param indicate Boolean. If `indicate = TRUE`, an extra column is added. This
#' extra column contains "Yes" or "No", indicating the use of animal models
#' in abstracts.
#' @param discard Boolean. If `discard = TRUE`, only abstracts are kept where
#' animal models are present.
#' @param col.abstract Symbol. Column containing abstracts.
#'
#' @return Data frame with calculated animal model scores.
#' If `discard = FALSE`, adds extra columns
#' to the original data frame with the calculated animal model scores.
#' If `discard = TRUE`, only abstracts with animal models are kept.
#'
#' @seealso [plot_score_animals()]
#'
#' @family score functions
#'
#' @export
calculate_score_animals <- function(df,
                                    keywords = animal_keywords,
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

  df_animals <- df %>%
    dplyr::mutate(Animal_score = purrr::map_int({{col.abstract}},
                                                ~ calculate_score(string = .x,
                                                                  keywords = keywords,
                                                                  case = case)))

  if (indicate == TRUE) {
    df_animals <- df_animals %>%
      dplyr::mutate(Animal_p = ifelse(Animal_score >= threshold, "Yes", "No"))
  }

  if (discard == TRUE) {
    df_animals <- df_animals %>%
      dplyr::filter(Animal_score >= threshold)

    return(df_animals)
  } else {
    return(df_animals)
  }
}

#' Plot frequency of animal model scores in abstracts
#'
#' Plot frequency of animal model scores in abstracts.
#'
#' Plots a frequency distribution of animal model scores in abstracts of a
#' data frame. The animal model score is influenced by the choice of
#' terms in `keywords`.
#' Plotting the distribution can help deciding if the
#' terms are well-chosen, or in choosing the right threshold to decide
#' which abstracts are considered to contain animal models.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The animal
#' model score is calculated based on these keywords. How much weight a keyword
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
#' @return Histogram displaying the distribution of animal scores in abstracts.
#'
#' @seealso [calculate_score_animals()]
#'
#' @family score functions
#'
#' @export
plot_score_animals <- function(df,
                               keywords = animal_keywords,
                               case = FALSE,
                               bins = NULL,
                               colour = "steelblue3",
                               col.abstract = Abstract,
                               col.pmid = PMID,
                               title = NULL) {

  if(is.null(title)) {
    title <- "Animal models score distribution"
  }

  df_animals <- df %>%
    dplyr::mutate(Animal_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                      keywords = keywords,
                                                                      case = case)))
  if (is.null(bins)) {
    bins <- max(df_animals$Animal_score) - min(df_animals$Animal_score)
  }

  df_animals <- df_animals %>%
    dplyr::select({{col.pmid}}, Animal_score) %>%
    dplyr::distinct()

  plot <- ggplot(df_animals, aes(x = Animal_score)) +
    geom_histogram(bins = bins,
                   fill = colour,
                   center = 0,
                   binwidth = 1) +
    theme_classic() +
    xlab("Animal model score") +
    ylab("# of abstracts")+
    ggtitle(title) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

  return(plot)
}
