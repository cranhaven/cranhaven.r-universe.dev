#' Calculate scores of a self-chosen topic
#'
#' Calculate score of a self-chosen topic for each abstract to identify
#' abstracts possibly corresponding to the topic of interest.
#'
#' Calculate score of a self-chosen topic for each abstract to identify
#' abstracts possibly corresponding to the topic of interest.
#' This score is added to the data frame as an additional
#' column, usually called `topic_score`, containing the calculated topic score.
#' If there is more than one topic of interest, the column `topic_score` should
#' be appropriately renamed.
#' To decide which abstracts are considered to correspond to the topic of interest,
#' a threshold
#' can be set via the `threshold` argument. Furthermore, an additional
#' column can be added, verbally indicating if the abstract corresponds to the
#' topic.
#' Choosing the right threshold can be facilitated using `plot_score_topic()`.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. The score is
#' calculated based on these keywords. How much weight a keyword in `keywords`
#' carries is determined by how often it is present in `keywords`, e.g. if
#' a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param col.score String. Name of `topic_score` column.
#' @param col.indicate String. Optional. Name of indicating column. If a string
#' is provided, an extra column is added to `df`, indicating if the abstract
#' corresponds to the topic of interest by "Yes" or "No".
#' @param threshold Integer. Optional. Threshold to decide if abstract
#' corresponds to topic of interest. If `col.topic` is specified or `discard = TRUE`
#' without `threshold` being specified, `threshold` is automatically set to `1`.
#' @param discard Boolean. If `discard = TRUE`, only abstracts are kept that
#' correspond to the topic of interest.
#' @param col.abstract Symbol. Column containing abstracts.
#'
#' @return Data frame with calculated topic scores.
#' If `discard = FALSE`, adds extra columns
#' to the original data frame with the calculated topic scores.
#' If `discard = TRUE`, only abstracts corresponding to
#' the topic of interest are kept.
#'
#' @seealso [assign_topic()], [plot_score_topic()]
#'
#' @family score functions
#'
#' @importFrom rlang :=
#'
#' @export
calculate_score_topic<- function(df,
                                keywords,
                                case = FALSE,
                                col.score = "topic_score",
                                col.indicate = NULL,
                                threshold = NULL,
                                discard = FALSE,
                                col.abstract = Abstract) {

  if(!is.null(col.indicate) & is.null(threshold)) {
    threshold <- 1
  }

  if(discard == TRUE & is.null(threshold)) {
    threshold <- 1
  }

  df_topic <- df %>%
    dplyr::mutate({{col.score}} := purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                     keywords = keywords,
                                                                     case = case)))

  if (!is.null(col.indicate)) {
    df_topic <- df_topic %>%
      dplyr::mutate({{col.indicate}} := ifelse(!!sym(col.score) >= threshold, "Yes", "No"))
  }

  if (discard == TRUE) {
    df_topic <- df_topic %>%
      dplyr::filter(!!sym(col.score) >= threshold)

    return(df_topic)
  } else {
    return(df_topic)
  }
}

#' Plot frequency of self-chosen topic scores in abstracts
#'
#' Plot frequency of self-chosen topic scores in abstracts.
#'
#' Plots a frequency distribution of self-chosen topic scores in abstracts of a
#' data frame. The topic score is influenced by the choice of
#' terms in `keywords`. Plotting the distribution can help in choosing the right
#' threshold to decide which abstracts correspond to the self-chosen
#' topic.
#'
#' @param df Data frame containing abstracts.
#' @param keywords Character vector. Vector containing keywords. How much weight
#' a keyword in `keywords` carries is determined by how often it is present in
#' `keywords`, e.g. if a keyword is mentioned twice in `keywords` and it is mentioned only once in
#' an abstract, it adds 2 points to the score.
#' @param case Boolean. If `case = TRUE`, terms contained in `keywords` are case
#' sensitive. If `case = FALSE`, terms contained in `keywords` are case insensitive.
#' @param name.topic String. Name of the topic.
#' @param bins Integer. Specifies how many bins are used to plot
#' the distribution. If `bins = NULL`, bins are calculated over the whole
#' range of scores, with one bin per score.
#' @param colour String. Colour of histogram.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Histogram displaying the distribution of self-chosen topic scores in
#' abstracts.
#'
#' @seealso [calculate_score_topic()], [assign_topic()]
#'
#' @family score functions
#'
#' @export
plot_score_topic <- function(df,
                             keywords,
                             case = FALSE,
                             name.topic = "TOPIC",
                             bins = NULL,
                             colour = "steelblue3",
                             col.abstract = Abstract,
                             col.pmid = PMID,
                             title = NULL) {

  if(is.null(title)) {
    title <- "Topic score distribution"
  }

  df_topic <- df %>%
    dplyr::mutate(topic_score = purrr::map_int({{col.abstract}}, ~ calculate_score(string = .x,
                                                                     keywords = keywords,
                                                                     case = case)))
  if (is.null(bins)) {
    bins <- max(df_topic$topic_score) - min(df_topic$topic_score)
  }

  df_topic <- df_topic %>%
    dplyr::select({{col.pmid}}, topic_score) %>%
    dplyr::distinct()

  plot <- ggplot(df_topic, aes(x = topic_score)) +
    geom_histogram(bins = bins,
                   fill = colour,
                   center = 0,
                   binwidth = 1) +
    theme_classic() +
    xlab(stringr::str_c("Score for topic '", name.topic, "'")) +
    ylab("# of abstracts")+
    ggtitle(title) +
    scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

  return(plot)
}
