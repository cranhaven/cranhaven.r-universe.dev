#' Assign topics based on precalculated scores
#'
#' Assign topics to abstracts based on precalculated scores.
#'
#' Assign topics to abstracts based on precalculated scores.
#' `assign_topic()` compares different precalculated topic scores and
#' assigns the abstract to the topic with the highest score. If there is a
#' tie between topic scores, the abstract is assigned to all topics in question.
#' If an abstract matches no topic, it is assigned to the topic "Unknown".
#'
#' @param df Data frame containing precalculated topic scores and PubMed-IDs.
#' @param col.topic Character vector. Vector with column names containing
#' precalculated topic scores.
#' @param threshold Integer vector. Vector containing thresholds for topic
#' columns. Positions in `threshold` correspond to positions in
#' `col.topic`.
#' @param topic.names Character vector. Optional. Vector containing names of new
#' topics. Positions in `topic.names` correspond to positions in
#' `col.topic`. If `topic.names` is not provided, `col.topic` is used
#' to name the new topics.
#' @param col.topic.name String. Name of the new topic column.
#' @param col.pmid String. Column containing PubMed-IDs.
#' @param discard Boolean. If `discard = TRUE`, only abstracts with
#' a newly assigned topic are kept. Abstracts without a newly assigned topic
#' are discarded.
#'
#' @return Data frame with topics based on precalculated
#' topic scores.
#'
#' @seealso [calculate_score_topic()], [plot_score_topic()],
#' [add_col_topic()]
#'
#' @family score functions
#'
#' @importFrom rlang :=
#'
#' @export
assign_topic <- function(df,
                         col.topic,
                         threshold,
                         topic.names = NULL,
                         col.topic.name = "Topic",
                         col.pmid = "PMID",
                         discard = FALSE) {

  . = NULL

  if(is.null(topic.names)) {
    topic.names <- col.topic
  }

  if(!length(col.topic) == length(threshold) | !length(col.topic) == length(topic.names)) {
    stop("col.topic, threshold, topic.names must be vectors of equal length")
  }

  # copies df to overwrite score values
  df_copy <- df

  colnames_df <- colnames(df)

  if(col.topic.name %in% colnames_df) {
    col.topic.name <- paste0(col.topic.name, ".assigned")
  }

  #set value to 10,000 if value is below score for topic
  for (i in seq_along(col.topic)) {
    df_copy <- df_copy %>%
      dplyr::mutate(!!sym(col.topic[i]) := ifelse(!!sym(col.topic[i]) >= threshold[i], !!sym(col.topic[i]), 10000))
  }

  # if all values of all topics are "unknown", they should match this number
  unknown_score <- 10000 * length(col.topic)

  # filters for abstracts with unknown topics
  df_unknown <- df_copy %>%
    dplyr::select(col.topic) %>%
    dplyr::mutate(sums = rowSums(.)) %>%
    dplyr::mutate(!!sym(col.topic.name) := ifelse(sums == unknown_score, "Unknown", "open")) %>%
    cbind(df_copy[[col.pmid]]) %>% # adds original PMID column
    dplyr::rename(!!sym(col.pmid) := dim(.)[2]) %>% #renames last column to "PMID"
    dplyr::select(!!sym(col.pmid), !!sym(col.topic.name)) %>%
    dplyr::filter(!!sym(col.topic.name) == "Unknown") %>%
    dplyr::distinct()

  # PMIDs of abstracts with unknown topics

  unknown_pmid <- df_unknown[[col.pmid]]

  # sorts abstracts to topics

  df_topic <- df %>%
    dplyr::filter(!df[[col.pmid]] %in% unknown_pmid) %>%
    dplyr::select(!!sym(col.pmid), col.topic) %>%
    dplyr::distinct()

  for (i in seq_along(col.topic)) {
    df_topic <- df_topic %>%
      dplyr::rename(!!sym(topic.names[i]) := col.topic[i])
  }

  for (i in seq_along(col.topic)) {
    df_topic <- df_topic %>%
      dplyr::mutate(!!sym(col.topic[i]) := ifelse(!!sym(col.topic[i]) >= threshold[i],
                                                  !!sym(col.topic[i]),
                                                  10000))
  }


  df_topic <- df_topic %>%
    tidyr::gather("key", "value", -PMID) %>%
    dplyr::filter(value != 10000) %>%
    dplyr::group_by(!!sym(col.pmid)) %>%
    dplyr::mutate(max_score = max(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!sym(col.topic.name) := ifelse(value == max_score, key, "No topic")) %>%
    dplyr::filter(!!sym(col.topic.name) != "No topic") %>%
    dplyr::select(!!sym(col.pmid), !!sym(col.topic.name))

  df_join <- rbind(df_unknown, df_topic)

  df_end <- dplyr::left_join(df, df_join)

  if(discard == TRUE) {
    df_end <- df_end %>%
      dplyr::filter(!!sym(col.topic.name) != "Unknown")
  }

  return(df_end)

}
