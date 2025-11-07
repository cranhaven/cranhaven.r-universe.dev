#' Get miRNA names as a character vector - helper
#'
#' Get miRNA names as a character vector of a data frame.
#'
#' Get miRNA names as a character vector of a data frame. How often a miRNA
#' name must appear in the data frame to be returned can be regulated via
#' the `threshold` argument. `threshold` can either
#' be an absolute value, e.g. 3, or a float between 0 and 1, e.g. 0.2.
#' If `threshold` is an absolute value, `get_mir()` returns only the miRNA names
#' mentioned in at least `threshold` abstracts.
#' If `threshold` is a float between 0 and 1, `get_mir()` returns
#' only miRNA names mentioned in at least `threshold` abstracts
#' of all abstracts.
#'
#' @param df Data frame containing miRNA names and PubMed-IDs.
#' @param threshold Integer or float. If `threshold` >= 1, return
#' miRNA names mentioned in at least `threshold` abstracts.
#' If `threshold` is between 0 and 1, return miRNA names mentioned
#' in at least `threshold` abstracts of all abstracts in `df`.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Character vector containing miRNA names.
#'
#' @seealso [get_mir_top()], [combine_mir()]
#'
#' @family get functions
#'
#' @importFrom magrittr %>%
#' @noRd
get_mir_threshold <- function(df,
                    threshold = NULL,
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
    dplyr::add_count({{col.mir}}, name = "n") %>%
    dplyr::mutate(total = n_row) %>%
    dplyr::mutate(perc = n / n_row)

  if(threshold < 0.9999 & threshold > 0.0000001) {
    mirna_filtered <- df_count %>%
      dplyr::filter(perc >= threshold) %>%
      dplyr::select({{col.mir}}) %>%
      dplyr::pull() %>%
      unique()

    return(mirna_filtered)
  } else {
    mirna_filtered <- df_count %>%
      dplyr::filter(n >= threshold) %>%
      dplyr::select({{col.mir}}) %>%
      dplyr::pull() %>%
      unique()

    return(mirna_filtered)
  }
}

#' Get most frequent miRNA names - helper
#'
#' Get most frequent miRNA names in a data frame.
#'
#' Get most frequent miRNA names in a data frame. How many miRNA names
#' are returned is regulated via the `top` argument. Ties among the most
#' frequently mentioned miRNAs are treated as
#' the same rank, e.g. if *miR-126*, *miR-34*, and *miR-29* were all mentioned
#' the most often with the same frequency, they would all be returned by
#' specifying `top = 1`, `top = 2`, and `top = 3`.
#'
#' @param df Data frame containing miRNA names.
#' @param top Integer. Specifies number of most frequent miRNA names to return.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Character vector containing miRNA names.
#'
#' @seealso [get_mir()], [combine_mir()]
#'
#' @family get functions
#'
#' @importFrom magrittr %>%
#' @noRd
get_mir_top <- function(df,
                        top = 5,
                        col.mir = miRNA) {
  top_mirnas <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::select({{col.mir}}, n) %>%
    dplyr::distinct() %>%
    dplyr::top_n(top, n) %>%
    dplyr::select({{col.mir}}) %>%
    dplyr::pull()

  return(top_mirnas)
}

#' Get miRNA names from a data frame
#'
#' Get miRNA names from a data frame. These miRNA names can either be the most
#' frequent ones, or the ones exceeding a threshold.
#'
#' Get miRNA names from a data frame. These miRNA names can either be the most
#' frequent ones, or the ones exceeding a threshold. Furthermore, if the data
#' frame contains abstracts of different topics, only the miRNA names of
#' specific topics can be obtained by setting the `topic` argument.
#'
#' * To get the most frequent miRNA names, set the `top` argument. `top`
#' determines how many most frequent miRNA names are returned, according to their
#' rank. Ties among the most
#' frequently mentioned miRNAs are treated as
#' the same rank, e.g. if *miR-126*, *miR-34*, and *miR-29* were all mentioned
#' the most often with the same frequency, they would all be returned by
#' specifying `top = 1`, `top = 2`, and `top = 3`.
#'
#' * To get the miRNA names exceeding a threshold, set the `threshold` argument.
#' `threshold` can either be an absolute value, e.g. 3, or a float between 0 and 1,
#' e.g. 0.2.
#' If `threshold` is an absolute value, `get_mir()` returns only the miRNA names
#' mentioned in at least `threshold` abstracts.
#' If `threshold` is a float between 0 and 1, `get_mir()` returns
#' only miRNA names mentioned in at least `threshold` abstracts
#' of all abstracts. `threshold` requires the data frame to have a column with
#' PubMed IDs.
#'
#' If neither `top` nor `threshold` is set, `top` is automatically set to `5`.
#'
#' @param df Data frame containing miRNA names. If `threshold` is set,
#' `df` must also contain PubMed-IDs. If `topic` is set, `df` must also contain
#' topic names.
#' @param top Integer. Optional. Specifies number of most frequent miRNA names
#' to return. If neither `top` nor `threshold` is set, `top` is automatically set
#' to `5`.
#' @param threshold Integer or float. Optional. If `threshold` >= 1, return
#' miRNA names mentioned in at least `threshold` abstracts.
#' If `threshold` is between 0 and 1, return miRNA names mentioned
#' in at least `threshold` abstracts of all abstracts in `df`.
#' @param topic String. Optional. Character vector specifying which topics to obtain
#' miRNA names from.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param col.topic Symbol. Column containing topic names.
#'
#' @return Character vector containing miRNA names.
#'
#' @family get functions
#'
#' @export

get_mir <- function(df,
                    top = NULL,
                    threshold = NULL,
                    topic = NULL,
                    col.mir = miRNA,
                    col.pmid = PMID,
                    col.topic = Topic) {

  if(!is.null(threshold) & !is.null(top)) {
    stop("'top' and 'threshold' cannot be set simultaneously.
         Please choose either 'top' or 'threshold'.")
  }

  if(is.null(threshold) & is.null(top)) {
    top <- 5
  }

  if(!is.null(topic) & length(topic) == 1) {
    df <- df %>%
      dplyr::filter({{col.topic}} == topic)
  }

  if(!is.null(topic) & length(topic) > 1) {
    df <- df %>%
      dplyr::filter({{col.topic}} %in% topic)
  }

  if(!is.null(top)) {
    top_mirnas <- get_mir_top(df = df,
                top = top,
                col.mir = {{col.mir}})

    return(top_mirnas)
  } else if(!is.null(threshold)) {
    thresh_mirnas <- get_mir_threshold(df = df,
                      threshold = threshold,
                      col.mir = {{col.mir}},
                      col.pmid = {{col.pmid}})
    return(thresh_mirnas)
  }
}
