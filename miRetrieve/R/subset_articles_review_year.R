#' Subset data frame for abstracts of research articles
#'
#' Subset data frame for abstracts of research articles only.
#'
#' Subset data frame for abstracts of research articles only.
#' At the same time, abstracts from other article types such as *Review*,
#' *Letter*, etc. are dropped.
#'
#' @param df Data frame containing article types.
#' @param col.type Symbol. Column containing articles types.
#'
#' @return Data frame containing abstracts of research articles only.
#'
#' @seealso [subset_review()], [subset_year()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_research <- function(df,
                            col.type = Type) {
  #Get PMIDs of non-original research articles
  pmid_review <- df %>%
    tidyr::unnest({{col.type}}) %>%
    dplyr::filter({{col.type}} == "Review") %>%
    dplyr::select(PMID) %>%
    dplyr::pull()

  pmid_letter <- df %>%
    tidyr::unnest({{col.type}}) %>%
    dplyr::filter({{col.type}} == "Letter") %>%
    dplyr::select(PMID) %>%
    dplyr::pull()

  pmid_retracted <- df %>%
    tidyr::unnest({{col.type}}) %>%
    dplyr::filter({{col.type}} == "Retracted") %>%
    dplyr::select(PMID) %>%
    dplyr::pull()

  pmid_combined <- c(pmid_review,
                     pmid_letter,
                     pmid_retracted) %>%
    unique()

  #Filter for original research Articles
  df <- df %>%
    dplyr::filter(!PMID %in% pmid_combined) %>% #filter out PMIDs with reviews, letters, retracted
    tidyr::unnest({{col.type}}) %>%
    dplyr::filter({{col.type}} == "Journal Article") #keep original Journal Articles

  return(df)
}

#' Subset data frame for abstracts of review articles
#'
#' Subset data frame for abstracts of review articles only.
#'
#' Subset data frame for abstracts of review articles only.
#' At the same time, abstracts from other article types such as *Journal Article*,
#' *Letter*, etc. are dropped.
#
#'
#' @param df Data frame containing article types.
#' @param col.type Symbol. Column containing articles types.
#'
#' @return Data frame containing abstracts of review articles only.
#'
#' @seealso [subset_research()], [subset_year()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_review <- function(df, col.type = Type) {
  # Keep review articles
  df <- df %>%
    tidyr::unnest({{col.type}}) %>%
    dplyr::filter({{col.type}} == "Review")

  return(df)
}

#' Subset data frame for abstracts published in a specific period
#'
#' Subset data frame for abstracts published in a specific period only.
#'
#' Subset data frame for abstracts published in a specific period only.
#' All other abstracts published not within this period are silently
#' dropped.
#'
#' @param df Data frame containing publication years.
#' @param col.year Symbol. Column containing publication years.
#' @param start Integer. Optional. Beginning of
#' publication period.
#' If `start = NULL`, `start` is set to the least recent year in `df`.
#' @param end Integer. Optional. End of
#' publication period.
#' If `end = NULL`, `end` is set to the most recent year in `df`.
#'
#' @return Data frame containing abstracts published in a specific period
#' only.
#'
#' @seealso [subset_research()], [subset_review()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_year <- function(df,
                        col.year = Year,
                        start = NULL,
                        end = NULL) {
  if(is.null(start)) {
    start <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      min()
  }

  if(is.null(end)) {
    end <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      max()
  }

  if(start > end) {
    stop("'start' must be equal to or smaller than 'end'.")
  }

  # Keep articles between two years
  df <- df %>%
    dplyr::filter(dplyr::between({{col.year}}, start, end))

  return(df)
}
