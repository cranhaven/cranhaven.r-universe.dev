#' Helper - Detect term in string.
#'
#' Detect term in string. Helper function.
#'
#' Detect term in string. Helper function.
#'
#' @param string String. Specifies string in which to look for term.
#' @param term String. Specifies which term to look for in string.
#' @param case Boolean. If case = TRUE, matches term case sensitive. If case =
#' FALSE, matches term case insensitive.
#'
#' @return Integer indicating term score.
#'
#' @noRd
detect_term <- function(string,
                        term,
                        case) {
  if(case != TRUE & case != FALSE) {
    stop("'case' must either be 'TRUE' or 'FALSE'.")
  }

  if(case == FALSE) {
    term_score <- stringr::str_match_all(stringr::str_to_lower(string),
                                         stringr::str_to_lower(term)) %>%
                  purrr::flatten() %>%
                  length()
  } else if (case == TRUE) {
    term_score <- stringr::str_match_all(string,
                                         term) %>%
                  purrr::flatten() %>%
                  length()
  }
  return(term_score)
}

#' Indicate if a term is contained in abstracts
#'
#' Indicate if a term is contained in abstracts.
#'
#' Indicate if a term is contained in an abstract. Terms provided can either
#' be case sensitive or insensitive. Per term, a new column is added to the data
#' frame indicating if the term is present in an abstract. Furthermore, if a term
#' is considered "present" in an abstract can be regulated via the `threshold`
#' argument. `threshold` determines how often a term must be in an abstract
#' to be considered "present".
#'
#' @param df Data frame containing abstracts.
#' @param term Character vector. Vector containing terms to indicate.
#' @param threshold Integer. Sets how often a term must be in an abstract to be
#' considered "present".
#' @param case Boolean. If `case = TRUE`, strings contained in `term` are case
#' sensitive. If `case = FALSE`, strings contained in `term` are case insensitive.
#' @param discard Boolean. If `discard = TRUE`, only abstracts containing the
#' terms in `term` are kept.
#' @param col.abstract Symbol. Column containing abstracts.
#'
#' @return Data frame. If `discard = FALSE`, the original data frame with additional
#' columns per term is returned. If `discard = TRUE`, only abstracts containing the
#' terms in `term` are returned.
#'
#' @seealso [indicate_mir()]
#'
#' @family indicate functions
#'
#' @importFrom rlang :=
#'
#' @export
indicate_term <- function(df,
                          term,
                          threshold = 1,
                          case = FALSE,
                          discard = FALSE,
                          col.abstract = Abstract) {

  . = NULL

  for (term_word in term) {
    col.term <- stringr::str_c("Term_", term_word)

    df <- df %>%
      dplyr::mutate(term_score = purrr::map_int({{col.abstract}}, ~ detect_term(string = .x,
                                                                  term = term_word,
                                                                  case = case))) %>%
      dplyr::mutate({{col.term}} := ifelse(term_score >= threshold, "Yes", "No")) %>%
      dplyr::select(-term_score)
  }

  if (discard == FALSE) {
    return(df)
  } else {

    col.term <- stringr::str_c("Term_", term)

    df <- df %>%
      dplyr::filter_at(col.term, dplyr::any_vars(. == "Yes"))

    return(df)
  }
}
