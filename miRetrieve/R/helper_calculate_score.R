#' Calculate score for keywords - helper.
#'
#' Helper function. Calculates score for keywords.
#'
#' @param string String. String to search for keywords.
#' @param keywords Character vector. Vector containing keywords to
#' search for in string.
#' @param case Boolean. If `case = TRUE`, keywords are case sensitive.
#' If `case = FALSE`, keywords are case insensitive.
#'
#' @return Integer corresponding keyword score
#' @noRd
calculate_score <- function(string,
                            keywords,
                            case = FALSE) {

    if(case == FALSE) {
        score <- stringr::str_match_all(stringr::str_to_lower(string),
                                        stringr::str_to_lower(keywords)) %>%
            purrr::flatten() %>%
            length()
    } else if (case == TRUE) {
        score <- stringr::str_match_all(string, keywords) %>%
            purrr::flatten() %>%
            length()
    }


  return(score)
}
