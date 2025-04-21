
#' Constituency election results
#'
#' Returns a list with details of the constituency and a tibble
#'  with election results.
#' @param constituency_id The ID of the constituency to return the data for.
#' If `NULL`, no data is returned. Defaults to `NULL`.
#' @param election_id The ID of the election to return the data for. Defaults
#' to 0, which returns the result of all elections held in that constituency.
#' @inheritParams mnis_basic_details
#' @return A list with details of the constituency, labelled `'details'`
#' and a tibble with election results, labelled `'results'`. The list and
#' tibble are stored in a single object.
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_constituency_results(constituency_id = 3709, election_id = 0)
#' }
#'
mnis_constituency_results <- function(constituency_id = NULL, election_id = 0,
                                      tidy = TRUE, tidy_style = "snake_case") {
  if (missing(constituency_id)) {
    stop("'constituency_id' cannot be empty", call. = FALSE)
  }

  query <- paste0(base_url, "ConstituencyResults/",
                  as.character(constituency_id), "/",
                  as.character(election_id), "/")

  got <- mnis_query(query)

  details <- got$Constituency$Details

  results <- tibble::as_tibble(got$Constituency$Results)

  ret_list <- list()

  if (tidy == TRUE) {
    ret_list <- mnis::constituency_results_tidy(results, details)
  } else {
    ret_list <- c(list(results = results), list(details = details))
  }
  ret_list
}
