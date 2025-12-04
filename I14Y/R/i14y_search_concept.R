#' Search for a Concept Summary
#'
#' @param query string. Search query.
#' @param language string. The language of the response data.
#' @param page integer. The number of the result page to return.
#' @param pageSize integer. The size of each result page.
#' @param publishers vector of strings. Filter with Publishers identifiers.
#' @param themes vector of strings. Filter with theme codes.
#' @param conceptValueTypes character vector. One or more concept value types to filter by.
#' @param registrationStatuses character vector. One or more registration statuses to filter by.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' i14y_search_concept(query = "noga", language = "en")
i14y_search_concept <- function(
  query = NULL,
  language = "de",
  page = 1,
  pageSize = 1000,
  publishers = NULL,
  themes = NULL,
  conceptValueTypes = NULL,
  registrationStatuses = NULL
) {
  check_not_null(language)
  check_not_null(page)
  check_not_null(pageSize)
  check_integer(page)
  check_integer(pageSize)
  language <- rlang::arg_match(language, c("de", "fr", "en", "it"))
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  if (!is.null(conceptValueTypes)) {
    valid_concept_value_types <- c("CodeList", "Date", "Numeric", "String")
    conceptValueTypes <- rlang::arg_match(
      conceptValueTypes,
      valid_concept_value_types
    )
  }

  if (!is.null(registrationStatuses)) {
    valid_registration_statuses <- c(
      "Superseded",
      "PreferredStandard",
      "Incomplete",
      "Candidate",
      "Qualified",
      "Recorded",
      "Standard"
    )
    registrationStatuses <- rlang::arg_match(
      registrationStatuses,
      valid_registration_statuses
    )
  }

  req <- httr2::request("https://input.i14y.admin.ch/api/Catalog/search")
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  req <- httr2::req_url_query(
    req,
    language = language,
    page = page,
    pageSize = pageSize,
    query = query,
    publishers = publishers,
    themes = themes,
    types = "Concept",
    conceptValueTypes = conceptValueTypes,
    registrationStatuses = registrationStatuses
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  tbl <- tibble::as_tibble(resp)
  return(tbl)
}
