#' Search within a nomenclature
#'
#' @param identifier string. The identifier of dcat dataset.
#' @param query string. The search query.
#' @param language string. The language of the response data.
#' @param page integer. The number of the result page to return
#' @param pageSize integer. The size of each result page
#' @param filters object. The filters
#'
#' @return a list
#' @export
i14y_search_nomenclature <- function(
  identifier = NULL,
  query = NULL,
  language = "de",
  page = NULL,
  pageSize = NULL,
  filters = NULL
) {
  check_not_null(identifier)
  check_not_null(query)
  check_not_null(language)
  check_string(identifier)
  check_string(query)
  check_string(language)
  language <- arg_match(language, c("de", "fr", "en", "it"))
  # TODO: validate args
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  req <- httr2::request("https://www.i14y.admin.ch")
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  req <- httr2::req_url_path_append(
    req,
    paste0("/api/Nomenclatures/", identifier, "/search")
  )
  req <- httr2::req_url_query(
    req,
    identifier = identifier,
    query = query,
    language = language,
    page = page,
    pageSize = pageSize,
    filters = filters
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  return(resp)
}
