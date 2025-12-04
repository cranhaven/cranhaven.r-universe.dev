#' Search the catalog for datasets, data services and public services.
#'
#' @param language string. The language to use for the search
#' @param query string. The search query
#' @param accessRights vector of strings. Only results with one of the
#' specified access rights (PUBLIC, NON_PUBLIC, RESTRICTED) are returned
#' @param formats vector of strings. Only results with at least one distribution
#' providing one of the specified formats are returned
#' @param publishers vector of strings. Only results with one of the specified
#' publishers are returned
#' @param statuses vector of strings. Only results with one of the specified
#' registration statuses are returned
#' @param themes vector of strings. Only results corresponding to one of the
#' specified themes are returned
#' @param types vector of strings. Only results with one of the specified types
#' (Dataset, DataService, PublicService) are returned
#' @param page integer. The number of the result page to return
#' @param pageSize integer. The size of each result page
#'
#' @return a tibble
#' @export
#'
#' @examples
#' i14y_search_catalog(query = "noga")
i14y_search_catalog <- function(
  query = NULL,
  language = "de",
  accessRights = NULL,
  formats = NULL,
  publishers = NULL,
  statuses = NULL,
  themes = NULL,
  types = NULL,
  page = NULL,
  pageSize = NULL
) {
  check_not_null(language)
  language <- arg_match(language, c("de", "fr", "en", "it"))
  # TODO: validate args
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  req <- httr2::request("https://input.i14y.admin.ch/api/Catalog/search")
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  # req <- httr2::req_url_path_append(req, "/api/Catalog/search")
  req <- httr2::req_url_query(
    req,
    language = language,
    query = query,
    accessRights = accessRights,
    formats = formats,
    publishers = publishers,
    statuses = statuses,
    themes = themes,
    types = types,
    page = page,
    pageSize = pageSize
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  tbl <- tibble::as_tibble(resp)
  return(tbl)
}
