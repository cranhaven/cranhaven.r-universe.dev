#' Get Concept view entity by ID
#'
#' @param id string. The Id of the response data.
#' @param language string. The language of the response data.
#'
#' @return a list
#' @export
#'
#' @examples
#' i14y_get_concept(
#'  id = "08d94604-e058-62a2-aa25-53f84b974201", # DV_NOGA_DIVISION
#'  language = "en"
#' )
i14y_get_concept <- function(
  id = NULL,
  language = "de"
) {
  check_not_null(id)
  check_not_null(language)
  check_string(id)
  check_string(language)
  language <- arg_match(language, c("de", "fr", "en", "it"))
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  req <- httr2::request("https://i14y.admin.ch")
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  req <- httr2::req_url_path_append(req, paste0("/api/conceptView/", id))
  req <- httr2::req_url_query(req, language = language)
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  return(resp)
}
