#' Get the information of a nomenclature by identifier
#'
#' @param identifier string. The identifier of the nomenclature.
#'
#' @return a list
#' @examples
#' i14y_get_content_information(
#'   identifier = "HCL_CH_ISCO_19_PROF"
#' )
#' @export
i14y_get_content_information <- function(
  identifier = NULL
) {
  check_not_null(identifier)
  check_string(identifier)
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
    paste0("/api/ContentConfigurations/", identifier)
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  return(resp)
}
