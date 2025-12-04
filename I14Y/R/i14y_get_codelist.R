#' Export a codelist
#'
#' The function uses the I14Y Console API:
#' <https://apiconsole.i14y.admin.ch/public/v1/index.html>.
#'
#' @param id string. The Id of the response data.
#' @param format string. The format of the export ("csv" or "json").
#'
#' @return a tibble
#' @export
#'
#' @examples
#' i14y_get_codelist(
#'   id = "08d94604-e058-62a2-aa25-53f84b974201" # for DV_NOGA_DIVISION
#' )
i14y_get_codelist <- function(
  id = NULL,
  format = "csv"
) {
  check_not_null(id)
  check_not_null(format)
  check_string(id)
  check_string(format)
  format <- arg_match(format, c("csv", "json"))
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  req <- httr2::request("https://api.i14y.admin.ch")
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  req <- httr2::req_url_path_append(
    req,
    paste0("/api/public/v1/concepts/", id, "/codelist-entries/exports/", format)
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  if (format == "csv") {
    resp <- httr2::resp_body_string(req)
    tbl <- readr::read_csv(resp, show_col_types = FALSE)
    return(tbl)
  }
  if (format == "json") {
    resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
    tbl <- tibble::as_tibble(resp)
    return(tbl)
  }
}
