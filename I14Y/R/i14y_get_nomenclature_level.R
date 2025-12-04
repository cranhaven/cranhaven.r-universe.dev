#' Export a level of a nomenclature
#'
#' @param identifier string. The identifier of dcat dataset.
#' @param language string. The language of the response data.
#' @param level integer. The level to export. Default 1.
#' @param format string. The format of the export (CSV or XLSX).
#' @param annotations boolean. Include annotations. Default FALSE.
#' @param filters object. The filters
#'
#' @return a tibble
#' @export
i14y_get_nomenclature_level <- function(
  identifier = NULL,
  language = "de",
  level = 1,
  format = "csv",
  annotations = FALSE,
  filters = NULL
) {
  check_not_null(identifier)
  check_not_null(language)
  check_not_null(level)
  check_not_null(format)
  check_string(identifier)
  check_string(language)
  check_string(format)
  check_integer(level)
  language <- arg_match(language, c("de", "fr", "en", "it"))
  format <- arg_match(format, c("csv", "xlsx"))
  # TODO: validate args "annotations" and "filters"
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
    paste0("/api/Nomenclatures/", identifier, "/levelexport/", format)
  )
  req <- httr2::req_url_query(
    req,
    identifier = identifier,
    format = format,
    level = level,
    annotations = annotations,
    filters = filters,
    language = language
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  if (format == "csv") {
    resp <- httr2::resp_body_string(req)
    tbl <- readr::read_csv(resp, show_col_types = FALSE)
    return(tbl)
  }
  if (format == "xlsx") {
    cli::cli_abort("The  format 'xlsx' has not yet been implemented.")
  }
  return(req)
}
