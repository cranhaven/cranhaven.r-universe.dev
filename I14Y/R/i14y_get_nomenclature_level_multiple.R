#' Export multiple levels of a nomenclature
#'
#' @param identifier string. The identifier of dcat dataset.
#' @param levelFrom integer. The first level to include.
#' @param levelTo integer. The last level to include.
#' @param format string. The format of the export ("csv" or "xlsx"). Default "csv".
#' @param language string. The language of the response data. Default "de".
#' @param annotations boolean. Include annotations
#' @param filters object. The filters
#'
#' @return a tibble
#' @export
#'
#' @examples
#' i14y_get_nomenclature_level_multiple(
#'   identifier = "HCL_CH_ISCO_19_PROF",
#'   format = "csv", # read internally
#'   levelFrom = 1,
#'   levelTo = 2,
#'   language = "fr"
#'  )
i14y_get_nomenclature_level_multiple <- function(
  identifier = NULL,
  language = "de",
  levelFrom = NULL,
  levelTo = NULL,
  format = "csv",
  annotations = NULL,
  filters = NULL
) {
  check_not_null(identifier)
  check_not_null(language)
  check_not_null(levelFrom)
  check_not_null(levelTo)
  check_not_null(format)
  check_string(identifier)
  check_string(language)
  check_string(format)
  check_integer(levelFrom)
  check_integer(levelTo)
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
    paste0("/api/Nomenclatures/", identifier, "/multiplelevels/", format)
  )
  req <- httr2::req_url_query(
    req,
    identifier = identifier,
    format = format,
    language = language,
    levelFrom = levelFrom,
    levelTo = levelTo,
    annotations = annotations,
    filters = filters
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  if (format == "csv") {
    resp <- httr2::resp_body_string(req)
    tbl <- readr::read_csv(resp, show_col_types = FALSE)
    return(tbl)
  }
  if (format == "xlsx") {
    stop("The format 'xlsx' has not yet been implemented.")
  }
  return(req)
}
