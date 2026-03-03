#' Query Deutschlandatlas indicators
#'
#' @param table Table id (default "p_apo_f_ZA2022").
#' @param params Query parameters for the ArcGIS service.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Deutschlandatlas API is backed by an ArcGIS feature service. You must
#' supply a `where` filter and output format `f` (usually "json"). Official docs:
#' https://github.com/AndreasFischer1985/deutschlandatlas-api.
#'
#' @seealso
#' [bunddev_parameters()] to inspect available query parameters.
#'
#' @examples
#' \dontrun{
#' deutschlandatlas_query(
#'   table = "p_apo_f_ZA2022",
#'   params = list(
#'     where = "1=1",
#'     outFields = "*",
#'     f = "json",
#'     returnGeometry = "false",
#'     resultRecordCount = 5
#'   )
#' )
#' }
#'
#' @return A tibble with indicator records.
#' @export
deutschlandatlas_query <- function(table = "p_apo_f_ZA2022",
                                   params = list(),
                                   safe = TRUE,
                                   refresh = FALSE,
                                   flatten = FALSE,
                                   flatten_mode = "json") {
  if (is.null(params$where)) {
    cli::cli_abort("'where' is required for deutschlandatlas_query().")
  }
  if (is.null(params$f)) {
    params$f <- "json"
  }
  if (is.null(params$outFields)) {
    params$outFields <- "*"
  }
  if (is.null(params$returnGeometry)) {
    params$returnGeometry <- "false"
  }

  if (is.list(params$geometry)) {
    params$geometry <- jsonlite::toJSON(params$geometry, auto_unbox = TRUE)
  }

  params$table <- table

  response <- bunddev_call(
    "deutschlandatlas",
    "query",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- deutschlandatlas_tidy_response(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = c("geometry"), mode = flatten_mode))
  }

  data
}

deutschlandatlas_tidy_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }
  if (!is.null(response$error)) {
    message <- response$error$message %||% "ArcGIS request failed."
    cli::cli_abort(message)
  }

  features <- response$features
  if (is.null(features) || length(features) == 0) {
    return(tibble::tibble())
  }

  sanitize_name <- function(name) {
    ascii <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
    ascii <- stringr::str_replace_all(ascii, "[^A-Za-z0-9]+", "_")
    ascii <- stringr::str_replace_all(ascii, "^_+|_+$", "")
    tolower(ascii)
  }

  rows <- purrr::map(features, function(feature) {
    attrs <- feature$attributes %||% list()
    geometry <- feature$geometry %||% list()
    attrs$geometry <- geometry
    attrs
  })

  data <- dplyr::bind_rows(rows)
  names(data) <- vapply(names(data), sanitize_name, character(1))
  data
}

bunddev_deutschlandatlas_query <- function(table = "p_apo_f_ZA2022",
                                           params = list(),
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  deutschlandatlas_query(
    table = table,
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
