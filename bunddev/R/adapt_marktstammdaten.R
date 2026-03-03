#' List MaStR filter options for electricity generation
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns filter definitions for public electricity generation data.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_filters_stromerzeugung()
#' }
#'
#' @return A tibble with filter metadata.
#' @export
marktstammdaten_filters_stromerzeugung <- function(safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetFilterColumnsErweiterteOeffentlicheEinheitStromerzeugung",
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_filters(response), flatten, flatten_mode)
}

#' List MaStR filter options for electricity consumption
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns filter definitions for public electricity consumption data.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_filters_stromverbrauch()
#' }
#'
#' @return A tibble with filter metadata.
#' @export
marktstammdaten_filters_stromverbrauch <- function(safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetFilterColumnsErweiterteOeffentlicheEinheitStromverbrauch",
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_filters(response), flatten, flatten_mode)
}

#' List MaStR filter options for gas generation
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns filter definitions for public gas generation data.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_filters_gaserzeugung()
#' }
#'
#' @return A tibble with filter metadata.
#' @export
marktstammdaten_filters_gaserzeugung <- function(safe = TRUE,
                                                 refresh = FALSE,
                                                 flatten = FALSE,
                                                 flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetFilterColumnsErweiterteOeffentlicheEinheitGaserzeugung",
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_filters(response), flatten, flatten_mode)
}

#' List MaStR filter options for gas consumption
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns filter definitions for public gas consumption data.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_filters_gasverbrauch()
#' }
#'
#' @return A tibble with filter metadata.
#' @export
marktstammdaten_filters_gasverbrauch <- function(safe = TRUE,
                                                 refresh = FALSE,
                                                 flatten = FALSE,
                                                 flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetFilterColumnsErweiterteOeffentlicheEinheitGasverbrauch",
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_filters(response), flatten, flatten_mode)
}

#' List MaStR electricity generation data
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns public electricity generation data from the MaStR.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_stromerzeugung(params = list(page = 1, pageSize = 5))
#' }
#'
#' @return A tibble with MaStR entries.
#' @export
marktstammdaten_stromerzeugung <- function(params = list(),
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetErweiterteOeffentlicheEinheitStromerzeugung",
    params = params,
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_entries(response), flatten, flatten_mode)
}

#' List MaStR electricity consumption data
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns public electricity consumption data from the MaStR.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_stromverbrauch(params = list(page = 1, pageSize = 5))
#' }
#'
#' @return A tibble with MaStR entries.
#' @export
marktstammdaten_stromverbrauch <- function(params = list(),
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetErweiterteOeffentlicheEinheitStromverbrauch",
    params = params,
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_entries(response), flatten, flatten_mode)
}

#' List MaStR gas generation data
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns public gas generation data from the MaStR.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_gaserzeugung(params = list(page = 1, pageSize = 5))
#' }
#'
#' @return A tibble with MaStR entries.
#' @export
marktstammdaten_gaserzeugung <- function(params = list(),
                                         safe = TRUE,
                                         refresh = FALSE,
                                         flatten = FALSE,
                                         flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetErweiterteOeffentlicheEinheitGaserzeugung",
    params = params,
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_entries(response), flatten, flatten_mode)
}

#' List MaStR gas consumption data
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns public gas consumption data from the MaStR.
#' Official docs: https://github.com/bundesAPI/marktstammdaten-api.
#'
#' @examples
#' \dontrun{
#' marktstammdaten_gasverbrauch(params = list(page = 1, pageSize = 5))
#' }
#'
#' @return A tibble with MaStR entries.
#' @export
marktstammdaten_gasverbrauch <- function(params = list(),
                                         safe = TRUE,
                                         refresh = FALSE,
                                         flatten = FALSE,
                                         flatten_mode = "json") {
  response <- marktstammdaten_request(
    "/Einheit/EinheitJson/GetErweiterteOeffentlicheEinheitGasverbrauch",
    params = params,
    safe = safe,
    refresh = refresh
  )

  marktstammdaten_maybe_flatten(marktstammdaten_tidy_entries(response), flatten, flatten_mode)
}

marktstammdaten_request <- function(path,
                                     params = list(),
                                     safe = TRUE,
                                     refresh = FALSE,
                                     parse = "json") {
  bunddev_call(
    "marktstammdaten",
    path = path,
    method = "GET",
    params = params,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

marktstammdaten_tidy_filters <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(response, function(item) {
    cleaned <- purrr::map(item, ~ if (is.null(.x)) NA else .x)
    tibble::as_tibble(cleaned)
  })
}

marktstammdaten_tidy_entries <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  rows <- response$Data
  if (is.null(rows) || length(rows) == 0) {
    return(tibble::tibble())
  }

  total <- response$Total %||% NA_real_

  data <- purrr::map_dfr(rows, function(item) {
    cleaned <- purrr::map(item, ~ if (is.null(.x)) NA else .x)
    tibble::as_tibble(cleaned)
  })

  data$total <- total
  marktstammdaten_add_time_cols(data)
}

marktstammdaten_add_time_cols <- function(data) {
  date_cols <- names(data)[grepl("Datum", names(data))]
  if (length(date_cols) == 0) {
    return(data)
  }

  parse_mastr_date <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(as.POSIXct(NA_character_, tz = "Europe/Berlin"))
    }
    value <- as.character(value)
    stamp <- stringr::str_match(value, "\\/Date\\(([-0-9]+)\\)\\/")[, 2]
    if (is.na(stamp)) {
      return(as.POSIXct(NA_character_, tz = "Europe/Berlin"))
    }
    bunddev_ms_to_posix(as.numeric(stamp))
  }

  for (col in date_cols) {
    data[[paste0(col, "_time")]] <- purrr::map(data[[col]], parse_mastr_date)
    data[[paste0(col, "_time")]] <- as.POSIXct(
      purrr::map_dbl(data[[paste0(col, "_time")]], as.numeric),
      origin = "1970-01-01",
      tz = "Europe/Berlin"
    )
  }

  data
}

marktstammdaten_maybe_flatten <- function(data, flatten, flatten_mode) {
  if (!isTRUE(flatten)) {
    return(data)
  }

  list_cols <- names(data)[purrr::map_lgl(data, is.list)]
  bunddev_flatten_list_cols(data, cols = list_cols, mode = flatten_mode)
}

bunddev_marktstammdaten_filters_stromerzeugung <- function(safe = TRUE,
                                                           refresh = FALSE,
                                                           flatten = FALSE,
                                                           flatten_mode = "json") {
  marktstammdaten_filters_stromerzeugung(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_filters_stromverbrauch <- function(safe = TRUE,
                                                           refresh = FALSE,
                                                           flatten = FALSE,
                                                           flatten_mode = "json") {
  marktstammdaten_filters_stromverbrauch(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_filters_gaserzeugung <- function(safe = TRUE,
                                                         refresh = FALSE,
                                                         flatten = FALSE,
                                                         flatten_mode = "json") {
  marktstammdaten_filters_gaserzeugung(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_filters_gasverbrauch <- function(safe = TRUE,
                                                         refresh = FALSE,
                                                         flatten = FALSE,
                                                         flatten_mode = "json") {
  marktstammdaten_filters_gasverbrauch(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_stromerzeugung <- function(params = list(),
                                                   safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  marktstammdaten_stromerzeugung(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_stromverbrauch <- function(params = list(),
                                                   safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  marktstammdaten_stromverbrauch(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_gaserzeugung <- function(params = list(),
                                                 safe = TRUE,
                                                 refresh = FALSE,
                                                 flatten = FALSE,
                                                 flatten_mode = "json") {
  marktstammdaten_gaserzeugung(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_marktstammdaten_gasverbrauch <- function(params = list(),
                                                 safe = TRUE,
                                                 refresh = FALSE,
                                                 flatten = FALSE,
                                                 flatten_mode = "json") {
  marktstammdaten_gasverbrauch(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
