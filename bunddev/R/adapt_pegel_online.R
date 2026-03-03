#' List Pegel-Online stations
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Pegel-Online API provides water level station metadata. Use query
#' parameters to filter stations by water, ids, or location. Official docs:
#' https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_station()] for a single station and
#' [pegel_online_measurements()] for time series values.
#'
#' @examples
#' \dontrun{
#' pegel_online_stations(params = list(limit = 5))
#' }
#'
#' @return A tibble with station metadata.
#' @export
pegel_online_stations <- function(params = list(),
                                  safe = TRUE,
                                  refresh = FALSE,
                                  flatten = FALSE,
                                  flatten_mode = "json") {
  response <- bunddev_call(
    "pegel_online",
    "getStations",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- pegel_online_tidy_stations(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = c("water", "timeseries"), mode = flatten_mode))
  }

  data
}

#' Get a Pegel-Online station
#'
#' @param station Station UUID, name, or number.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Fetches a single station record. Official docs:
#' https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_stations()] and [pegel_online_timeseries()].
#'
#' @examples
#' \dontrun{
#' stations <- pegel_online_stations(params = list(limit = 1))
#' pegel_online_station(stations$uuid[[1]])
#' }
#'
#' @return A tibble with station metadata.
#' @export
pegel_online_station <- function(station,
                                 params = list(),
                                 safe = TRUE,
                                 refresh = FALSE,
                                 flatten = FALSE,
                                 flatten_mode = "json") {
  params$station <- station
  response <- bunddev_call(
    "pegel_online",
    "getStationsById",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- pegel_online_tidy_station(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = c("water", "timeseries"), mode = flatten_mode))
  }

  data
}

#' List Pegel-Online waters
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Lists waters available in Pegel-Online. Official docs:
#' https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_stations()] for station metadata.
#'
#' @examples
#' \dontrun{
#' pegel_online_waters(params = list(limit = 5))
#' }
#'
#' @return A tibble with water metadata.
#' @export
pegel_online_waters <- function(params = list(),
                                safe = TRUE,
                                refresh = FALSE,
                                flatten = FALSE,
                                flatten_mode = "json") {
  response <- bunddev_call(
    "pegel_online",
    "getWaters",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- pegel_online_tidy_waters(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = c("stations", "timeseries"), mode = flatten_mode))
  }

  data
}

#' Get Pegel-Online timeseries metadata
#'
#' @param station Station UUID, name, or number.
#' @param timeseries Timeseries shortname.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns metadata for a timeseries, including unit and gauge zero. Official
#' docs: https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_measurements()] for measurement values.
#'
#' @examples
#' \dontrun{
#' stations <- pegel_online_stations(params = list(limit = 1))
#' pegel_online_timeseries(stations$uuid[[1]], "W")
#' }
#'
#' @return A tibble with timeseries metadata.
#' @export
pegel_online_timeseries <- function(station,
                                    timeseries,
                                    params = list(),
                                    safe = TRUE,
                                    refresh = FALSE) {
  params$station <- station
  params$timeseries <- timeseries
  response <- bunddev_call(
    "pegel_online",
    "getCurrentMeasurmentByStation",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  pegel_online_tidy_timeseries(response)
}

#' Get Pegel-Online measurements
#'
#' @param station Station UUID, name, or number.
#' @param timeseries Timeseries shortname.
#' @param start Start timestamp in ISO 8601.
#' @param end End timestamp in ISO 8601.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement values for a station timeseries. Timestamps must be
#' ISO 8601 strings. Official docs: https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_timeseries()] for metadata and
#' [pegel_online_measurements_plot()] for PNG plots.
#'
#' @examples
#' \dontrun{
#' stations <- pegel_online_stations(params = list(limit = 1))
#' pegel_online_measurements(stations$uuid[[1]], "W",
#'   start = "2024-01-01T00:00:00Z",
#'   end = "2024-01-02T00:00:00Z"
#' )
#' }
#'
#' @return A tibble with measurements.
#'
#' Includes `timestamp_time` as POSIXct in Europe/Berlin.
#' @export
pegel_online_measurements <- function(station,
                                      timeseries,
                                      start = NULL,
                                      end = NULL,
                                      safe = TRUE,
                                      refresh = FALSE) {
  params <- list(station = station, timeseries = timeseries)
  if (!is.null(start)) {
    params$start <- start
  }
  if (!is.null(end)) {
    params$end <- end
  }

  response <- bunddev_call(
    "pegel_online",
    "getMeasurementByStation",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  pegel_online_tidy_measurements(response)
}

#' Get Pegel-Online measurements plot
#'
#' @param station Station UUID, name, or number.
#' @param timeseries Timeseries shortname.
#' @param start Start timestamp in ISO 8601.
#' @param end End timestamp in ISO 8601.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a PNG plot for the measurements endpoint. Official docs:
#' https://pegel-online.api.bund.dev.
#'
#' @seealso
#' [pegel_online_measurements()] for numeric values.
#'
#' @examples
#' \dontrun{
#' stations <- pegel_online_stations(params = list(limit = 1))
#' pegel_online_measurements_plot(stations$uuid[[1]], "W",
#'   start = "2024-01-01T00:00:00Z",
#'   end = "2024-01-02T00:00:00Z"
#' )
#' }
#'
#' @return A tibble with raw PNG bytes.
#' @export
pegel_online_measurements_plot <- function(station,
                                           timeseries,
                                           start = NULL,
                                           end = NULL,
                                           safe = TRUE,
                                           refresh = FALSE) {
  params <- list(station = station, timeseries = timeseries)
  if (!is.null(start)) {
    params$start <- start
  }
  if (!is.null(end)) {
    params$end <- end
  }

  response <- bunddev_call(
    "pegel_online",
    "getMeasurementDiagramByStation",
    params = params,
    parse = "raw",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(station = station, timeseries = timeseries, png = list(response))
}

pegel_online_tidy_stations <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  num_or_na <- function(value) {
    if (is.null(value)) NA_real_ else as.numeric(value)
  }

  tibble::tibble(
    uuid = purrr::map_chr(response, ~ chr_or_na(.x$uuid)),
    number = purrr::map_chr(response, ~ chr_or_na(.x$number)),
    shortname = purrr::map_chr(response, ~ chr_or_na(.x$shortname)),
    longname = purrr::map_chr(response, ~ chr_or_na(.x$longname)),
    km = purrr::map_dbl(response, ~ num_or_na(.x$km)),
    agency = purrr::map_chr(response, ~ chr_or_na(.x$agency)),
    longitude = purrr::map_dbl(response, ~ num_or_na(.x$longitude)),
    latitude = purrr::map_dbl(response, ~ num_or_na(.x$latitude)),
    water = purrr::map(response, ~ .x$water %||% list()),
    timeseries = purrr::map(response, ~ .x$timeseries %||% list())
  )
}

pegel_online_tidy_station <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }
  pegel_online_tidy_stations(list(response))
}

pegel_online_tidy_waters <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }
  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  tibble::tibble(
    shortname = purrr::map_chr(response, ~ chr_or_na(.x$shortname)),
    longname = purrr::map_chr(response, ~ chr_or_na(.x$longname)),
    stations = purrr::map(response, ~ .x$stations %||% list()),
    timeseries = purrr::map(response, ~ .x$timeseries %||% list())
  )
}

pegel_online_tidy_timeseries <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }
  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  num_or_na <- function(value) {
    if (is.null(value)) NA_real_ else as.numeric(value)
  }

  current <- response$currentMeasurement %||% list()
  current_value <- num_or_na(current$value)
  current_time <- current$timestamp %||% NA_character_

  tibble::tibble(
    shortname = chr_or_na(response$shortname),
    longname = chr_or_na(response$longname),
    unit = chr_or_na(response$unit),
    equidistance = num_or_na(response$equidistance),
    gauge_zero = list(response$gaugeZero %||% list()),
    characteristic_values = list(response$characteristicValues %||% list()),
    current_value = current_value,
    current_timestamp = chr_or_na(current_time),
    current_timestamp_time = as.POSIXct(current_time, tz = "Europe/Berlin")
  )
}

pegel_online_tidy_measurements <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  timestamps <- purrr::map_chr(response, ~ as.character(.x$timestamp %||% NA_character_))
  values <- purrr::map_dbl(response, ~ as.numeric(.x$value %||% NA_real_))

  tibble::tibble(
    timestamp = timestamps,
    value = values,
    timestamp_time = as.POSIXct(timestamps, tz = "Europe/Berlin")
  )
}

bunddev_pegel_online_stations <- function(params = list(),
                                          safe = TRUE,
                                          refresh = FALSE,
                                          flatten = FALSE,
                                          flatten_mode = "json") {
  pegel_online_stations(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_pegel_online_station <- function(station,
                                         params = list(),
                                         safe = TRUE,
                                         refresh = FALSE,
                                         flatten = FALSE,
                                         flatten_mode = "json") {
  pegel_online_station(
    station = station,
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_pegel_online_waters <- function(params = list(),
                                        safe = TRUE,
                                        refresh = FALSE,
                                        flatten = FALSE,
                                        flatten_mode = "json") {
  pegel_online_waters(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_pegel_online_timeseries <- function(station,
                                            timeseries,
                                            params = list(),
                                            safe = TRUE,
                                            refresh = FALSE) {
  pegel_online_timeseries(
    station = station,
    timeseries = timeseries,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_pegel_online_measurements <- function(station,
                                              timeseries,
                                              start = NULL,
                                              end = NULL,
                                              safe = TRUE,
                                              refresh = FALSE) {
  pegel_online_measurements(
    station = station,
    timeseries = timeseries,
    start = start,
    end = end,
    safe = safe,
    refresh = refresh
  )
}

bunddev_pegel_online_measurements_plot <- function(station,
                                                   timeseries,
                                                   start = NULL,
                                                   end = NULL,
                                                   safe = TRUE,
                                                   refresh = FALSE) {
  pegel_online_measurements_plot(
    station = station,
    timeseries = timeseries,
    start = start,
    end = end,
    safe = safe,
    refresh = refresh
  )
}
