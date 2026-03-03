#' List available SMARD timestamps
#'
#' @param filter Filter id.
#' @param region Region code.
#' @param resolution Data resolution.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The SMARD API provides power market data published by the Bundesnetzagentur.
#' Use this helper to retrieve available timestamps for a given filter/region
#' and resolution. Official docs: https://smard.api.bund.dev.
#'
#' Note: The registry rate limit states that more than 60 requests per hour are
#' not permitted. Use `safe = TRUE` to respect the limit.
#'
#' @seealso
#' [smard_timeseries()] and [smard_table()] for data retrieval, and
#' [bunddev_parameters()] for parameter discovery.
#'
#' @examples
#' \dontrun{
#' smard_indices(410, region = "DE", resolution = "hour")
#' }
#'
#' @return A tibble of timestamps.
#' @export
smard_indices <- function(filter, region = "DE", resolution = "hour", safe = TRUE, refresh = FALSE) {
  path <- sprintf("/chart_data/%s/%s/index_%s.json", filter, region, resolution)
  response <- smard_request("indices", path, params = list(
    filter = filter,
    region = region,
    resolution = resolution
  ), safe = safe, refresh = refresh)
  bunddev_tidy_smard(response, operation_id = "indices")
}

#' Fetch SMARD timeseries data
#'
#' @param filter Filter id.
#' @param region Region code.
#' @param resolution Data resolution.
#' @param timestamp Timestamp from indices (ms), POSIXct, or Date.
#'
#' Timestamps are interpreted in the Europe/Berlin timezone.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a tidy time series for a single filter/region. Use
#' [smard_indices()] to obtain a valid timestamp. Official docs:
#' https://smard.api.bund.dev.
#'
#' @seealso
#' [smard_indices()] for timestamps and [smard_table()] for table output.
#'
#' @examples
#' \dontrun{
#' indices <- smard_indices(410, region = "DE", resolution = "hour")
#' smard_timeseries(410, region = "DE", resolution = "hour", timestamp = indices$timestamp[[1]])
#' }
#'
#' @return A tibble of time series values.
#'
#' Includes a `time` column with POSIXct timestamps in Europe/Berlin.
#' @export
smard_timeseries <- function(filter, region = "DE", resolution = "hour", timestamp,
                             safe = TRUE, refresh = FALSE) {
  timestamp_ms <- bunddev_timestamp_to_ms(timestamp)
  path <- sprintf(
    "/chart_data/%s/%s/%s_%s_%s_%s.json",
    filter,
    region,
    filter,
    region,
    resolution,
    timestamp_ms
  )
  response <- smard_request("timeseries", path, params = list(
    filter = filter,
    filterCopy = filter,
    region = region,
    regionCopy = region,
    resolution = resolution,
    timestamp = timestamp_ms
  ), safe = safe, refresh = refresh)
  bunddev_tidy_smard(response, operation_id = "timeseries")
}

#' Fetch SMARD table data
#'
#' @param filter Filter id.
#' @param region Region code.
#' @param timestamp Timestamp from indices (ms), POSIXct, or Date.
#'
#' Timestamps are interpreted in the Europe/Berlin timezone.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns table-style SMARD data for a single timestamp. Use
#' [smard_indices()] to obtain a valid timestamp. Official docs:
#' https://smard.api.bund.dev.
#'
#' @seealso
#' [smard_indices()] for timestamps and [smard_timeseries()] for time series.
#'
#' @examples
#' \dontrun{
#' indices <- smard_indices(410, region = "DE", resolution = "hour")
#' smard_table(410, region = "DE", timestamp = indices$timestamp[[1]])
#' }
#'
#' @return A tibble of table values.
#'
#' Includes a `time` column with POSIXct timestamps in Europe/Berlin.
#' @export
smard_table <- function(filter, region = "DE", timestamp, safe = TRUE, refresh = FALSE) {
  timestamp_ms <- bunddev_timestamp_to_ms(timestamp)
  path <- sprintf(
    "/table_data/%s/%s/%s_%s_quarterhour_%s.json",
    filter,
    region,
    filter,
    region,
    timestamp_ms
  )
  response <- smard_request("table", path, params = list(
    filter = filter,
    filterCopy = filter,
    region = region,
    regionCopy = region,
    timestamp = timestamp_ms
  ), safe = safe, refresh = refresh)
  bunddev_tidy_smard(response, operation_id = "table")
}

bunddev_tidy_smard <- function(response, operation_id = NULL) {
  if (operation_id == "indices") {
    if (is.null(response$timestamps)) {
      return(tibble::tibble())
    }
    return(tibble::tibble(timestamp = purrr::map_dbl(response$timestamps, as.numeric)))
  }

  if (operation_id == "timeseries") {
    series <- response$series
    if (is.null(series) || length(series) == 0) {
      return(tibble::tibble())
    }
    pull_value <- function(x, idx) {
      if (length(x) < idx || is.null(x[[idx]])) {
        return(NA_real_)
      }
      as.numeric(x[[idx]])
    }
    timestamps <- purrr::map_dbl(series, ~ pull_value(.x, 1))
    return(tibble::tibble(
      timestamp = timestamps,
      time = bunddev_ms_to_posix(timestamps),
      value = purrr::map_dbl(series, ~ pull_value(.x, 2))
    ))
  }

  if (operation_id == "table") {
    series <- response$series
    if (is.null(series) || length(series) == 0) {
      return(tibble::tibble())
    }
    values <- purrr::map_dfr(series, function(item) {
      if (is.null(item$values) || length(item$values) == 0) {
        return(tibble::tibble())
      }
      purrr::map_dfr(item$values, function(value) {
        timestamp <- as.numeric(value$timestamp)
        tibble::tibble(
          timestamp = timestamp,
          time = bunddev_ms_to_posix(timestamp),
          value = purrr::map_dbl(value$versions, ~ as.numeric(.x$value)),
          name = purrr::map_chr(value$versions, ~ if (is.null(.x$name)) NA_character_ else .x$name)
        )
      })
    })
    return(values)
  }

  tibble::tibble()
}

bunddev_smard_indices <- function(filter, region = "DE", resolution = "hour") {
  smard_indices(filter = filter, region = region, resolution = resolution)
}

bunddev_smard_timeseries <- function(filter, region = "DE", resolution = "hour", timestamp) {
  smard_timeseries(filter = filter, region = region, resolution = resolution, timestamp = timestamp)
}

bunddev_smard_table <- function(filter, region = "DE", timestamp) {
  smard_table(filter = filter, region = region, timestamp = timestamp)
}

smard_request <- function(operation_id, path, params, safe = TRUE, refresh = FALSE) {
  bunddev_call(
    "smard",
    path = path,
    method = "GET",
    params = params,
    base_url = "https://www.smard.de/app",
    safe = safe,
    refresh = refresh
  )
}

