#' List air quality measurements
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The Luftqualitaet API provides air quality data and metadata from the
#' Umweltbundesamt. Use query parameters to filter by date/time and station.
#' Official docs: https://luftqualitaet.api.bund.dev.
#'
#' @seealso
#' [luftqualitaet_measures()] for measurement metadata and
#' [luftqualitaet_components()] for pollutant components.
#'
#' @examples
#' \dontrun{
#' luftqualitaet_airquality(params = list(
#'   date_from = "2024-01-01",
#'   date_to = "2024-01-02"
#' ))
#' }
#'
#' @return A tibble with air quality data.
#' @export
luftqualitaet_airquality <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/airquality/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' Get air quality date limits
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns date limits for air quality measurements. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with date limits.
#'
#' @export
luftqualitaet_airquality_limits <- function(safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/airquality/limits", list(), safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List annual balances
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns annual balance data for a component and year. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with annual balance data.
#' @export
luftqualitaet_annualbalances <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/annualbalances/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List components
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns available pollutant components. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with component metadata.
#' @export
luftqualitaet_components <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/components/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List measurements metadata
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement metadata for stations, components, and scopes. Official
#' docs: https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with measurement metadata.
#' @export
luftqualitaet_measures <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/measures/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' Get measurement date limits
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns date limits for measurement metadata. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with date limits.
#'
#' @export
luftqualitaet_measures_limits <- function(safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/measures/limits", list(), safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List networks
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns networks used by the Luftqualitaet API. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with network metadata.
#' @export
luftqualitaet_networks <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/networks/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List scopes
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns scopes used by the Luftqualitaet API. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with scope metadata.
#' @export
luftqualitaet_scopes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/scopes/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List station settings
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns station settings metadata. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with station settings metadata.
#' @export
luftqualitaet_stationsettings <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/stationsettings/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List station types
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns station types metadata. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with station types metadata.
#' @export
luftqualitaet_stationtypes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/stationtypes/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List thresholds
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns threshold metadata for components and scopes. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with threshold metadata.
#' @export
luftqualitaet_thresholds <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/thresholds/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List transgressions
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns exceedances (transgressions) data. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with transgressions data.
#' @export
luftqualitaet_transgressions <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/transgressions/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List transgression types
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns available transgression types. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with transgression type metadata.
#' @export
luftqualitaet_transgressiontypes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/transgressiontypes/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

#' List combined metadata
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns combined metadata for a given use case. Official docs:
#' https://luftqualitaet.api.bund.dev.
#'
#' @return A tibble with combined metadata.
#' @export
luftqualitaet_meta <- function(params = list(), safe = TRUE, refresh = FALSE) {
  response <- luftqualitaet_request("/meta/json", params, safe, refresh)
  luftqualitaet_tidy_table(response)
}

luftqualitaet_request <- function(path, query, safe = TRUE, refresh = FALSE) {
  bunddev_call(
    "luftqualitaet",
    path = path,
    method = "GET",
    params = query %||% list(),
    parse = "json",
    safe = safe,
    refresh = refresh
  )
}

luftqualitaet_tidy_table <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  indices <- response$indices %||% list()
  data <- response$data
  if (is.null(data)) {
    data <- response[setdiff(names(response), c("count", "indices", "request"))]
  }

  if (is.null(data) || length(data) == 0) {
    return(tibble::tibble())
  }

  key_name <- NULL
  col_names <- NULL
  if (length(indices) > 0) {
    if (length(indices) == 1 && is.list(indices[[1]]) && all(purrr::map_lgl(indices[[1]], is.character))) {
      key_name <- names(indices)[[1]]
      col_names <- c(key_name, unname(unlist(indices[[1]])))
    } else {
      col_names <- unname(unlist(indices))
    }
  }

  rows <- data
  keys <- names(rows)

  row_list <- purrr::map(rows, function(values) {
    if (is.null(values)) {
      values <- list()
    }
    as.list(values)
  })

  if (!is.null(key_name) && !is.null(keys)) {
    row_list <- purrr::map2(row_list, keys, function(values, key) {
      c(setNames(list(key), key_name), values)
    })
  }

  if (!is.null(col_names)) {
    row_list <- purrr::map(row_list, function(values) {
      length(values) <- length(col_names)
      names(values) <- col_names
      values
    })
  }

  dplyr::bind_rows(row_list)
}

bunddev_luftqualitaet_airquality <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_airquality(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_airquality_limits <- function(safe = TRUE, refresh = FALSE) {
  luftqualitaet_airquality_limits(safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_annualbalances <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_annualbalances(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_components <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_components(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_measures <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_measures(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_measures_limits <- function(safe = TRUE, refresh = FALSE) {
  luftqualitaet_measures_limits(safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_networks <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_networks(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_scopes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_scopes(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_stationsettings <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_stationsettings(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_stationtypes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_stationtypes(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_thresholds <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_thresholds(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_transgressions <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_transgressions(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_transgressiontypes <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_transgressiontypes(params = params, safe = safe, refresh = refresh)
}

bunddev_luftqualitaet_meta <- function(params = list(), safe = TRUE, refresh = FALSE) {
  luftqualitaet_meta(params = params, safe = safe, refresh = refresh)
}
