#' List Autobahn road ids
#'
#' @return A tibble with available road ids.
#'
#' @details
#' Lists Autobahn road ids from the Autobahn App API (Autobahn GmbH).
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roadworks()] and [autobahn_warnings()] for road-specific data.
#'
#' @examples
#' \dontrun{
#' autobahn_roads()
#' }
#' @export
autobahn_roads <- function() {
  bunddev_call_tidy("autobahn", "list-autobahnen")
}

#' List Autobahn roadworks
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with roadworks.
#'
#' @details
#' Returns current roadworks for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] to list available roads, and [autobahn_roadwork_details()]
#' for detail records.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_roadworks(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_roadworks <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-roadworks",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' List Autobahn warnings
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with warnings.
#'
#' @details
#' Returns current warnings for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] to list roads and [autobahn_warning_details()] for details.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_warnings(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_warnings <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-warnings",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' List Autobahn webcams
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with webcams.
#'
#' @details
#' Returns webcam entries for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] and [autobahn_webcam_details()].
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_webcams(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_webcams <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-webcams",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' List Autobahn closures
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with closures.
#'
#' @details
#' Returns current closures for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] and [autobahn_closure_details()].
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_closures(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_closures <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-closures",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' List Autobahn charging stations
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with charging stations.
#'
#' @details
#' Returns charging stations for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] and [autobahn_charging_station_details()].
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_charging_stations(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_charging_stations <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-charging-stations",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' List Autobahn lorry parking areas
#'
#' @param road_id Road identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with lorry parking areas.
#'
#' @details
#' Returns lorry parking areas for a specific Autobahn road id.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roads()] and [autobahn_parking_lorry_details()].
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' autobahn_parking_lorries(roads$road_id[[1]], flatten = TRUE)
#' }
#' @export
autobahn_parking_lorries <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "list-parking-lorries",
    params = list(roadId = road_id),
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn roadwork details
#'
#' @param roadwork_id Roadwork identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with roadwork details.
#'
#' @details
#' Returns full details for a single roadwork entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_roadworks()] to list roadworks.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' roadworks <- autobahn_roadworks(roads$road_id[[1]])
#' autobahn_roadwork_details(roadworks$identifier[[1]])
#' }
#' @export
autobahn_roadwork_details <- function(roadwork_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-roadwork",
    params = list(roadworkId = roadwork_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn warning details
#'
#' @param warning_id Warning identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with warning details.
#'
#' @details
#' Returns full details for a single warning entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_warnings()] to list warnings.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' warnings <- autobahn_warnings(roads$road_id[[1]])
#' autobahn_warning_details(warnings$identifier[[1]])
#' }
#' @export
autobahn_warning_details <- function(warning_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-warning",
    params = list(warningId = warning_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn webcam details
#'
#' @param webcam_id Webcam identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with webcam details.
#'
#' @details
#' Returns full details for a single webcam entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_webcams()] to list webcams.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' webcams <- autobahn_webcams(roads$road_id[[1]])
#' autobahn_webcam_details(webcams$identifier[[1]])
#' }
#' @export
autobahn_webcam_details <- function(webcam_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-webcam",
    params = list(webcamId = webcam_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn closure details
#'
#' @param closure_id Closure identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with closure details.
#'
#' @details
#' Returns full details for a single closure entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_closures()] to list closures.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' closures <- autobahn_closures(roads$road_id[[1]])
#' autobahn_closure_details(closures$identifier[[1]])
#' }
#' @export
autobahn_closure_details <- function(closure_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-closure",
    params = list(closureId = closure_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn charging station details
#'
#' @param station_id Charging station identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with charging station details.
#'
#' @details
#' Returns full details for a single charging station entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_charging_stations()] to list stations.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' stations <- autobahn_charging_stations(roads$road_id[[1]])
#' autobahn_charging_station_details(stations$identifier[[1]])
#' }
#' @export
autobahn_charging_station_details <- function(station_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-charging-station",
    params = list(stationId = station_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Get Autobahn lorry parking details
#'
#' @param lorry_id Lorry parking identifier.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with lorry parking details.
#'
#' @details
#' Returns full details for a single lorry parking entry.
#' Official docs: https://autobahn.api.bund.dev.
#'
#' @seealso
#' [autobahn_parking_lorries()] to list parking areas.
#'
#' @examples
#' \dontrun{
#' roads <- autobahn_roads()
#' parking <- autobahn_parking_lorries(roads$road_id[[1]])
#' autobahn_parking_lorry_details(parking$identifier[[1]])
#' }
#' @export
autobahn_parking_lorry_details <- function(lorry_id, flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "autobahn",
    "get-parking-lorry",
    params = list(lorryId = lorry_id),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_tidy_autobahn <- function(response, operation_id = NULL, flatten = FALSE,
                                  road_id = NA_character_, flatten_mode = "json") {
  if (operation_id == "list-autobahnen") {
    roads <- response$roads
    if (is.null(roads)) {
      return(tibble::tibble())
    }
    return(tibble::tibble(road_id = as.character(roads)))
  }

  items <- NULL
  if (operation_id == "list-roadworks") {
    items <- response$roadworks
  } else if (operation_id == "list-warnings") {
    items <- response$warning
  } else if (operation_id == "list-webcams") {
    items <- response$webcam
  } else if (operation_id == "list-closures") {
    items <- response$closure
  } else if (operation_id == "list-charging-stations") {
    items <- response$electric_charging_station
  } else if (operation_id == "list-parking-lorries") {
    items <- response$parking_lorry
  } else if (operation_id %in% c("get-roadwork", "get-warning")) {
    items <- list(response)
  } else if (operation_id %in% c(
    "get-webcam",
    "get-closure",
    "get-charging-station",
    "get-parking-lorry"
  )) {
    items <- list(response)
  }

  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble())
  }

  bunddev_tidy_autobahn_items(
    items,
    road_id = road_id,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_autobahn_roads <- function() {
  autobahn_roads()
}

bunddev_autobahn_roadworks <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_roadworks(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_warnings <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_warnings(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_webcams <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_webcams(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_closures <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_closures(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_charging_stations <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_charging_stations(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_parking_lorries <- function(road_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_parking_lorries(road_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_roadwork_details <- function(roadwork_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_roadwork_details(roadwork_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_warning_details <- function(warning_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_warning_details(warning_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_webcam_details <- function(webcam_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_webcam_details(webcam_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_closure_details <- function(closure_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_closure_details(closure_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_charging_station_details <- function(station_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_charging_station_details(station_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_autobahn_parking_lorry_details <- function(lorry_id, flatten = FALSE, flatten_mode = "json") {
  autobahn_parking_lorry_details(lorry_id, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_tidy_autobahn_items <- function(items, road_id = NA_character_,
                                        flatten = FALSE, flatten_mode = "json") {
  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  lgl_or_na <- function(value) {
    if (is.null(value)) NA else tolower(as.character(value)) == "true"
  }
  collapse_or_na <- function(value) {
    if (is.null(value)) NA_character_ else paste(value, collapse = " ")
  }
  list_or_empty <- function(value) {
    if (is.null(value)) list() else value
  }

  data <- tibble::tibble(
    road_id = road_id,
    identifier = purrr::map_chr(items, ~ chr_or_na(.x$identifier)),
    title = purrr::map_chr(items, ~ chr_or_na(.x$title)),
    subtitle = purrr::map_chr(items, ~ chr_or_na(.x$subtitle)),
    display_type = purrr::map_chr(items, ~ chr_or_na(.x$display_type)),
    icon = purrr::map_chr(items, ~ chr_or_na(.x$icon)),
    is_blocked = purrr::map_lgl(items, ~ lgl_or_na(.x$isBlocked)),
    future = purrr::map_lgl(items, ~ if (is.null(.x$future)) NA else isTRUE(.x$future)),
    start_timestamp = purrr::map_chr(items, ~ chr_or_na(.x$startTimestamp)),
    start_time = purrr::map(items, ~ bunddev_ms_to_posix(.x$startTimestamp)),
    point = purrr::map_chr(items, ~ chr_or_na(.x$point)),
    extent = purrr::map_chr(items, ~ chr_or_na(.x$extent)),
    coordinate_lat = purrr::map_chr(items, ~ chr_or_na(.x$coordinate$lat)),
    coordinate_long = purrr::map_chr(items, ~ chr_or_na(.x$coordinate$long)),
    description = purrr::map_chr(items, ~ collapse_or_na(.x$description)),
    footer = purrr::map_chr(items, ~ collapse_or_na(.x$footer)),
    route_recommendation = purrr::map(items, ~ list_or_empty(.x$routeRecommendation)),
    lorry_parking_feature_icons = purrr::map(items, ~ list_or_empty(.x$lorryParkingFeatureIcons))
  )

  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("route_recommendation", "lorry_parking_feature_icons"),
      mode = flatten_mode
    ))
  }

  data
}
