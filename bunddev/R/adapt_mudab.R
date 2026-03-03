#' List MUDAB project stations
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns project stations from the MUDAB database.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_project_stations(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with project stations.
#' @export
mudab_project_stations <- function(filter = NULL,
                                   range = NULL,
                                   orderby = NULL,
                                   safe = TRUE,
                                   refresh = FALSE,
                                   flatten = FALSE,
                                   flatten_mode = "json") {
  response <- mudab_request(
    "/PROJECTSTATION_SMALL",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_maybe_flatten(mudab_tidy_key(response, "V_MUDAB_PROJECTSTATION"), flatten, flatten_mode)
}

#' List MUDAB stations
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement stations from the MUDAB database.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_stations(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with stations.
#' @export
mudab_stations <- function(filter = NULL,
                           range = NULL,
                           orderby = NULL,
                           safe = TRUE,
                           refresh = FALSE) {
  response <- mudab_request(
    "/STATION_SMALL",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "STATION_SMALL")
}

#' List MUDAB parameters
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement parameters from the MUDAB database.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameters(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameters.
#' @export
mudab_parameters <- function(filter = NULL,
                             range = NULL,
                             orderby = NULL,
                             safe = TRUE,
                             refresh = FALSE) {
  response <- mudab_request(
    "/MV_PARAMETER",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_PARAMETER")
}

#' List MUDAB parameter values
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement values for parameters from the MUDAB database.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameter_values(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameter values.
#'
#' Includes `datetime_time` as POSIXct in Europe/Berlin when date/time fields are
#' present.
#' @export
mudab_parameter_values <- function(filter = NULL,
                                   range = NULL,
                                   orderby = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  response <- mudab_request(
    "/MV_STATION_MSMNT",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_STATION_MSMNT")
}

#' List MUDAB parameters (Biologie)
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns parameter entries for the Biologie compartment.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameters_biologie(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameters.
#' @export
mudab_parameters_biologie <- function(filter = NULL,
                                      range = NULL,
                                      orderby = NULL,
                                      safe = TRUE,
                                      refresh = FALSE) {
  response <- mudab_request(
    "/MV_PARAMETER_BIOLOGIE",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_PARAMETER_BIOLOGIE")
}

#' List MUDAB parameters (Biota)
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns parameter entries for the Biota compartment.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameters_biota(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameters.
#' @export
mudab_parameters_biota <- function(filter = NULL,
                                   range = NULL,
                                   orderby = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  response <- mudab_request(
    "/MV_PARAMETER_BIOTA",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_PARAMETER_BIOTA")
}

#' List MUDAB parameters (Wasser)
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns parameter entries for the Wasser compartment.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameters_wasser(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameters.
#' @export
mudab_parameters_wasser <- function(filter = NULL,
                                    range = NULL,
                                    orderby = NULL,
                                    safe = TRUE,
                                    refresh = FALSE) {
  response <- mudab_request(
    "/MV_PARAMETER_WASSER",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_PARAMETER_WASSER")
}

#' List MUDAB parameters (Sediment)
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns parameter entries for the Sediment compartment.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_parameters_sediment(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with parameters.
#' @export
mudab_parameters_sediment <- function(filter = NULL,
                                      range = NULL,
                                      orderby = NULL,
                                      safe = TRUE,
                                      refresh = FALSE) {
  response <- mudab_request(
    "/MV_PARAMETER_SEDIMENT",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "MV_PARAMETER_SEDIMENT")
}

#' List MUDAB PLC stations
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns HELCOM PLC stations.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_plc_stations(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with PLC stations.
#' @export
mudab_plc_stations <- function(filter = NULL,
                               range = NULL,
                               orderby = NULL,
                               safe = TRUE,
                               refresh = FALSE) {
  response <- mudab_request(
    "/V_PLC_STATION",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "V_PLC_STATION")
}

#' List MUDAB PLC parameters
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns parameters measured at PLC stations.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_plc_parameters(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with PLC parameters.
#' @export
mudab_plc_parameters <- function(filter = NULL,
                                 range = NULL,
                                 orderby = NULL,
                                 safe = TRUE,
                                 refresh = FALSE) {
  response <- mudab_request(
    "/V_GEMESSENE_PARA_PLC",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "V_GEMESSENE_PARA_PLC")
}

#' List MUDAB PLC measurement values
#'
#' @param filter Optional filter definition.
#' @param range Optional range specification.
#' @param orderby Optional ordering specification.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns PLC station measurement values.
#' Official docs: https://mudab.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' mudab_plc_measurements(range = list(from = 0, count = 5))
#' }
#'
#' @return A tibble with PLC measurements.
#' @export
mudab_plc_measurements <- function(filter = NULL,
                                   range = NULL,
                                   orderby = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  response <- mudab_request(
    "/V_MESSWERTE_PLC",
    body = mudab_build_body(filter, range, orderby),
    safe = safe,
    refresh = refresh
  )

  mudab_tidy_key(response, "V_MESSWERTE_PLC")
}

mudab_build_body <- function(filter, range, orderby) {
  body <- list()
  if (!is.null(filter)) {
    body$filter <- filter
  }
  if (!is.null(range)) {
    body$range <- range
  }
  if (!is.null(orderby)) {
    body$orderby <- orderby
  }
  body
}

mudab_request <- function(path, body, safe = TRUE, refresh = FALSE, parse = "json") {
  bunddev_call(
    "mudab",
    path = path,
    method = "POST",
    body = body,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

mudab_tidy_key <- function(response, key) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  rows <- response[[key]]
  if (is.null(rows) || length(rows) == 0) {
    return(tibble::tibble())
  }

  data <- purrr::map_dfr(rows, function(item) {
    cleaned <- purrr::map(item, ~ if (is.null(.x)) NA else .x)
    tibble::as_tibble(cleaned)
  })

  mudab_add_time_cols(data)
}

mudab_add_time_cols <- function(data) {
  if (!all(c("DATE_STM", "TIME_STM") %in% names(data))) {
    return(data)
  }

  datetime <- paste0(data$DATE_STM, stringr::str_pad(data$TIME_STM, 4, pad = "0"))
  data$datetime_time <- as.POSIXct(datetime, format = "%Y%m%d%H%M", tz = "Europe/Berlin")
  data
}

mudab_maybe_flatten <- function(data, flatten, flatten_mode) {
  if (!isTRUE(flatten)) {
    return(data)
  }

  list_cols <- names(data)[purrr::map_lgl(data, is.list)]
  bunddev_flatten_list_cols(data, cols = list_cols, mode = flatten_mode)
}

bunddev_mudab_project_stations <- function(filter = NULL,
                                           range = NULL,
                                           orderby = NULL,
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  mudab_project_stations(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_mudab_stations <- function(filter = NULL,
                                   range = NULL,
                                   orderby = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  mudab_stations(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameters <- function(filter = NULL,
                                     range = NULL,
                                     orderby = NULL,
                                     safe = TRUE,
                                     refresh = FALSE) {
  mudab_parameters(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameter_values <- function(filter = NULL,
                                           range = NULL,
                                           orderby = NULL,
                                           safe = TRUE,
                                           refresh = FALSE) {
  mudab_parameter_values(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameters_biologie <- function(filter = NULL,
                                              range = NULL,
                                              orderby = NULL,
                                              safe = TRUE,
                                              refresh = FALSE) {
  mudab_parameters_biologie(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameters_biota <- function(filter = NULL,
                                           range = NULL,
                                           orderby = NULL,
                                           safe = TRUE,
                                           refresh = FALSE) {
  mudab_parameters_biota(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameters_wasser <- function(filter = NULL,
                                            range = NULL,
                                            orderby = NULL,
                                            safe = TRUE,
                                            refresh = FALSE) {
  mudab_parameters_wasser(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_parameters_sediment <- function(filter = NULL,
                                              range = NULL,
                                              orderby = NULL,
                                              safe = TRUE,
                                              refresh = FALSE) {
  mudab_parameters_sediment(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_plc_stations <- function(filter = NULL,
                                       range = NULL,
                                       orderby = NULL,
                                       safe = TRUE,
                                       refresh = FALSE) {
  mudab_plc_stations(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_plc_parameters <- function(filter = NULL,
                                         range = NULL,
                                         orderby = NULL,
                                         safe = TRUE,
                                         refresh = FALSE) {
  mudab_plc_parameters(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}

bunddev_mudab_plc_measurements <- function(filter = NULL,
                                           range = NULL,
                                           orderby = NULL,
                                           safe = TRUE,
                                           refresh = FALSE) {
  mudab_plc_measurements(
    filter = filter,
    range = range,
    orderby = orderby,
    safe = safe,
    refresh = refresh
  )
}
