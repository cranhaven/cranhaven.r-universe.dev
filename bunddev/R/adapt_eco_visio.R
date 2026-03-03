#' List Eco-Visio counters for an organization
#'
#' @param id_organisme Organization ID (e.g., 4586 for Bike Count Display).
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The Eco-Visio API provides access to bicycle and pedestrian counter data.
#' This function returns all counters registered for a specific organization.
#' Official docs: https://github.com/bundesAPI/eco-visio-api.
#'
#' @seealso
#' [eco_visio_data()] to retrieve measurement data for a counter.
#'
#' @examples
#' \dontrun{
#' eco_visio_counters(4586)
#' }
#'
#' @return A tibble with counter metadata.
#' @export
eco_visio_counters <- function(id_organisme,
                               safe = TRUE,
                               refresh = FALSE) {
  if (is.null(id_organisme) || id_organisme == "") {
    cli::cli_abort("id_organisme is required.")
  }

  response <- bunddev_call(
    "eco_visio",
    "zaehler",
    params = list(idOrganisme = id_organisme),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  eco_visio_tidy_counters(response)
}

#' Get Eco-Visio counter measurement data
#'
#' @param id_organisme Organization ID.
#' @param id_pdc Counter ID.
#' @param interval Aggregation interval (1-6: 1=15min, 2=hours, 3=days, 4=weeks,
#'   5=months, 6=years).
#' @param flow_ids Practice IDs (semicolon-separated string or character vector).
#' @param begin Optional start date (Date or "YYYY-MM-DD" string).
#' @param end Optional end date (Date or "YYYY-MM-DD" string).
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns measurement data for a specific counter. The `flow_ids` parameter
#' specifies which traffic types to include (e.g., pedestrians, cyclists).
#' Use [eco_visio_counters()] to discover available counters and their flow IDs.
#'
#' @seealso
#' [eco_visio_counters()] to list available counters.
#'
#' @examples
#' \dontrun{
#' # Get daily data for a counter
#' eco_visio_data(
#'   id_organisme = 4586,
#'   id_pdc = 100125331,
#'   interval = 4,
#'   flow_ids = "101125331"
#' )
#' }
#'
#' @return A tibble with measurement data.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
eco_visio_data <- function(id_organisme,
                           id_pdc,
                           interval,
                           flow_ids,
                           begin = NULL,
                           end = NULL,
                           safe = TRUE,
                           refresh = FALSE) {
  if (is.null(id_organisme) || id_organisme == "") {
    cli::cli_abort("id_organisme is required.")
  }
  if (is.null(id_pdc) || id_pdc == "") {
    cli::cli_abort("id_pdc is required.")
  }
  if (is.null(interval)) {
    cli::cli_abort("interval is required.")
  }

  if (is.character(flow_ids) && length(flow_ids) > 1) {
    flow_ids <- paste(flow_ids, collapse = ";")
  }
  if (is.null(flow_ids) || flow_ids == "") {
    cli::cli_abort("flow_ids is required.")
  }

  params <- list(
    idOrganisme = id_organisme,
    idPdc = id_pdc,
    interval = interval,
    flowIds = flow_ids
  )

  if (!is.null(begin)) {
    params$begin <- format(as.Date(begin), "%Y-%m-%d")
  }

  if (!is.null(end)) {
    params$end <- format(as.Date(end), "%Y-%m-%d")
  }

  response <- bunddev_call(
    "eco_visio",
    "zaehlerdaten",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  eco_visio_tidy_data(response)
}

eco_visio_tidy_counters <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  counters <- response
  if (!is.null(response$counter) || !is.null(response$counters)) {
    counters <- response$counter %||% response$counters %||% response
  }

  if (!is.list(counters) || length(counters) == 0) {
    return(tibble::tibble(response = list(response)))
  }

  if (is.data.frame(counters)) {
    return(tibble::as_tibble(counters))
  }

  to_chr <- function(value) if (is.null(value)) NA_character_ else as.character(value)
  to_num <- function(value) if (is.null(value)) NA_real_ else as.numeric(value)
  to_int <- function(value) if (is.null(value)) NA_integer_ else as.integer(value)

  if (all(purrr::map_lgl(counters, is.list))) {
    tibble::tibble(
      id = purrr::map_chr(counters, ~ to_chr(.x$id %||% .x$idPdc)),
      name = purrr::map_chr(counters, ~ to_chr(.x$name %||% .x$nom)),
      lat = purrr::map_dbl(counters, ~ to_num(.x$lat %||% .x$latitude)),
      lon = purrr::map_dbl(counters, ~ to_num(.x$lon %||% .x$lng %||% .x$longitude)),
      flow_ids = purrr::map(counters, ~ .x$flowIds %||% .x$pratpieton %||% list()),
      raw = counters
    )
  } else {
    tibble::tibble(response = list(counters))
  }
}

eco_visio_tidy_data <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  if (is.data.frame(response)) {
    data <- tibble::as_tibble(response)
    if ("date" %in% names(data)) {
      data$date_time <- as.POSIXct(data$date, tz = "Europe/Berlin")
    }
    return(data)
  }

  if (is.list(response) && all(c("date", "comptage") %in% names(response[[1]] %||% list()))) {
    tibble::tibble(
      date = purrr::map_chr(response, ~ as.character(.x$date %||% NA_character_)),
      count = purrr::map_int(response, ~ as.integer(.x$comptage %||% NA_integer_)),
      date_time = as.POSIXct(
        purrr::map_chr(response, ~ as.character(.x$date %||% NA_character_)),
        tz = "Europe/Berlin"
      )
    )
  } else {
    tibble::tibble(response = list(response))
  }
}

bunddev_eco_visio_counters <- function(id_organisme,
                                       safe = TRUE,
                                       refresh = FALSE) {
  eco_visio_counters(
    id_organisme = id_organisme,
    safe = safe,
    refresh = refresh
  )
}

bunddev_eco_visio_data <- function(id_organisme,
                                   id_pdc,
                                   interval,
                                   flow_ids,
                                   begin = NULL,
                                   end = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  eco_visio_data(
    id_organisme = id_organisme,
    id_pdc = id_pdc,
    interval = interval,
    flow_ids = flow_ids,
    begin = begin,
    end = end,
    safe = safe,
    refresh = refresh
  )
}
