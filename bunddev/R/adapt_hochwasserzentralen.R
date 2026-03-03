#' Get flood gauge information
#'
#' @param pegelnummer Pegelnummer identifier (e.g., "HE_24820206").
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns metadata for a single flood gauge (pegel) from
#' hochwasserzentralen.de. Official docs:
#' https://bundesapi.github.io/hochwasserzentralen-api/.
#'
#' @examples
#' \dontrun{
#' hochwasserzentralen_pegel_info("HE_24820206")
#' }
#'
#' @return A tibble with pegel metadata.
#' @export
hochwasserzentralen_pegel_info <- function(pegelnummer, safe = TRUE, refresh = FALSE) {
  params <- list(pgnr = pegelnummer)
  response <- hochwasserzentralen_request(
    "/webservices/get_infospegel.php",
    params = params,
    method = "POST",
    safe = safe,
    refresh = refresh
  )

  hochwasserzentralen_tidy_table(response)
}

#' List flood portal states and connected regions
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns metadata for all Bundeslaender and connected regions in the
#' hochwasserzentralen.de portal. Official docs:
#' https://bundesapi.github.io/hochwasserzentralen-api/.
#'
#' @examples
#' \dontrun{
#' hochwasserzentralen_bundeslaender()
#' }
#'
#' @return A tibble with Bundesland metadata.
#' @export
hochwasserzentralen_bundeslaender <- function(safe = TRUE, refresh = FALSE) {
  response <- hochwasserzentralen_request(
    "/webservices/get_infosbundesland.php",
    method = "GET",
    safe = safe,
    refresh = refresh
  )

  hochwasserzentralen_tidy_table(response)
}

#' Get flood portal metadata for a Bundesland
#'
#' @param bundesland_id Bundesland id (e.g., "HE").
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns metadata for a single Bundesland or region in the
#' hochwasserzentralen.de portal. Official docs:
#' https://bundesapi.github.io/hochwasserzentralen-api/.
#'
#' @examples
#' \dontrun{
#' hochwasserzentralen_bundesland_info("HE")
#' }
#'
#' @return A tibble with Bundesland metadata.
#' @export
hochwasserzentralen_bundesland_info <- function(bundesland_id, safe = TRUE, refresh = FALSE) {
  params <- list(id = bundesland_id)
  response <- hochwasserzentralen_request(
    "/webservices/get_infosbundesland.php",
    params = params,
    method = "POST",
    safe = safe,
    refresh = refresh
  )

  hochwasserzentralen_tidy_table(response)
}

#' List flood gauge locations
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns latitude/longitude coordinates for all pegel identifiers available
#' in the hochwasserzentralen.de portal. Official docs:
#' https://bundesapi.github.io/hochwasserzentralen-api/.
#'
#' @examples
#' \dontrun{
#' hochwasserzentralen_lagepegel()
#' }
#'
#' @return A tibble with pegel coordinates.
#' @export
hochwasserzentralen_lagepegel <- function(safe = TRUE, refresh = FALSE) {
  response <- hochwasserzentralen_request(
    "/webservices/get_lagepegel.php",
    method = "GET",
    safe = safe,
    refresh = refresh
  )

  hochwasserzentralen_tidy_table(response)
}

#' Get Bundesland GeoJSON boundaries
#'
#' @param version GeoJSON version identifier (e.g., "20211130").
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns GeoJSON boundaries for Bundeslaender and connected regions from the
#' hochwasserzentralen.de portal. Official docs:
#' https://bundesapi.github.io/hochwasserzentralen-api/.
#'
#' @examples
#' \dontrun{
#' hochwasserzentralen_bundesland_geojson("20211130", flatten = TRUE)
#' }
#'
#' @return A tibble with GeoJSON metadata and feature list-columns.
#' @export
hochwasserzentralen_bundesland_geojson <- function(version,
                                                   safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  path <- paste0("/vhosts/geojson/bundesland.", version, ".geojson")
  response <- hochwasserzentralen_request(
    path,
    method = "GET",
    safe = safe,
    refresh = refresh
  )

  data <- hochwasserzentralen_tidy_geojson(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = "features", mode = flatten_mode))
  }

  data
}

hochwasserzentralen_request <- function(path,
                                        params = list(),
                                        method = "GET",
                                        safe = TRUE,
                                        refresh = FALSE,
                                        parse = "json",
                                        operation_id = NULL) {
  method_lower <- tolower(method)

  # For POST requests, send params as form body; for GET, use query params
  if (method_lower == "post" && length(params) > 0) {
    bunddev_call(
      "hochwasserzentralen",
      path = path,
      method = method,
      body = params,
      body_type = "form",
      parse = parse,
      safe = safe,
      refresh = refresh
    )
  } else {
    bunddev_call(
      "hochwasserzentralen",
      path = path,
      method = method,
      params = params,
      parse = parse,
      safe = safe,
      refresh = refresh
    )
  }
}

hochwasserzentralen_tidy_table <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  # If response has GeoJSON features, extract properties from each feature
  if (is.list(response) && !is.null(response$features) && length(response$features) > 0) {
    rows <- purrr::map(response$features, function(feature) {
      if (!is.null(feature$properties)) {
        return(feature$properties)
      }
      return(list())
    })
    return(dplyr::bind_rows(rows))
  }

  if (!is.list(response)) {
    return(tibble::tibble(value = response))
  }

  columns <- purrr::imap(response, function(value, name) {
    if (is.null(value)) {
      return(NA_character_)
    }
    if (is.list(value) && !is.data.frame(value)) {
      value <- unlist(value, recursive = FALSE, use.names = FALSE)
    }
    value
  })

  tibble::tibble(!!!columns)
}

hochwasserzentralen_tidy_geojson <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    type = response$type %||% NA_character_,
    name = response$name %||% NA_character_,
    features = list(response$features %||% response$fetures %||% list())
  )
}

bunddev_hochwasserzentralen_pegel_info <- function(pegelnummer, safe = TRUE, refresh = FALSE) {
  hochwasserzentralen_pegel_info(pegelnummer = pegelnummer, safe = safe, refresh = refresh)
}

bunddev_hochwasserzentralen_bundeslaender <- function(safe = TRUE, refresh = FALSE) {
  hochwasserzentralen_bundeslaender(safe = safe, refresh = refresh)
}

bunddev_hochwasserzentralen_bundesland_info <- function(bundesland_id, safe = TRUE, refresh = FALSE) {
  hochwasserzentralen_bundesland_info(bundesland_id = bundesland_id, safe = safe, refresh = refresh)
}

bunddev_hochwasserzentralen_lagepegel <- function(safe = TRUE, refresh = FALSE) {
  hochwasserzentralen_lagepegel(safe = safe, refresh = refresh)
}

bunddev_hochwasserzentralen_bundesland_geojson <- function(version,
                                                           safe = TRUE,
                                                           refresh = FALSE,
                                                           flatten = FALSE,
                                                           flatten_mode = "json") {
  hochwasserzentralen_bundesland_geojson(
    version = version,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
