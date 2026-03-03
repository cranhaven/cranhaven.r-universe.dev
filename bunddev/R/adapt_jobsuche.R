#' Search Jobsuche listings
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Jobsuche API provides access to job listings from the Bundesagentur fuer
#' Arbeit. Authentication is required via an API key passed as `X-API-Key`.
#' See https://jobsuche.api.bund.dev for the official API documentation.
#'
#' Use [bunddev_auth_set()] to configure the key and [bunddev_parameters()] to
#' discover supported query parameters.
#'
#' @seealso
#' [jobsuche_search_app()] for the app endpoint, [jobsuche_logo()] to fetch
#' employer logos, and [bunddev_auth_set()] for authentication.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
#' bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY")
#' jobsuche_search(params = list(was = "data", size = 5), flatten = TRUE)
#' }
#'
#' @return A tibble with job listings.
#'
#' Includes parsed POSIXct columns (suffix `_time`) in Europe/Berlin.
#' @export
jobsuche_search <- function(params = list(),
                            safe = TRUE,
                            refresh = FALSE,
                            flatten = FALSE,
                            flatten_mode = "json") {
  response <- jobsuche_request(
    "search",
    path = "/pc/v4/jobs",
    query = params,
    safe = safe,
    refresh = refresh
  )
  jobsuche_tidy_response(response, flatten = flatten, flatten_mode = flatten_mode)
}

#' Search Jobsuche listings (app endpoint)
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' This uses the mobile app endpoint of the Jobsuche API. It shares the same
#' authentication mechanism and parameters as [jobsuche_search()].
#'
#' See https://jobsuche.api.bund.dev for API documentation.
#'
#' @seealso
#' [jobsuche_search()] for the standard endpoint and [bunddev_parameters()] for
#' parameter discovery.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
#' bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY")
#' jobsuche_search_app(params = list(was = "data", size = 5), flatten = TRUE)
#' }
#'
#' @return A tibble with job listings.
#'
#' Includes parsed POSIXct columns (suffix `_time`) in Europe/Berlin.
#' @export
jobsuche_search_app <- function(params = list(),
                                safe = TRUE,
                                refresh = FALSE,
                                flatten = FALSE,
                                flatten_mode = "json") {
  response <- jobsuche_request(
    "search_app",
    path = "/pc/v4/app/jobs",
    query = params,
    safe = safe,
    refresh = refresh
  )
  jobsuche_tidy_response(response, flatten = flatten, flatten_mode = flatten_mode)
}

#' Fetch Jobsuche employer logo
#'
#' @param hash_id Logo hash id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns the raw logo bytes for a given employer hash id. Use this together
#' with listings returned by [jobsuche_search()] or [jobsuche_search_app()].
#'
#' See https://jobsuche.api.bund.dev for API documentation.
#'
#' @seealso
#' [jobsuche_search()] for listings and [bunddev_auth_set()] for auth setup.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
#' bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY")
#' logo <- jobsuche_logo("abc123")
#' }
#'
#' @return A tibble with raw logo bytes.
#' @export
jobsuche_logo <- function(hash_id, safe = TRUE, refresh = FALSE) {
  response <- jobsuche_request(
    "logo",
    path = paste0("/ed/v1/arbeitgeberlogo/", hash_id),
    safe = safe,
    refresh = refresh,
    parse = "raw"
  )
  tibble::tibble(hash_id = hash_id, logo = list(response))
}

bunddev_tidy_jobsuche <- function(response, operation_id = NULL,
                                  flatten = FALSE, flatten_mode = "json") {
  jobsuche_tidy_response(response, flatten = flatten, flatten_mode = flatten_mode)
}

jobsuche_tidy_response <- function(response, flatten = FALSE, flatten_mode = "json") {
  listings <- response$stellenangebote
  if (is.null(listings) || length(listings) == 0) {
    return(tibble::tibble())
  }

  parse_time <- function(value, tz = "Europe/Berlin") {
    if (is.null(value)) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
    }
    if (inherits(value, "POSIXct")) {
      return(value)
    }
    if (inherits(value, "Date")) {
      return(as.POSIXct(value, tz = tz))
    }
    if (is.numeric(value)) {
      return(bunddev_ms_to_posix(value, tz = tz))
    }
    if (is.character(value) && grepl("^\\d{12,}$", value)) {
      return(bunddev_ms_to_posix(value, tz = tz))
    }
    suppressWarnings(as.POSIXct(value, tz = tz))
  }

  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  num_or_na <- function(value) {
    if (is.null(value)) NA_real_ else as.numeric(value)
  }

  data <- tibble::tibble(
    hash_id = purrr::map_chr(listings, ~ chr_or_na(.x$hashId)),
    beruf = purrr::map_chr(listings, ~ chr_or_na(.x$beruf)),
    refnr = purrr::map_chr(listings, ~ chr_or_na(.x$refnr)),
    arbeitgeber = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitgeber)),
    aktuelle_veroeffentlichungsdatum = purrr::map_chr(
      listings,
      ~ chr_or_na(.x$aktuelleVeroeffentlichungsdatum)
    ),
    aktuelle_veroeffentlichungsdatum_time = purrr::map(
      listings,
      ~ parse_time(.x$aktuelleVeroeffentlichungsdatum)
    ),
    eintrittsdatum = purrr::map_chr(listings, ~ chr_or_na(.x$eintrittsdatum)),
    eintrittsdatum_time = purrr::map(listings, ~ parse_time(.x$eintrittsdatum)),
    arbeitsort_plz = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitsort$plz)),
    arbeitsort_ort = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitsort$ort)),
    arbeitsort_strasse = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitsort$strasse)),
    arbeitsort_region = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitsort$region)),
    arbeitsort_land = purrr::map_chr(listings, ~ chr_or_na(.x$arbeitsort$land)),
    arbeitsort_lat = purrr::map_dbl(listings, ~ num_or_na(.x$arbeitsort$koordinaten$lat)),
    arbeitsort_lon = purrr::map_dbl(listings, ~ num_or_na(.x$arbeitsort$koordinaten$lon)),
    modifikations_timestamp = purrr::map_chr(listings, ~ chr_or_na(.x$modifikationsTimestamp)),
    modifikations_timestamp_time = purrr::map(listings, ~ parse_time(.x$modifikationsTimestamp)),
    page = chr_or_na(response$page),
    size = chr_or_na(response$size),
    max_ergebnisse = chr_or_na(response$maxErgebnisse),
    facetten = list(response$facetten %||% list())
  )

  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = c("facetten"), mode = flatten_mode))
  }

  data
}

jobsuche_request <- function(operation_id, path, query = NULL,
                              safe = TRUE, refresh = FALSE, parse = "json") {
  bunddev_call(
    "jobsuche",
    path = path,
    method = "GET",
    params = query,
    base_url = "https://rest.arbeitsagentur.de/jobboerse/jobsuche-service",
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

bunddev_jobsuche_search <- function(params = list(),
                                    safe = TRUE,
                                    refresh = FALSE,
                                    flatten = FALSE,
                                    flatten_mode = "json") {
  jobsuche_search(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_jobsuche_search_app <- function(params = list(),
                                        safe = TRUE,
                                        refresh = FALSE,
                                        flatten = FALSE,
                                        flatten_mode = "json") {
  jobsuche_search_app(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_jobsuche_logo <- function(hash_id, safe = TRUE, refresh = FALSE) {
  jobsuche_logo(hash_id = hash_id, safe = safe, refresh = refresh)
}
