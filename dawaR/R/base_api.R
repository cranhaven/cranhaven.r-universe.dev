#' Base API function
#'
#' @description This function is the foundation of all communication with the
#'   api in this package. Not intended for end-users, but it provides all the
#'   flexibility of the api without the nice conveniences of the other functions
#'   in this package. By default it reformats the api response (which is json)
#'   to a list object.
#'
#' @param section Defines what section of the api should be called.
#'   `available_sections()` exposes all options for this parameter.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> input query parameters.
#' @param append_to_url This options enables you to append a string to the url.
#'   It is inserted in the request after the `section` but before the `...`.
#' @param format The format to provide as a query parameter for the api call. By
#'   default it has value `NULL` and triggers default json formatting. When
#'   `geojson` or `geojsonz` is provided, the function will return the raw json
#'   data instead of the usual list.
#' @param verbose whether or not to provide `{cli}` output for the user. Enabled
#'   by default, but disabled for all functions that wrap this function
#' @param cache Option to use caching in a `tempdir()`. Enabled by default. More
#'   info can be found in the documentation for `httr2::req_cache()`.
#' @param dry_run With this option enabled, the function will output the request
#'   that has been created - Without sending it to the api. This is useful for
#'   debugging.
#' @param func_params Option to include extra parameters (just like `...`
#'   above). This option is used to pass down the dot-inputs from `get_data()`
#'   and `get_map_data()`. It is recommended to use the dynamic dots instead of
#'   this option, as an end user.
#'
#' @returns By default returns the API response as a list output. For
#'   `geojson(z)` formats it returns the raw json. When `dry_run = TRUE` it
#'   returns the request that has been created but does not run it.
#'
#' @export
#' @importFrom utils packageDescription
#'
#' @examples
#' if (connection_check()) {
#'   x <- dawa(section = "sogne")
#'
#'   x[[1]]
#' }
dawa <- function(
  section,
  ...,
  append_to_url = NULL,
  format = NULL,
  verbose = TRUE,
  cache = TRUE,
  dry_run = FALSE,
  func_params = list()
) {
  warn_api_deprecation()

  if (!is.null(format)) {
    format <- match.arg(
      format,
      c(
        "json",
        "jsonp",
        "ndjson",
        "csv",
        "geojson",
        "geojsonz"
      )
    )
  }
  if (!is.null(append_to_url)) {
    if (!typeof(append_to_url) == "character") {
      cli::cli_abort("{.var append_to_url} must be of type {.var character}")
    }
  }

  params <- list(
    ...,
    format = format
  )

  # Add landpostnumre parameter to specify only postal codes on land
  # see issue #100
  if (section == "postnumre") {
    params <- append(
      params,
      list(landpostnumre = "")
    )
  }

  base_url <- "https://api.dataforsyningen.dk"

  if (!connection_check()) {
    cli::cli_alert_warning(
      "You do not have access to api.dataforsyningen.dk.
        Please check your connection settings."
    )
    return(NULL) # Exit early if no connection is detected
  }

  section_info(section, verbose)

  base_request <- httr2::request(base_url) |>
    httr2::req_url_path_append(section) |>
    httr2::req_url_path_append(append_to_url) |>
    httr2::req_url_query(!!!params) |> # user provided query params
    httr2::req_url_query(!!!func_params) |> # list of inputs from funcs
    httr2::req_user_agent(
      paste0(
        "dawaR_",
        utils::packageDescription("dawaR", fields = "Version"),
        " at https://dawar.aleksanderbl.dk)"
      )
    ) |>
    httr2::req_timeout(100) |> # Timeout limit of 10 seconds
    httr2::req_retry(max_tries = 3) # Retry on transient erros 503 and 429

  if (cache == TRUE) {
    temp_dir <- tempdir() # Location for caching the response
    dawa_request <- base_request |>
      httr2::req_cache(temp_dir)
  } else if (cache == FALSE) {
    dawa_request <- base_request |>
      httr2::req_url_query(cache = "no-cache")
  }

  if (verbose == TRUE) {
    cli::cli_alert_info("Accessing {.url {dawa_request[1]}}")
  }

  resp <- NULL

  if (dry_run == TRUE) {
    return(httr2::req_dry_run(dawa_request))
  } else if (dry_run == FALSE) {
    tryCatch(
      {
        resp <- httr2::req_perform(dawa_request)
      },
      error = function(e) {
        cli::cli_alert_danger("Request failed: {e$message}")
        resp <- resp
      }
    )
  }

  if (is.null(resp)) {
    cli::cli_alert_danger("The API returned a {resp$status_code} error.")
    cli::cli_alert_danger("No content will be returned")
    return(resp)
  }

  if (!is.null(format) && !is.null(resp) && !httr2::resp_is_error(resp)) {
    tryCatch(
      {
        if (format %in% c("geojson", "geojsonz")) {
          return(resp |> httr2::resp_body_string())
        }
      },
      error = function(e) {
        cli::cli_alert_danger("Failed to parse response: {e$message}")
      }
    )
    # nolint start
  } else if (
    !is.null(resp) &&
      !httr2::resp_is_error(resp) &&
      httr2::resp_content_type(resp) != "application/json"
  ) {
    # nolint end
    cli::cli_abort("The API did not return JSON")
  } else if (!is.null(resp) && !httr2::resp_is_error(resp)) {
    tryCatch(
      {
        return(resp |> httr2::resp_body_json())
      },
      error = function(e) {
        cli::cli_alert_danger("Failed to parse response: {e$message}")
      }
    )
  }
}


#' Base function for interacting with `reverse geocoding` of sections.
#'
#' @description This function reverse geocodes the coordinates provided. When
#' used out of the box, the function wants WGS84 coordinates (lat/long). ETRS89
#' or UTM zone 32 can be provided as well. When coordinates from outside of
#' Denmark is provided, the function will fail with a code `404`.
#'
#'
#' @param x The X coordinate to geocode. If no `srid` is provided this should be
#'   the lattitude.
#' @param y The Y coordinate to geocode. If no `srid` is provided this should be
#'   the longitude.
#' @param type The coordinate type for the API to parse. By default none is
#'   provided and `WGS84` is used. The options are `ETRS89`, `UTM32` and
#'   `WGS84`. The functions checks the input against the available types and
#'   errs if the type is not allowed.
#'
#' @inheritParams dawa
#'
#' @return Returns a list object with information about the provided `section`
#'    for the location that is input.
#' @export
#'
#' @examples
#' if (connection_check()) {
#'   reverse("regioner", x = 12.58515, y = 55.68324)
#' }
reverse <- function(section, x, y, verbose = TRUE, type = NULL, ...) {
  warn_api_deprecation()

  if (!is.null(type)) {
    if (check_coordinate_type(type)) {
      coord <- coordinate_type(type)
    }
  } else {
    coord <- type
  }

  if (!connection_check()) {
    cli::cli_alert_warning(
      "You do not have access to api.dataforsyningen.dk.
        Please check your connection settings."
    )
    return(NULL) # Exit early if no connection is detected
  }

  section_info(section, verbose, type = "reverse")

  response <- dawa(
    section = section,
    append_to_url = "reverse",
    verbose = FALSE,
    x = x,
    y = y,
    srid = coord,
    ...
  )

  if (is.null(response)) {
    if (verbose) {
      cli::cli_alert_warning("The API did not return any data.")
    }
    return(NULL)
  }

  return(response)
}

#' Base function for interacting with `autocomplete` of sections.
#'
#' @description This function is very handy when needing to either validate or
#' simply "fill in gaps" with autocompletion from the API.
#'
#'
#' @param input The input to autocomplete. Could be a name, a place and similar.
#'
#' @inheritParams dawa
#'
#' @return Returns the autocomplete information for the provided `section`
#'    given the input.
#' @export
#'
#' @examples
#' if (connection_check()) {
#'   autocomplete("regioner", "midt")
#' }
autocomplete <- function(section, input, ...) {
  warn_api_deprecation()

  if (!connection_check()) {
    cli::cli_alert_warning(
      "You do not have access to api.dataforsyningen.dk.
        Please check your connection settings."
    )
    return(NULL) # Exit early if no connection is detected
  }

  response <- dawa(
    section = section,
    append_to_url = "autocomplete",
    verbose = FALSE,
    q = input,
    ... = ...
  )

  if (is.null(response)) {
    return(NULL)
  }

  return(response)
}
