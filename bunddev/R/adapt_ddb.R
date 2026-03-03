#' Search Deutsche Digitale Bibliothek
#'
#' @param query Search query string.
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns search results from the Deutsche Digitale Bibliothek API. You need an
#' API key from https://www.deutsche-digitale-bibliothek.de/user/apikey. The key
#' is sent in the `Authorization` header as `OAuth oauth_consumer_key="<key>"`.
#'
#' Configure authentication via [bunddev_auth_set()] using a template-style
#' scheme, or set the `DDB_API_KEY` environment variable directly.
#'
#' @seealso
#' [bunddev_auth_set()] to configure authentication.
#'
#' @examples
#' \dontrun{
#' # Recommended: use bunddev_auth_set with template scheme
#' Sys.setenv(DDB_API_KEY = "<api-key>")
#' bunddev_auth_set("ddb", type = "api_key", env_var = "DDB_API_KEY",
#'                  scheme = "OAuth oauth_consumer_key=\"%s\"")
#' ddb_search(query = "berlin", params = list(rows = 5))
#' }
#'
#' @return A tibble with search metadata and result payload.
#' @export
ddb_search <- function(query,
                       params = list(),
                       safe = TRUE,
                       refresh = FALSE,
                       flatten = FALSE,
                       flatten_mode = "json") {
  if (is.null(query) || query == "") {
    cli::cli_abort("query is required for ddb_search().")
  }
  params$query <- query

  response <- ddb_request(
    "/search",
    params = params,
    safe = safe,
    refresh = refresh
  )

  data <- ddb_tidy_response(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = "response", mode = flatten_mode))
  }

  data
}

#' List DDB institutions
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns institutions registered in the DDB.
#' Requires the DDB API key.
#'
#' @examples
#' \dontrun{
#' ddb_institutions(params = list(hasItems = TRUE))
#' }
#'
#' @return A tibble with institution entries.
#' @export
ddb_institutions <- function(params = list(),
                             safe = TRUE,
                             refresh = FALSE,
                             flatten = FALSE,
                             flatten_mode = "json") {
  response <- ddb_request(
    "/institutions",
    params = params,
    safe = safe,
    refresh = refresh
  )

  data <- ddb_tidy_list(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = names(data)[purrr::map_lgl(data, is.list)], mode = flatten_mode))
  }

  data
}

#' List DDB institution sectors
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns institution sector metadata.
#' Requires the DDB API key.
#'
#' @examples
#' \dontrun{
#' ddb_institution_sectors()
#' }
#'
#' @return A tibble with sector entries.
#' @export
ddb_institution_sectors <- function(safe = TRUE, refresh = FALSE) {
  response <- ddb_request(
    "/institutions/sectors",
    safe = safe,
    refresh = refresh
  )

  ddb_tidy_list(response)
}

ddb_request <- function(path,
                        params = list(),
                        safe = TRUE,
                        refresh = FALSE,
                        parse = "json") {
  # bunddev_call() handles auth via bunddev_auth_get/header automatically
  # Configure auth via: bunddev_auth_set("ddb", type = "api_key", env_var = "DDB_API_KEY",
  #                                      scheme = "OAuth oauth_consumer_key=\"%s\"")
  # Or set DDB_API_KEY env var and it will use legacy fallback below

  auth <- bunddev_auth_get("ddb")
  if (auth$type == "none") {
    # Legacy fallback for direct env var usage
    api_key <- ddb_api_key()
    bunddev_call(
      "ddb",
      path = path,
      method = "GET",
      params = params,
      headers = list(Authorization = paste0("OAuth oauth_consumer_key=\"", api_key, "\"")),
      parse = parse,
      safe = safe,
      refresh = refresh
    )
  } else {
    bunddev_call(
      "ddb",
      path = path,
      method = "GET",
      params = params,
      parse = parse,
      safe = safe,
      refresh = refresh
    )
  }
}

ddb_api_key <- function() {
  auth <- bunddev_auth_get("ddb")
  if (auth$type == "api_key") {
    api_key <- Sys.getenv(auth$env_var)
    if (api_key == "") {
      cli::cli_abort("Environment variable '{auth$env_var}' is not set.")
    }
    return(api_key)
  }

  api_key <- Sys.getenv("DDB_API_KEY")
  if (api_key == "") {
    cli::cli_abort("DDB_API_KEY is not set.")
  }
  api_key
}

ddb_tidy_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(response = list(response))
}

ddb_tidy_list <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  if (is.list(response) && length(response) > 0 && all(purrr::map_lgl(response, is.list))) {
    return(purrr::map_dfr(response, function(item) {
      cleaned <- purrr::map(item, ~ if (is.null(.x)) NA else .x)
      tibble::as_tibble(cleaned)
    }))
  }

  tibble::tibble(response = list(response))
}

bunddev_ddb_search <- function(query,
                               params = list(),
                               safe = TRUE,
                               refresh = FALSE,
                               flatten = FALSE,
                               flatten_mode = "json") {
  ddb_search(
    query = query,
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_ddb_institutions <- function(params = list(),
                                     safe = TRUE,
                                     refresh = FALSE,
                                     flatten = FALSE,
                                     flatten_mode = "json") {
  ddb_institutions(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_ddb_institution_sectors <- function(safe = TRUE, refresh = FALSE) {
  ddb_institution_sectors(safe = safe, refresh = refresh)
}
