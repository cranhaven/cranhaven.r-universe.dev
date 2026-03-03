#' List food and product warnings
#'
#' @param food Optional request options for food warnings (rows, start, sort, fq).
#' @param products Optional request options for product warnings (rows, start, sort, fq).
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Lebensmittelwarnungen API returns German food and product safety warnings.
#' Requests are sent as JSON to the `warnings/merged` endpoint. Official docs:
#' https://lebensmittelwarnung.api.bund.dev and
#' https://github.com/bundesAPI/lebensmittelwarnung-api.
#'
#' The API expects an `Authorization` header. By default the adapter uses the
#' public key documented in the OpenAPI spec. Override it with
#' [bunddev_auth_set()] if needed.
#'
#' @seealso
#' [bunddev_auth_set()] to configure the authorization header.
#'
#' @examples
#' \dontrun{
#' lebensmittelwarnung_warnings(food = list(rows = 10), products = list(rows = 0))
#' }
#'
#' @return A tibble with warning entries.
#'
#' Includes `published_date_time` as POSIXct in Europe/Berlin.
#' @export
lebensmittelwarnung_warnings <- function(food = list(),
                                         products = list(),
                                         safe = TRUE,
                                         refresh = FALSE,
                                         flatten = FALSE,
                                         flatten_mode = "json") {
  body <- list()
  if (!is.null(food)) {
    body$food <- food
  }
  if (!is.null(products)) {
    body$products <- products
  }

  response <- lebensmittelwarnung_request(body, safe = safe, refresh = refresh)
  data <- lebensmittelwarnung_tidy_response(response)

  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("affected_states", "product", "rapex_information", "safety_information"),
      mode = flatten_mode
    ))
  }

  data
}

lebensmittelwarnung_request <- function(body, safe = TRUE, refresh = FALSE) {
  # Get auth token or use default public key
  auth <- bunddev_auth_get("lebensmittelwarnung")
  default_key <- "baystmuv-vi-1.0 os=ios, key=9d9e8972-ff15-4943-8fea-117b5a973c61"
  auth_value <- default_key
  if (auth$type == "api_key") {
    if (is.na(auth$env_var) || auth$env_var == "") {
      cli::cli_abort("API key env_var is not set for 'lebensmittelwarnung'.")
    }
    api_key <- Sys.getenv(auth$env_var)
    if (api_key == "") {
      cli::cli_abort("Environment variable '{auth$env_var}' is not set.")
    }
    auth_value <- api_key
  }

  bunddev_call(
    "lebensmittelwarnung",
    "list-warnungen",
    body = body,
    body_type = "json",
    headers = list(Authorization = auth_value),
    parse = "json",
    safe = safe,
    refresh = refresh
  )
}

lebensmittelwarnung_tidy_response <- function(response) {
  payload <- response$response %||% response
  docs <- payload$docs
  if (is.null(docs) || length(docs) == 0) {
    return(tibble::tibble())
  }

  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  int_or_na <- function(value) {
    if (is.null(value)) NA_integer_ else as.integer(value)
  }
  num_or_na <- function(value) {
    if (is.null(value)) NA_real_ else as.numeric(value)
  }
  bool_or_na <- function(value) {
    if (is.null(value)) NA else as.logical(value)
  }

  num_found <- int_or_na(payload$numFound)
  published_ms <- purrr::map_dbl(docs, ~ num_or_na(.x$publishedDate))

  tibble::tibble(
    type = purrr::map_chr(docs, ~ chr_or_na(.x$`_type`)),
    archived = purrr::map_lgl(docs, ~ bool_or_na(.x$archived)),
    id = purrr::map_int(docs, ~ int_or_na(.x$id)),
    link = purrr::map_chr(docs, ~ chr_or_na(.x$link)),
    title = purrr::map_chr(docs, ~ chr_or_na(.x$title)),
    warning = purrr::map_chr(docs, ~ chr_or_na(.x$warning)),
    affected_states = purrr::map(docs, ~ .x$affectedStates %||% character()),
    product = purrr::map(docs, ~ .x$product %||% list()),
    rapex_information = purrr::map(docs, ~ .x$rapexInformation %||% list()),
    safety_information = purrr::map(docs, ~ .x$safetyInformation %||% list()),
    published_date = published_ms,
    published_date_time = purrr::map(published_ms, ~ bunddev_ms_to_posix(.x)),
    num_found = num_found
  )
}

bunddev_lebensmittelwarnung_warnings <- function(food = list(),
                                                 products = list(),
                                                 safe = TRUE,
                                                 refresh = FALSE,
                                                 flatten = FALSE,
                                                 flatten_mode = "json") {
  lebensmittelwarnung_warnings(
    food = food,
    products = products,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
