#' Search the Bundestag lobbyregister
#'
#' @param q Optional search string.
#' @param sort Optional sorting order.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns search results from the Bundestag lobbyregister.
#' Official docs: https://bundesapi.github.io/bundestag-lobbyregister-api/.
#'
#' @examples
#' \dontrun{
#' bundestag_lobbyregister_search(q = "energie")
#' }
#'
#' @return A tibble with search metadata and result entries.
#' @export
bundestag_lobbyregister_search <- function(q = NULL,
                                           sort = NULL,
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  params <- list()
  if (!is.null(q)) {
    params$q <- q
  }
  if (!is.null(sort)) {
    params$sort <- sort
  }

  response <- bunddev_call(
    "bundestag_lobbyregister",
    "sucheDetailJson",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- bundestag_lobbyregister_tidy(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("search_parameters", "results", "source"),
      mode = flatten_mode
    ))
  }

  data
}

bundestag_lobbyregister_tidy <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    source = response$source %||% NA_character_,
    source_url = response$sourceUrl %||% NA_character_,
    search_url = response$searchUrl %||% NA_character_,
    search_date = response$searchDate %||% NA_character_,
    search_parameters = list(response$searchParameters %||% list()),
    result_count = response$resultCount %||% NA_integer_,
    results = list(response$results %||% list())
  )
}

bunddev_bundestag_lobbyregister_search <- function(q = NULL,
                                                   sort = NULL,
                                                   safe = TRUE,
                                                   refresh = FALSE,
                                                   flatten = FALSE,
                                                   flatten_mode = "json") {
  bundestag_lobbyregister_search(
    q = q,
    sort = sort,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
