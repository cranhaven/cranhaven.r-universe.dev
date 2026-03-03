#' Query Bundeshaushalt budget data
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Bundeshaushalt API provides budget data for federal income and expenses.
#' Required query parameters are `year` and `account`. Official docs:
#' https://github.com/bundesAPI/bundeshaushalt-api.
#'
#' @examples
#' \dontrun{
#' bundeshaushalt_budget_data(params = list(year = 2021, account = "expenses"))
#' }
#'
#' @return A tibble with budget data and nested detail lists.
#'
#' Includes `timestamp_time` as POSIXct in Europe/Berlin.
#' @export
bundeshaushalt_budget_data <- function(params = list(),
                                       safe = TRUE,
                                       refresh = FALSE,
                                       flatten = FALSE,
                                       flatten_mode = "json") {
  required <- c("year", "account")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required parameter(s): {paste(missing, collapse = ", ")}")
  }

  response <- bunddev_call(
    "bundeshaushalt",
    "budgetData",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- bundeshaushalt_tidy_budget_data(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("details", "children", "parents", "related"),
      mode = flatten_mode
    ))
  }

  data
}

bundeshaushalt_tidy_budget_data <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  meta <- response$meta %||% list()
  timestamp <- meta$timestamp %||% NA_real_
  timestamp <- as.numeric(timestamp)

  tibble::tibble(
    account = meta$account %||% NA_character_,
    entity = meta$entity %||% NA_character_,
    level_cur = meta$levelCur %||% NA_integer_,
    level_max = meta$levelMax %||% NA_integer_,
    modify_date = meta$modifyDate %||% NA_character_,
    quota = meta$quota %||% NA_character_,
    timestamp = timestamp,
    timestamp_time = bunddev_ms_to_posix(timestamp),
    unit = meta$unit %||% NA_character_,
    year = meta$year %||% NA_integer_,
    details = list(response$details %||% list()),
    children = list(response$children %||% list()),
    parents = list(response$parents %||% list()),
    related = list(response$related %||% list())
  )
}

bunddev_bundeshaushalt_budget_data <- function(params = list(),
                                               safe = TRUE,
                                               refresh = FALSE,
                                               flatten = FALSE,
                                               flatten_mode = "json") {
  bundeshaushalt_budget_data(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
