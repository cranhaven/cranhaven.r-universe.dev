#' List Dashboard Deutschland entries
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns the full list of dashboard entries and metadata for each item.
#' Official docs: https://bundesapi.github.io/dashboard-deutschland-api/.
#'
#' @examples
#' \dontrun{
#' dashboard_deutschland_get()
#' }
#'
#' @return A tibble with dashboard entries.
#' @export
dashboard_deutschland_get <- function(safe = TRUE,
                                      refresh = FALSE,
                                      flatten = FALSE,
                                      flatten_mode = "json") {
  response <- bunddev_call(
    "dashboard_deutschland",
    "get",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- dashboard_deutschland_tidy_get(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("category", "tags", "layout_tiles"),
      mode = flatten_mode
    ))
  }

  data
}

#' Query Dashboard Deutschland indicators
#'
#' @param ids Indicator ids, semicolon-separated or as a character vector.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns indicator data for the specified ids. Official docs:
#' https://bundesapi.github.io/dashboard-deutschland-api/.
#'
#' @examples
#' \dontrun{
#' dashboard_deutschland_indicators("tile_1667811574092")
#' }
#'
#' @return A tibble with indicator data.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
dashboard_deutschland_indicators <- function(ids = NULL,
                                             safe = TRUE,
                                             refresh = FALSE) {
  params <- list()
  if (!is.null(ids)) {
    if (length(ids) > 1) {
      ids <- paste(ids, collapse = ";")
    }
    params$ids <- ids
  }

  response <- bunddev_call(
    "dashboard_deutschland",
    "indicators",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  dashboard_deutschland_tidy_indicators(response)
}

#' Get Dashboard Deutschland GeoJSON
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns GeoJSON data for Germany and the federal states.
#' Official docs: https://bundesapi.github.io/dashboard-deutschland-api/.
#'
#' @examples
#' \dontrun{
#' dashboard_deutschland_geo(flatten = TRUE)
#' }
#'
#' @return A tibble with GeoJSON metadata.
#' @export
dashboard_deutschland_geo <- function(safe = TRUE,
                                      refresh = FALSE,
                                      flatten = FALSE,
                                      flatten_mode = "json") {
  response <- bunddev_call(
    "dashboard_deutschland",
    "geo",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- dashboard_deutschland_tidy_geo(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("crs", "features"),
      mode = flatten_mode
    ))
  }

  data
}

dashboard_deutschland_tidy_get <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  to_chr <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  to_lgl <- function(value) {
    if (is.null(value)) NA else isTRUE(value)
  }
  to_int <- function(value) {
    if (is.null(value)) NA_integer_ else as.integer(value)
  }

  tibble::tibble(
    id = purrr::map_chr(response, ~ to_chr(.x$id)),
    name = purrr::map_chr(response, ~ to_chr(.x$name)),
    name_en = purrr::map_chr(response, ~ to_chr(.x$nameEn)),
    description = purrr::map_chr(response, ~ to_chr(.x$description)),
    description_en = purrr::map_chr(response, ~ to_chr(.x$descriptionEn)),
    teaser = purrr::map_chr(response, ~ to_chr(.x$teaser)),
    teaser_en = purrr::map_chr(response, ~ to_chr(.x$teaserEn)),
    conclusion = purrr::map_chr(response, ~ to_chr(.x$conclusion)),
    conclusion_en = purrr::map_chr(response, ~ to_chr(.x$conclusionEn)),
    category = purrr::map(response, ~ .x$category %||% list()),
    tags = purrr::map(response, ~ .x$tags %||% list()),
    image = purrr::map_chr(response, ~ to_chr(.x$image)),
    clicks = purrr::map_int(response, ~ to_int(.x$clicks)),
    order_id = purrr::map_int(response, ~ to_int(.x$orderId)),
    trending = purrr::map_lgl(response, ~ to_lgl(.x$trending)),
    top = purrr::map_lgl(response, ~ to_lgl(.x$top)),
    layout_tiles = purrr::map(response, ~ .x$layoutTiles %||% list()),
    layout_mode = purrr::map_chr(response, ~ to_chr(.x$layoutMode))
  )
}

dashboard_deutschland_tidy_indicators <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  to_chr <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  to_num <- function(value) {
    if (is.null(value)) NA_real_ else as.numeric(value)
  }

  dates <- purrr::map_dbl(response, ~ to_num(.x$date))

  tibble::tibble(
    id = purrr::map_chr(response, ~ to_chr(.x$id)),
    date = dates,
    date_time = bunddev_ms_to_posix(dates),
    json = purrr::map_chr(response, ~ to_chr(.x$json)),
    title = purrr::map_chr(response, ~ to_chr(.x$title))
  )
}

dashboard_deutschland_tidy_geo <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    type = response$type %||% NA_character_,
    name = response$name %||% NA_character_,
    title = response$title %||% NA_character_,
    version = response$version %||% NA_character_,
    copyright_short = response$copyrightShort %||% NA_character_,
    copyright_url = response$copyrightUrl %||% NA_character_,
    crs = list(response$crs %||% list()),
    features = list(response$features %||% list())
  )
}

bunddev_dashboard_deutschland_get <- function(safe = TRUE,
                                              refresh = FALSE,
                                              flatten = FALSE,
                                              flatten_mode = "json") {
  dashboard_deutschland_get(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dashboard_deutschland_indicators <- function(ids = NULL,
                                                     safe = TRUE,
                                                     refresh = FALSE) {
  dashboard_deutschland_indicators(ids = ids, safe = safe, refresh = refresh)
}

bunddev_dashboard_deutschland_geo <- function(safe = TRUE,
                                              refresh = FALSE,
                                              flatten = FALSE,
                                              flatten_mode = "json") {
  dashboard_deutschland_geo(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
