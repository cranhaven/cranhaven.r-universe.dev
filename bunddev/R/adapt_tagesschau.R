#' Fetch Tagesschau homepage items
#'
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Fetches the Tagesschau homepage feed as provided by the ARD Tagesschau API.
#' Official docs: https://bundesapi.github.io/tagesschau-api/.
#'
#' Note: The registry rate limit allows up to 60 requests per hour. Usage of
#' content is restricted to private, non-commercial use unless otherwise stated
#' by the source (see Tagesschau CC license notes).
#'
#' @seealso
#' [tagesschau_news()], [tagesschau_search()], and [tagesschau_channels()].
#'
#' @examples
#' \dontrun{
#' tagesschau_homepage(flatten = TRUE)
#' }
#'
#' @return A tibble with homepage items.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
tagesschau_homepage <- function(flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "tagesschau",
    "homepage",
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Fetch Tagesschau news items
#'
#' @param regions Optional region ids.
#' @param ressort Optional ressort filter.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns current news items with optional filters for region or ressort.
#' Official docs: https://bundesapi.github.io/tagesschau-api/.
#'
#' @seealso
#' [tagesschau_homepage()] and [tagesschau_search()].
#'
#' @examples
#' \dontrun{
#' tagesschau_news(ressort = "inland", flatten = TRUE)
#' }
#'
#' @return A tibble with news items.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
tagesschau_news <- function(regions = NULL, ressort = NULL,
                            flatten = FALSE, flatten_mode = "json") {
  params <- list()
  if (!is.null(regions)) {
    params$regions <- regions
  }
  if (!is.null(ressort)) {
    params$ressort <- ressort
  }
  bunddev_call_tidy(
    "tagesschau",
    "news",
    params = params,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Search Tagesschau content
#'
#' @param search_text Query string.
#' @param page_size Results per page.
#' @param result_page Result page index.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Searches Tagesschau content by free-text query. Official docs:
#' https://bundesapi.github.io/tagesschau-api/.
#'
#' @seealso
#' [tagesschau_news()] and [tagesschau_homepage()].
#'
#' @examples
#' \dontrun{
#' tagesschau_search(search_text = "energie", page_size = 10, flatten = TRUE)
#' }
#'
#' @return A tibble with search results.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
tagesschau_search <- function(search_text = NULL,
                              page_size = NULL,
                              result_page = NULL,
                              flatten = FALSE,
                              flatten_mode = "json") {
  params <- list()
  if (!is.null(search_text)) {
    params$searchText <- search_text
  }
  if (!is.null(page_size)) {
    params$pageSize <- page_size
  }
  if (!is.null(result_page)) {
    params$resultPage <- result_page
  }
  bunddev_call_tidy(
    "tagesschau",
    "search",
    params = params,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Fetch Tagesschau channels
#'
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Lists the Tagesschau channels endpoint. Official docs:
#' https://bundesapi.github.io/tagesschau-api/.
#'
#' @seealso
#' [tagesschau_news()] and [tagesschau_homepage()].
#'
#' @examples
#' \dontrun{
#' tagesschau_channels(flatten = TRUE)
#' }
#'
#' @return A tibble with channels.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
tagesschau_channels <- function(flatten = FALSE, flatten_mode = "json") {
  bunddev_call_tidy(
    "tagesschau",
    "channels",
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_tidy_tagesschau <- function(response, operation_id = NULL,
                                    flatten = FALSE, flatten_mode = "json") {
  if (operation_id %in% c("homepage", "news")) {
    news <- bunddev_tidy_tagesschau_items(response$news, section = "news")
    regional <- bunddev_tidy_tagesschau_items(response$regional, section = "regional")
    data <- dplyr::bind_rows(news, regional)
  } else if (operation_id == "search") {
    data <- bunddev_tidy_tagesschau_items(response$searchResults, section = "search")
  } else if (operation_id == "channels") {
    data <- bunddev_tidy_tagesschau_items(response$channels, section = "channels")
  } else {
    data <- tibble::tibble()
  }

  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c(
        "teaser_image",
        "tracking",
        "tags",
        "images",
        "streams",
        "geotags",
        "branding_image",
        "first_frame"
      ),
      mode = flatten_mode
    ))
  }

  data
}

bunddev_tagesschau_homepage <- function(flatten = FALSE, flatten_mode = "json") {
  tagesschau_homepage(flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_tagesschau_news <- function(regions = NULL, ressort = NULL,
                                    flatten = FALSE, flatten_mode = "json") {
  tagesschau_news(
    regions = regions,
    ressort = ressort,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_tagesschau_search <- function(search_text = NULL,
                                      page_size = NULL,
                                      result_page = NULL,
                                      flatten = FALSE,
                                      flatten_mode = "json") {
  tagesschau_search(
    search_text = search_text,
    page_size = page_size,
    result_page = result_page,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_tagesschau_channels <- function(flatten = FALSE, flatten_mode = "json") {
  tagesschau_channels(flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_tidy_tagesschau_items <- function(items, section) {
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble())
  }

  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  lgl_or_na <- function(value) {
    if (is.null(value)) NA else isTRUE(value)
  }
  list_or_empty <- function(value) {
    if (is.null(value)) list() else value
  }

  data <- tibble::tibble(
    section = section,
    sophora_id = purrr::map_chr(items, ~ chr_or_na(.x$sophoraId)),
    external_id = purrr::map_chr(items, ~ chr_or_na(.x$externalId)),
    title = purrr::map_chr(items, ~ chr_or_na(.x$title)),
    date = purrr::map_chr(items, ~ chr_or_na(.x$date)),
    date_time = purrr::map(items, ~ as.POSIXct(.x$date, tz = "Europe/Berlin")),
    topline = purrr::map_chr(items, ~ chr_or_na(.x$topline)),
    first_sentence = purrr::map_chr(items, ~ chr_or_na(.x$firstSentence)),
    details = purrr::map_chr(items, ~ chr_or_na(.x$details)),
    detailsweb = purrr::map_chr(items, ~ chr_or_na(.x$detailsweb)),
    share_url = purrr::map_chr(items, ~ chr_or_na(.x$shareURL)),
    update_check_url = purrr::map_chr(items, ~ chr_or_na(.x$updateCheckUrl)),
    region_id = purrr::map_chr(items, ~ chr_or_na(.x$regionId)),
    ressort = purrr::map_chr(items, ~ chr_or_na(.x$ressort)),
    type = purrr::map_chr(items, ~ chr_or_na(.x$type)),
    breaking_news = purrr::map_lgl(items, ~ lgl_or_na(.x$breakingNews)),
    copyright = purrr::map_chr(items, ~ chr_or_na(.x$copyright)),
    alttext = purrr::map_chr(items, ~ chr_or_na(.x$alttext)),
    teaser_image = purrr::map(items, ~ list_or_empty(.x$teaserImage)),
    tracking = purrr::map(items, ~ list_or_empty(.x$tracking)),
    tags = purrr::map(items, ~ list_or_empty(.x$tags)),
    images = purrr::map(items, ~ list_or_empty(.x$images)),
    streams = purrr::map(items, ~ list_or_empty(.x$streams)),
    geotags = purrr::map(items, ~ list_or_empty(.x$geotags)),
    branding_image = purrr::map(items, ~ list_or_empty(.x$brandingImage)),
    first_frame = purrr::map(items, ~ list_or_empty(.x$firstFrame))
  )

  data
}
