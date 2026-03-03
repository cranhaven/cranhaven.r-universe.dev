#' List German public holidays
#'
#' @param jahr Year to query (defaults to current year on the API).
#' @param nur_land Optional Bundesland code to filter.
#' @param nur_daten Logical; return only date values (1) or include names (0).
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The Feiertage API returns German public holidays from a Wikipedia-based
#' dataset. The endpoint supports filtering by year and Bundesland. Official
#' docs: https://github.com/bundesAPI/feiertage-api.
#'
#' @seealso
#' [bunddev_parameters()] for available query parameters.
#'
#' @examples
#' \dontrun{
#' feiertage_list(jahr = 2024)
#' feiertage_list(jahr = 2024, nur_land = "BY")
#' }
#'
#' @return A tibble with holiday names and dates.
#'
#' Region-level results include a `region` column and a `note` column for
#' holiday-specific hints.
#'
#' Includes `date_time` as POSIXct in Europe/Berlin.
#' @export
feiertage_list <- function(jahr = NULL,
                           nur_land = NULL,
                           nur_daten = NULL,
                           safe = TRUE,
                           refresh = FALSE) {
  params <- list()
  if (!is.null(jahr)) {
    params$jahr <- jahr
  }
  if (!is.null(nur_land)) {
    params$nur_land <- nur_land
  }
  if (!is.null(nur_daten)) {
    params$nur_daten <- as.integer(nur_daten)
  }

  response <- bunddev_call(
    "feiertage",
    "getFeiertage",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  is_holiday_entry <- function(value) {
    is.list(value) && any(c("datum", "hinweis") %in% names(value))
  }

  build_rows <- function(region, entries) {
    if (is.null(entries) || length(entries) == 0) {
      return(tibble::tibble())
    }

    if (all(purrr::map_lgl(entries, is.character))) {
      dates <- unname(unlist(entries))
      tibble::tibble(
        region = region,
        holiday = names(entries),
        date = as.character(dates),
        note = NA_character_,
        date_time = as.POSIXct(dates, tz = "Europe/Berlin")
      )
    } else {
      dates <- purrr::map_chr(entries, ~ as.character(.x$datum %||% NA_character_))
      notes <- purrr::map_chr(entries, ~ as.character(.x$hinweis %||% NA_character_))
      tibble::tibble(
        region = region,
        holiday = names(entries),
        date = dates,
        note = notes,
        date_time = as.POSIXct(dates, tz = "Europe/Berlin")
      )
    }
  }

  if (all(purrr::map_lgl(response, is.character)) || all(purrr::map_lgl(response, is_holiday_entry))) {
    region <- if (!is.null(nur_land)) nur_land else NA_character_
    return(build_rows(region, response))
  }

  purrr::imap_dfr(response, ~ build_rows(.y, .x))
}

bunddev_feiertage_list <- function(jahr = NULL,
                                   nur_land = NULL,
                                   nur_daten = NULL,
                                   safe = TRUE,
                                   refresh = FALSE) {
  feiertage_list(
    jahr = jahr,
    nur_land = nur_land,
    nur_daten = nur_daten,
    safe = safe,
    refresh = refresh
  )
}
