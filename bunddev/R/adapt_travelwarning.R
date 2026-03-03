#' List travel warnings
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The travelwarning API provides travel and safety information from the
#' Auswaertiges Amt. This endpoint returns a list of all travel warnings with
#' metadata. Official docs: https://travelwarning.api.bund.dev.
#'
#' @seealso
#' [travelwarning_warning()] for full details of a single warning.
#'
#' @examples
#' \dontrun{
#' travelwarning_warnings()
#' }
#'
#' @return A tibble with travel warnings.
#'
#' Includes `last_modified_time` and `effective_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_warnings <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getTravelwarning",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

#' Get a travel warning by content id
#'
#' @param content_id Travel warning content id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns the full travel warning content, including HTML blocks.
#' Official docs: https://travelwarning.api.bund.dev.
#'
#' @seealso
#' [travelwarning_warnings()] to list available ids.
#'
#' @examples
#' \dontrun{
#' warnings <- travelwarning_warnings()
#' travelwarning_warning(warnings$content_id[[1]])
#' }
#'
#' @return A tibble with travel warning details.
#'
#' Includes `last_modified_time` and `effective_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_warning <- function(content_id, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getSingleTravelwarning",
    params = list(contentId = content_id),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

#' List foreign representatives in Germany
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a list of foreign representatives in Germany. Official docs:
#' https://travelwarning.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' travelwarning_representatives_germany()
#' }
#'
#' @return A tibble with representatives.
#'
#' Includes `last_modified_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_representatives_germany <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getRepresentativesGermany",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

#' List German representatives in foreign countries
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a list of German representatives in foreign countries. Official docs:
#' https://travelwarning.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' travelwarning_representatives_country()
#' }
#'
#' @return A tibble with representatives.
#'
#' Includes `last_modified_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_representatives_country <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getRepresentativesCountry",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

#' List state names documents
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns documents with state name information. Official docs:
#' https://travelwarning.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' travelwarning_state_names()
#' }
#'
#' @return A tibble with state name entries.
#'
#' Includes `last_modified_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_state_names <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getStateNames",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

#' List healthcare documents
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns healthcare documents provided by the Auswaertiges Amt.
#' Official docs: https://travelwarning.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' travelwarning_healthcare()
#' }
#'
#' @return A tibble with healthcare entries.
#'
#' Includes `last_modified_time` as POSIXct in Europe/Berlin.
#' @export
travelwarning_healthcare <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "travelwarning",
    "getHealthcare",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  travelwarning_tidy_entries(response)
}

travelwarning_tidy_entries <- function(response) {
  payload <- response$response %||% response
  if (is.null(payload) || length(payload) == 0) {
    return(tibble::tibble())
  }

  response_country <- payload$country %||% NA_character_
  response_last_modified <- payload$lastModified %||% NA_integer_

  entries <- payload[setdiff(names(payload), c("lastModified", "contentList", "country"))]
  if (length(entries) == 0) {
    return(tibble::tibble())
  }

  snake_case <- function(name) {
    name <- stringr::str_replace_all(name, "([a-z0-9])([A-Z])", "\\1_\\2")
    tolower(name)
  }

  to_time <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Europe/Berlin"))
    }
    as.POSIXct(as.numeric(value[[1]]), origin = "1970-01-01", tz = "Europe/Berlin")
  }

  is_id_key <- function(name) {
    stringr::str_detect(name, "^[0-9]+$")
  }

  build_row <- function(entry, content_id, parent_content_id, country) {
    if (!is.list(entry)) {
      entry <- list(value = entry)
    }

    data <- tibble::as_tibble(entry)
    names(data) <- snake_case(names(data))
    data$content_id <- content_id
    data$parent_content_id <- parent_content_id
    data$response_country <- country
    data$response_last_modified <- response_last_modified
    data$last_modified_time <- to_time(entry$lastModified %||% NA_integer_)
    data$effective_time <- to_time(entry$effective %||% NA_integer_)
    if (!is.na(response_last_modified)) {
      data$response_last_modified_time <- to_time(response_last_modified)
    }
    data
  }

  purrr::imap_dfr(entries, function(entry, content_id) {
    if (!is.list(entry)) {
      return(build_row(entry, content_id, NA_character_, response_country))
    }

    entry_country <- entry$country %||% response_country
    nested_entries <- entry[is_id_key(names(entry))]
    if (length(nested_entries) > 0 && !is.null(entry$contentList)) {
      return(purrr::imap_dfr(nested_entries, function(nested, nested_id) {
        build_row(nested, nested_id, content_id, entry_country)
      }))
    }

    build_row(entry, content_id, NA_character_, entry_country)
  })
}

bunddev_travelwarning_warnings <- function(safe = TRUE, refresh = FALSE) {
  travelwarning_warnings(safe = safe, refresh = refresh)
}

bunddev_travelwarning_warning <- function(content_id, safe = TRUE, refresh = FALSE) {
  travelwarning_warning(content_id = content_id, safe = safe, refresh = refresh)
}

bunddev_travelwarning_representatives_germany <- function(safe = TRUE, refresh = FALSE) {
  travelwarning_representatives_germany(safe = safe, refresh = refresh)
}

bunddev_travelwarning_representatives_country <- function(safe = TRUE, refresh = FALSE) {
  travelwarning_representatives_country(safe = safe, refresh = refresh)
}

bunddev_travelwarning_state_names <- function(safe = TRUE, refresh = FALSE) {
  travelwarning_state_names(safe = safe, refresh = refresh)
}

bunddev_travelwarning_healthcare <- function(safe = TRUE, refresh = FALSE) {
  travelwarning_healthcare(safe = safe, refresh = refresh)
}
