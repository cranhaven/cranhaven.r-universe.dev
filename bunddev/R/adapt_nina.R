#' List NINA travel warnings
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The NINA API provides warnings and app data from the Bundesamt fuer
#' Bevoelkerungsschutz. This endpoint returns a list of travel warnings with
#' metadata. Official docs: https://nina.api.bund.dev.
#'
#' @seealso
#' [nina_warning()] for a single warning and [nina_mapdata()] for
#' map-based alerts.
#'
#' @examples
#' \dontrun{
#' nina_warnings()
#' }
#'
#' @return A tibble with warning metadata.
#'
#' Includes `last_modified_time` and `effective_time` as POSIXct in Europe/Berlin.
#' @export
nina_warnings <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getTravelwarning",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  nina_tidy_entries(response)
}

#' Get a NINA travel warning by content id
#'
#' @param content_id Travel warning content id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns full travel warning content, including HTML blocks.
#'
#' @seealso
#' [nina_warnings()] for ids.
#'
#' @examples
#' \dontrun{
#' warnings <- nina_warnings()
#' nina_warning(warnings$content_id[[1]])
#' }
#'
#' @return A tibble with warning details.
#'
#' Includes `last_modified_time` and `effective_time` as POSIXct in Europe/Berlin.
#' @export
nina_warning <- function(content_id, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getSingleTravelwarning",
    params = list(contentId = content_id),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  nina_tidy_entries(response)
}

#' Get a NINA warning (JSON)
#'
#' @param identifier Warning identifier.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a warning in JSON format.
#'
#' @seealso
#' [nina_mapdata()] to discover identifiers.
#'
#' @examples
#' \dontrun{
#' map <- nina_mapdata("mowas")
#' nina_warning_json(map$id[[1]])
#' }
#'
#' @return A tibble with warning details.
#'
#' Includes `sent_time` as POSIXct in Europe/Berlin.
#' @export
nina_warning_json <- function(identifier, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getWarning",
    params = list(identifier = identifier),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  nina_tidy_warning(response)
}

#' Get a NINA warning (GeoJSON)
#'
#' @param identifier Warning identifier.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns a warning in GeoJSON format.
#'
#' @return A tibble with geojson payload.
#' @export
nina_warning_geojson <- function(identifier, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getWarningGeojson",
    params = list(identifier = identifier),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(identifier = identifier, geojson = list(response))
}

#' Get NINA dashboard data
#'
#' @param ars ARS code.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns dashboard data for the given ARS code. Official docs:
#' https://nina.api.bund.dev.
#'
#' @return A tibble with dashboard payload.
#' @export
nina_dashboard <- function(ars, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getDashboard",
    params = list(ARS = ars),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(ars = ars, data = list(response))
}

#' Get COVID rules for an ARS
#'
#' @param ars ARS code.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with COVID rules data.
#' @export
nina_covid_rules <- function(ars, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getARSCovidRules",
    params = list(ARS = ars),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(ars = ars, data = list(response))
}

#' Get COVID info data
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with COVID info data.
#' @export
nina_covid_infos <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getCovidInfos",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(data = list(response))
}

#' Get COVID ticker
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with COVID ticker data.
#' @export
nina_covid_ticker <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getCovidTicker",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(data = list(response))
}

#' Get COVID ticker message
#'
#' @param id Ticker message id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with ticker message data.
#' @export
nina_covid_ticker_message <- function(id, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getCovidTickerMessage",
    params = list(id = id),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(id = id, data = list(response))
}

#' Get COVID map data
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with COVID map data.
#' @export
nina_covid_map <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getCovidMap",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(data = list(response))
}

#' List NINA logos
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with logo metadata.
#'
#' Includes `last_modification_time` as POSIXct in Europe/Berlin.
#' @export
nina_logos <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getLogos",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  nina_tidy_list(response, list_name = "logos", time_field = "lastModificationDate")
}

#' Get a logo file
#'
#' @param filename Logo file name.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with raw logo bytes.
#' @export
nina_logo <- function(filename, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getLogo",
    params = list(filename = filename),
    parse = "raw",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(filename = filename, bytes = list(response))
}

#' List NINA event codes
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with event codes.
#'
#' Includes `last_modification_time` as POSIXct in Europe/Berlin.
#' @export
nina_event_codes <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getEventCodes",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  nina_tidy_list(response, list_name = "eventCodes", time_field = "lastModificationDate")
}

#' Get an event code file
#'
#' @param filename Event code filename.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with raw event code bytes.
#' @export
nina_event_code <- function(filename, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getEventCode",
    params = list(filename = filename),
    parse = "raw",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(filename = filename, bytes = list(response))
}

#' List emergency tips
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with emergency tips.
#'
#' Includes `last_modification_time` as POSIXct in Europe/Berlin.
#' @export
nina_notfalltipps <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getNotfalltipps",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tips <- response$notfalltipps$category %||% list()
  if (length(tips) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    title = purrr::map_chr(tips, ~ .x$title %||% NA_character_),
    tips = purrr::map(tips, ~ .x$tips %||% list()),
    last_modification_date = purrr::map_dbl(tips, ~ as.numeric(.x$lastModificationDate %||% NA_real_)),
    last_modification_time = purrr::map(purrr::map_dbl(tips, ~ as.numeric(.x$lastModificationDate %||% NA_real_)),
      ~ bunddev_ms_to_posix(.x))
  )
}

#' List FAQs
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with FAQs.
#'
#' Includes `last_modification_time` as POSIXct in Europe/Berlin.
#' @export
nina_faqs <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getFAQs",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  faqs <- response$FAQ %||% list()
  if (length(faqs) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    question = purrr::map_chr(faqs, ~ .x$question %||% NA_character_),
    answer = purrr::map_chr(faqs, ~ .x$answer %||% NA_character_),
    last_modification_date = purrr::map_dbl(faqs, ~ as.numeric(.x$lastModificationDate %||% NA_real_)),
    last_modification_time = purrr::map(purrr::map_dbl(faqs, ~ as.numeric(.x$lastModificationDate %||% NA_real_)),
      ~ bunddev_ms_to_posix(.x))
  )
}

#' Get data version info
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with version data.
#'
#' Includes `last_modification_time` as POSIXct in Europe/Berlin.
#' @export
nina_version <- function(safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getDataVersion",
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(
    data = list(response)
  )
}

#' List map data
#'
#' @param source Map data source.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Valid sources are: `katwarn`, `biwapp`, `mowas`, `dwd`, `lhp`, `police`.
#'
#' @return A tibble with map data entries.
#'
#' Includes `start_date_time` as POSIXct in Europe/Berlin.
#' @export
nina_mapdata <- function(source = c("katwarn", "biwapp", "mowas", "dwd", "lhp", "police"),
                         safe = TRUE,
                         refresh = FALSE) {
  source <- rlang::arg_match(source)
  operation_id <- switch(
    source,
    katwarn = "getKatwarnMapData",
    biwapp = "getBiwappMapData",
    mowas = "getMowasMapData",
    dwd = "getDwdMapData",
    lhp = "getLhpMapData",
    police = "getPoliceMapData"
  )

  response <- bunddev_call(
    "nina",
    operation_id,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- nina_tidy_mapdata(response)
  data$source <- source
  data
}

nina_mapdata_katwarn <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("katwarn", safe = safe, refresh = refresh)
}

nina_mapdata_biwapp <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("biwapp", safe = safe, refresh = refresh)
}

nina_mapdata_mowas <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("mowas", safe = safe, refresh = refresh)
}

nina_mapdata_dwd <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("dwd", safe = safe, refresh = refresh)
}

nina_mapdata_lhp <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("lhp", safe = safe, refresh = refresh)
}

nina_mapdata_police <- function(safe = TRUE, refresh = FALSE) {
  nina_mapdata("police", safe = safe, refresh = refresh)
}

#' Get MOWAS archive mapping
#'
#' @param identifier Warning identifier.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with archive mapping data.
#' @export
nina_archive_mowas_mapping <- function(identifier, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getArchiveMowasMapping",
    params = list(identifier = identifier),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(identifier = identifier, data = list(response))
}

#' Get MOWAS archive entry
#'
#' @param identifier Warning identifier.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with archive entry data.
#' @export
nina_archive_mowas <- function(identifier, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getArchiveMowas",
    params = list(identifier = identifier),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(identifier = identifier, data = list(response))
}

#' Get MOWAS RSS feed
#'
#' @param ars ARS code.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @return A tibble with RSS XML text.
#' @export
nina_mowas_rss <- function(ars, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "nina",
    "getMowasRss",
    params = list(ARS = ars),
    parse = "text",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(ars = ars, rss = response)
}

nina_tidy_entries <- function(response) {
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

  to_time <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Europe/Berlin"))
    }
    as.POSIXct(as.numeric(value[[1]]), origin = "1970-01-01", tz = "Europe/Berlin")
  }

  snake_case <- function(name) {
    name <- stringr::str_replace_all(name, "([a-z0-9])([A-Z])", "\\1_\\2")
    tolower(name)
  }

  purrr::imap_dfr(entries, function(entry, content_id) {
    if (!is.list(entry)) {
      entry <- list(value = entry)
    }

    data <- tibble::as_tibble(entry)
    names(data) <- snake_case(names(data))
    data$content_id <- content_id
    data$response_country <- response_country
    data$response_last_modified <- response_last_modified
    data$last_modified_time <- to_time(entry$lastModified %||% NA_integer_)
    data$effective_time <- to_time(entry$effective %||% NA_integer_)
    if (!is.na(response_last_modified)) {
      data$response_last_modified_time <- to_time(response_last_modified)
    }
    data
  })
}

nina_tidy_warning <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }
  sent_time <- response$sent %||% NA_character_

  tibble::tibble(
    identifier = response$identifier %||% NA_character_,
    sender = response$sender %||% NA_character_,
    sent = response$sent %||% NA_character_,
    status = response$status %||% NA_character_,
    msg_type = response$msgType %||% NA_character_,
    scope = response$scope %||% NA_character_,
    code = list(response$code %||% list()),
    info = list(response$info %||% list()),
    sent_time = as.POSIXct(sent_time, tz = "Europe/Berlin")
  )
}

nina_tidy_list <- function(response, list_name, time_field = NULL) {
  entries <- response[[list_name]]
  if (is.null(entries) || length(entries) == 0) {
    return(tibble::tibble())
  }

  data <- dplyr::bind_rows(entries)
  if (!is.null(time_field)) {
    data$last_modification_date <- as.numeric(response[[time_field]] %||% NA_real_)
    data$last_modification_time <- bunddev_ms_to_posix(data$last_modification_date)
  }
  data
}

nina_tidy_mapdata <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    id = purrr::map_chr(response, ~ .x$id %||% NA_character_),
    version = purrr::map_int(response, ~ as.integer(.x$version %||% NA_integer_)),
    start_date = purrr::map_chr(response, ~ .x$startDate %||% NA_character_),
    severity = purrr::map_chr(response, ~ .x$severity %||% NA_character_),
    urgency = purrr::map_chr(response, ~ .x$urgency %||% NA_character_),
    type = purrr::map_chr(response, ~ .x$type %||% NA_character_),
    i18n_title = purrr::map(response, ~ .x$i18nTitle %||% list()),
    trans_keys = purrr::map(response, ~ .x$transKeys %||% list()),
    start_date_time = as.POSIXct(purrr::map_chr(response, ~ .x$startDate %||% NA_character_), tz = "Europe/Berlin")
  )
}

bunddev_nina_warnings <- function(safe = TRUE, refresh = FALSE) {
  nina_warnings(safe = safe, refresh = refresh)
}

bunddev_nina_warning <- function(content_id, safe = TRUE, refresh = FALSE) {
  nina_warning(content_id = content_id, safe = safe, refresh = refresh)
}

bunddev_nina_warning_json <- function(identifier, safe = TRUE, refresh = FALSE) {
  nina_warning_json(identifier = identifier, safe = safe, refresh = refresh)
}

bunddev_nina_warning_geojson <- function(identifier, safe = TRUE, refresh = FALSE) {
  nina_warning_geojson(identifier = identifier, safe = safe, refresh = refresh)
}

bunddev_nina_mapdata <- function(source = c("katwarn", "biwapp", "mowas", "dwd", "lhp", "police"),
                                 safe = TRUE,
                                 refresh = FALSE) {
  nina_mapdata(source = source, safe = safe, refresh = refresh)
}
