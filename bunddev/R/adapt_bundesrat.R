#' List Bundesrat API endpoints
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns the start list for the Bundesrat mobile API, including the URLs for
#' other available resources. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_startlist()
#' }
#'
#' @return A tibble with endpoint metadata.
#' @export
bundesrat_startlist <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/startlist_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List current Bundesrat news
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns current news items from the Bundesrat app feed. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_aktuelles()
#' }
#'
#' @return A tibble with news items.
#' @export
bundesrat_aktuelles <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/01_Aktuelles/aktuelles_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat dates and events
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns scheduled Bundesrat dates and events. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_termine()
#' }
#'
#' @return A tibble with dates and events.
#' @export
bundesrat_termine <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/02_Termine/termine_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat plenum compact entries
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns plenum compact entries for Bundesrat sessions. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_plenum_kompakt()
#' }
#'
#' @return A tibble with plenum compact entries.
#' @export
bundesrat_plenum_kompakt <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/03_Plenum/plenum_kompakt_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat current plenum session entries
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns entries for the current plenum session. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_plenum_aktuelle_sitzung()
#' }
#'
#' @return A tibble with current plenum session entries.
#' @export
bundesrat_plenum_aktuelle_sitzung <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/SharedDocs/3_Plenum/plenum_aktuelleSitzung_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat plenum entries in chronological order
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns plenum entries ordered chronologically. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_plenum_chronologisch()
#' }
#'
#' @return A tibble with plenum entries.
#' @export
bundesrat_plenum_chronologisch <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/SharedDocs/3_Plenum/plenum_toChronologisch_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' Get Bundesrat upcoming plenum sessions
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns upcoming Bundesrat sessions. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_plenum_naechste_sitzungen()
#' }
#'
#' @return A tibble with upcoming session metadata.
#' @export
bundesrat_plenum_naechste_sitzungen <- function(view = "render[iOSDetailsWithoutInnerDate]",
                                                safe = TRUE,
                                                refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/SharedDocs/3_Plenum/plenum_naechsteSitzungen.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat members
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns Bundesrat member entries from the mobile feed. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_mitglieder()
#' }
#'
#' @return A tibble with member entries.
#' @export
bundesrat_mitglieder <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/SharedDocs/2_Mitglieder/mitglieder_table.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' Get Bundesrat voting distribution
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns the Bundesrat voting distribution. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_stimmverteilung()
#' }
#'
#' @return A tibble with voting distribution metadata.
#' @export
bundesrat_stimmverteilung <- function(view = "render[iOSDetailsWithoutInnerDate]",
                                      safe = TRUE,
                                      refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/06_Stimmen/stimmverteilung.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

#' List Bundesrat presidium entries
#'
#' @param view Rendering mode for the XML output.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns Bundesrat presidium entries. Official docs:
#' https://bundesrat.api.bund.dev.
#'
#' @examples
#' \dontrun{
#' bundesrat_praesidium()
#' }
#'
#' @return A tibble with presidium entries.
#' @export
bundesrat_praesidium <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  response <- bundesrat_request(
    "/iOS/v3/05_Bundesrat/Praesidium/bundesrat_praesidium.xml",
    params = list(view = view),
    safe = safe,
    refresh = refresh
  )

  bundesrat_tidy_items(response)
}

bundesrat_request <- function(path,
                              params = list(),
                              safe = TRUE,
                              refresh = FALSE,
                              parse = "xml",
                              operation_id = NULL) {
  bunddev_call(
    "bundesrat",
    path = path,
    method = "GET",
    params = params,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

bundesrat_tidy_items <- function(document) {
  if (is.null(document)) {
    return(tibble::tibble())
  }

  root <- xml2::xml_root(document)
  root_name <- xml2::xml_name(root)

  if (identical(root_name, "iOS")) {
    items <- xml2::xml_find_all(root, ".//list/item")
  } else if (identical(root_name, "item")) {
    items <- list(root)
  } else {
    items <- xml2::xml_find_all(root, ".//item")
  }

  if (length(items) == 0) {
    return(tibble::tibble())
  }

  data <- purrr::map_dfr(items, bundesrat_item_to_row)
  bundesrat_add_time_cols(data)
}

bundesrat_item_to_row <- function(item) {
  children <- xml2::xml_children(item)
  if (length(children) == 0) {
    return(tibble::tibble())
  }

  child_names <- xml2::xml_name(children)
  unique_names <- unique(child_names)

  values <- purrr::map(unique_names, function(name) {
    nodes <- children[child_names == name]
    texts <- xml2::xml_text(nodes, trim = TRUE)
    if (length(texts) == 1) {
      text <- texts[[1]]
      if (is.null(text) || text == "") {
        return(NA_character_)
      }
      return(text)
    }
    list(texts)
  })

  names(values) <- unique_names
  tibble::as_tibble(values)
}

bundesrat_add_time_cols <- function(data) {
  time_fields <- c("date", "dateOfIssue", "imageDate", "layoutDate", "startdate", "stopdate")
  fields <- intersect(time_fields, names(data))
  if (length(fields) == 0) {
    return(data)
  }

  for (field in fields) {
    data[[paste0(field, "_time")]] <- do.call(
      c,
      purrr::map(data[[field]], bundesrat_parse_time)
    )
  }

  data
}

bundesrat_parse_time <- function(value) {
  if (is.list(value)) {
    value <- value[[1]]
  }
  if (is.null(value) || length(value) == 0) {
    return(as.POSIXct(NA_character_, tz = "Europe/Berlin"))
  }
  value <- trimws(as.character(value))
  if (is.na(value) || value == "") {
    return(as.POSIXct(NA_character_, tz = "Europe/Berlin"))
  }

  formats <- c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d.%m.%Y")
  for (fmt in formats) {
    parsed <- as.POSIXct(value, format = fmt, tz = "Europe/Berlin")
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  as.POSIXct(NA_character_, tz = "Europe/Berlin")
}

bunddev_bundesrat_startlist <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_startlist(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_aktuelles <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_aktuelles(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_termine <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_termine(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_plenum_kompakt <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_plenum_kompakt(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_plenum_aktuelle_sitzung <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_plenum_aktuelle_sitzung(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_plenum_chronologisch <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_plenum_chronologisch(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_plenum_naechste_sitzungen <- function(view = "render[iOSDetailsWithoutInnerDate]",
                                                        safe = TRUE,
                                                        refresh = FALSE) {
  bundesrat_plenum_naechste_sitzungen(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_mitglieder <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_mitglieder(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_stimmverteilung <- function(view = "render[iOSDetailsWithoutInnerDate]",
                                              safe = TRUE,
                                              refresh = FALSE) {
  bundesrat_stimmverteilung(view = view, safe = safe, refresh = refresh)
}

bunddev_bundesrat_praesidium <- function(view = "renderXml", safe = TRUE, refresh = FALSE) {
  bundesrat_praesidium(view = view, safe = safe, refresh = refresh)
}
