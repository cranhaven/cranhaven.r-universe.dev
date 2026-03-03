#' Search training offers
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' The Ausbildungssuche API provides training offer data from the
#' Bundesagentur fuer Arbeit. Authentication is required via `X-API-Key`
#' (clientId `infosysbub-absuche`, discoverable from
#' https://web.arbeitsagentur.de/weiterbildungssuche/suche). Official docs:
#' https://bundesapi.github.io/ausbildungssuche-api/.
#'
#' This adapter uses the `X-API-Key` header. Set it via [bunddev_auth_set()] and
#' `AUSBILDUNGSSUCHE_API_KEY`.
#'
#' @seealso
#' [ausbildungssuche_details()] for a single offer and [bunddev_auth_set()] for
#' authentication.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AUSBILDUNGSSUCHE_API_KEY = "infosysbub-absuche")
#' bunddev_auth_set("ausbildungssuche", type = "api_key", env_var = "AUSBILDUNGSSUCHE_API_KEY")
#' ausbildungssuche_search(params = list(size = 5))
#' }
#'
#' @return A tibble with training offers.
#'
#' Includes `aktualisierungsdatum_time` as POSIXct in Europe/Berlin.
#' @export
ausbildungssuche_search <- function(params = list(),
                                    safe = TRUE,
                                    refresh = FALSE,
                                    flatten = FALSE,
                                    flatten_mode = "json") {
  response <- bunddev_call(
    "ausbildungssuche",
    "ausbildungssuche",
    params = params,
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  data <- ausbildungssuche_tidy_response(response)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("unterrichtsform", "dauer", "angebot", "adresse", "links", "aggregations"),
      mode = flatten_mode
    ))
  }

  data
}

#' Get training offer details
#'
#' @param offer_id Offer id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns detailed information for a single offer.
#'
#' @seealso
#' [ausbildungssuche_search()] to find offer ids.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AUSBILDUNGSSUCHE_API_KEY = "infosysbub-absuche")
#' bunddev_auth_set("ausbildungssuche", type = "api_key", env_var = "AUSBILDUNGSSUCHE_API_KEY")
#' ausbildungssuche_details(12345)
#' }
#'
#' @return A tibble with offer details.
#' @export
ausbildungssuche_details <- function(offer_id, safe = TRUE, refresh = FALSE) {
  response <- bunddev_call(
    "ausbildungssuche",
    "ausbildungsdetails",
    params = list(id = offer_id),
    parse = "json",
    safe = safe,
    refresh = refresh
  )

  tibble::tibble(offer_id = offer_id, data = list(response))
}

ausbildungssuche_tidy_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  items <- response$`_embedded`$termine %||% list()
  if (length(items) == 0) {
    return(tibble::tibble())
  }

  to_chr <- function(value) if (is.null(value)) NA_character_ else as.character(value)
  to_num <- function(value) if (is.null(value)) NA_real_ else as.numeric(value)
  to_int <- function(value) if (is.null(value)) NA_integer_ else as.integer(value)

  updated <- purrr::map_dbl(items, ~ to_num(.x$aktualisierungsdatum))

  tibble::tibble(
    id = purrr::map_int(items, ~ to_int(.x$id)),
    unterrichtsform = purrr::map(items, ~ .x$unterrichtsform %||% list()),
    dauer = purrr::map(items, ~ .x$dauer %||% list()),
    anbieterbewertung = purrr::map_chr(items, ~ to_chr(.x$anbieterbewertung)),
    angebot = purrr::map(items, ~ .x$angebot %||% list()),
    adresse = purrr::map(items, ~ .x$adresse %||% list()),
    unterrichtszeiten = purrr::map_chr(items, ~ to_chr(.x$unterrichtszeiten)),
    kosten_wert = purrr::map_chr(items, ~ to_chr(.x$kostenWert)),
    kosten_waehrung = purrr::map_chr(items, ~ to_chr(.x$kostenWaehrung)),
    kosten_bemerkung = purrr::map_chr(items, ~ to_chr(.x$kostenBemerkung)),
    foerderung = purrr::map_lgl(items, ~ isTRUE(.x$foerderung)),
    link = purrr::map_chr(items, ~ to_chr(.x$link)),
    bemerkung = purrr::map_chr(items, ~ to_chr(.x$bemerkung)),
    beginn = purrr::map_chr(items, ~ to_chr(.x$beginn)),
    ende = purrr::map_chr(items, ~ to_chr(.x$ende)),
    individueller_einstieg = purrr::map_lgl(items, ~ isTRUE(.x$individuellerEinstieg)),
    anmeldeschluss = purrr::map_chr(items, ~ to_chr(.x$anmeldeschluss)),
    bemerkung_zeit = purrr::map_chr(items, ~ to_chr(.x$bemerkungZeit)),
    pruefende_stelle = purrr::map_chr(items, ~ to_chr(.x$pruefendeStelle)),
    eigene_angebotsnummer = purrr::map_chr(items, ~ to_chr(.x$eigeneAngebotsnummer)),
    teilnehmer_min = purrr::map_int(items, ~ to_int(.x$teilnehmerMin)),
    teilnehmer_max = purrr::map_int(items, ~ to_int(.x$teilnehmerMax)),
    aktualisierungsdatum = updated,
    aktualisierungsdatum_time = purrr::map(updated, ~ bunddev_ms_to_posix(.x)),
    links = list(response$`_links` %||% list()),
    aggregations = list(response$aggregations %||% list()),
    page = list(response$page %||% list())
  )
}
