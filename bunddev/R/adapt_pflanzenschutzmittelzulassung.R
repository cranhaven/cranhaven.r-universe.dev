#' List approved plant protection products
#'
#' @param kennr Optional product identification number (9 characters).
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' The Pflanzenschutzmittelzulassung API provides access to Germany's plant
#' protection product database from the Bundesamt fuer Verbraucherschutz und
#' Lebensmittelsicherheit (BVL). This function returns approved pesticides.
#' Official docs: https://github.com/bundesAPI/pflanzenschutzmittelzulassung-api.
#'
#' @seealso
#' [psm_wirkstoffe()] to list active ingredients,
#' [psm_stand()] for data version.
#'
#' @examples
#' \dontrun{
#' psm_mittel()
#' psm_mittel(kennr = "024780-00")
#' }
#'
#' @return A tibble with plant protection product data.
#' @export
psm_mittel <- function(kennr = NULL,
                       params = list(),
                       safe = TRUE,
                       refresh = FALSE) {
  if (!is.null(kennr)) {
    params$kennr <- kennr
  }

  response <- psm_request(
    "/mittel/",
    params = params,
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

#' List active ingredients
#'
#' @param wirkstoffId Optional active ingredient ID.
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns active ingredients (Wirkstoffe) from the plant protection product
#' database.
#'
#' @seealso
#' [psm_mittel()] to list products.
#'
#' @examples
#' \dontrun{
#' psm_wirkstoffe()
#' }
#'
#' @return A tibble with active ingredient data.
#' @export
psm_wirkstoffe <- function(wirkstoffId = NULL,
                           params = list(),
                           safe = TRUE,
                           refresh = FALSE) {
  if (!is.null(wirkstoffId)) {
    params$wirkstoffId <- wirkstoffId
  }

  response <- psm_request(
    "/wirkstoff/",
    params = params,
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

#' List approved applications
#'
#' @param kennr Optional product identification number.
#' @param awg_id Optional application identifier (16 characters).
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns approved applications (Anwendungsgebiete) which define the
#' combinations of products, crops, and pests for which use is permitted.
#'
#' @seealso
#' [psm_mittel()] to list products.
#'
#' @examples
#' \dontrun{
#' psm_anwendungen(kennr = "024780-00")
#' }
#'
#' @return A tibble with application data.
#' @export
psm_anwendungen <- function(kennr = NULL,
                            awg_id = NULL,
                            params = list(),
                            safe = TRUE,
                            refresh = FALSE) {
  if (!is.null(kennr)) {
    params$kennr <- kennr
  }
  if (!is.null(awg_id)) {
    params$awg_id <- awg_id
  }

  response <- psm_request(
    "/awg/",
    params = params,
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

#' Get data version
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns the release date/version of the plant protection product database.
#'
#' @examples
#' \dontrun{
#' psm_stand()
#' }
#'
#' @return A tibble with version information.
#' @export
psm_stand <- function(safe = TRUE, refresh = FALSE) {
  response <- psm_request(
    "/stand/",
    params = list(),
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

#' List crop groups
#'
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns hierarchical crop group classifications.
#'
#' @examples
#' \dontrun{
#' psm_kultur_gruppen()
#' }
#'
#' @return A tibble with crop group data.
#' @export
psm_kultur_gruppen <- function(params = list(),
                               safe = TRUE,
                               refresh = FALSE) {
  response <- psm_request(
    "/kultur_gruppe/",
    params = params,
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

#' List pest groups
#'
#' @param params Additional query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns hierarchical pest/pathogen group classifications.
#'
#' @examples
#' \dontrun{
#' psm_schadorg_gruppen()
#' }
#'
#' @return A tibble with pest group data.
#' @export
psm_schadorg_gruppen <- function(params = list(),
                                 safe = TRUE,
                                 refresh = FALSE) {
  response <- psm_request(
    "/schadorg_gruppe/",
    params = params,
    safe = safe,
    refresh = refresh
  )

  psm_tidy_items(response)
}

psm_request <- function(path, params = list(), safe = TRUE, refresh = FALSE, parse = "json") {
  bunddev_call(
    "pflanzenschutzmittelzulassung",
    path = path,
    method = "GET",
    params = params,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

psm_tidy_items <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  items <- response$items
  if (is.null(items) || length(items) == 0) {
    if (is.data.frame(response)) {
      return(tibble::as_tibble(response))
    }
    return(tibble::tibble(response = list(response)))
  }

  if (is.data.frame(items)) {
    data <- tibble::as_tibble(items)
    if ("m_row$$" %in% names(data)) {
      data$`m_row$$` <- NULL
    }
    return(data)
  }

  rows <- purrr::map(items, function(item) {
    item$`m_row$$` <- NULL
    tibble::as_tibble(purrr::map(item, ~ if (is.null(.x)) NA else .x))
  })

  dplyr::bind_rows(rows)
}

bunddev_psm_mittel <- function(kennr = NULL,
                               params = list(),
                               safe = TRUE,
                               refresh = FALSE) {
  psm_mittel(
    kennr = kennr,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_psm_wirkstoffe <- function(wirkstoffId = NULL,
                                   params = list(),
                                   safe = TRUE,
                                   refresh = FALSE) {
  psm_wirkstoffe(
    wirkstoffId = wirkstoffId,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_psm_anwendungen <- function(kennr = NULL,
                                    awg_id = NULL,
                                    params = list(),
                                    safe = TRUE,
                                    refresh = FALSE) {
  psm_anwendungen(
    kennr = kennr,
    awg_id = awg_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_psm_stand <- function(safe = TRUE, refresh = FALSE) {
  psm_stand(safe = safe, refresh = refresh)
}

bunddev_psm_kultur_gruppen <- function(params = list(),
                                       safe = TRUE,
                                       refresh = FALSE) {
  psm_kultur_gruppen(params = params, safe = safe, refresh = refresh)
}

bunddev_psm_schadorg_gruppen <- function(params = list(),
                                         safe = TRUE,
                                         refresh = FALSE) {
  psm_schadorg_gruppen(params = params, safe = safe, refresh = refresh)
}
