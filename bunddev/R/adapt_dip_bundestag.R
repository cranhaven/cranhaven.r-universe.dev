#' List DIP Vorgang entries
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns metadata for DIP Vorgang entries. Requires an API key. Obtain a key
#' from https://dip.bundestag.de/über-dip/hilfe/api.
#'
#' Configure authentication via [bunddev_auth_set()] or set the
#' `DIP_BUNDESTAG_API_KEY` environment variable directly.
#'
#' @seealso
#' [bunddev_auth_set()] to configure authentication.
#'
#' @examples
#' \dontrun{
#' # Recommended: use bunddev_auth_set
#' Sys.setenv(DIP_BUNDESTAG_API_KEY = "<api-key>")
#' bunddev_auth_set(
#'   "dip_bundestag",
#'   type = "api_key",
#'   env_var = "DIP_BUNDESTAG_API_KEY",
#'   scheme = "ApiKey"
#' )
#' dip_bundestag_vorgang_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_vorgang_list <- function(params = list(),
                                       safe = TRUE,
                                       refresh = FALSE,
                                       flatten = FALSE,
                                       flatten_mode = "json") {
  response <- dip_bundestag_request("/vorgang", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Vorgang
#'
#' @param vorgang_id Vorgang id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns metadata for a single Vorgang.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_vorgang(84343)
#' }
#'
#' @return A tibble with Vorgang metadata.
#' @export
dip_bundestag_vorgang <- function(vorgang_id,
                                  params = list(),
                                  safe = TRUE,
                                  refresh = FALSE) {
  response <- dip_bundestag_request(
    "/vorgang/{id}",
    params = c(list(id = vorgang_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Vorgangsposition entries
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns metadata for Vorgangsposition entries.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_vorgangsposition_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_vorgangsposition_list <- function(params = list(),
                                                safe = TRUE,
                                                refresh = FALSE,
                                                flatten = FALSE,
                                                flatten_mode = "json") {
  response <- dip_bundestag_request("/vorgangsposition", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Vorgangsposition
#'
#' @param vorgangsposition_id Vorgangsposition id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_vorgangsposition(173376)
#' }
#'
#' @return A tibble with Vorgangsposition metadata.
#' @export
dip_bundestag_vorgangsposition <- function(vorgangsposition_id,
                                           params = list(),
                                           safe = TRUE,
                                           refresh = FALSE) {
  response <- dip_bundestag_request(
    "/vorgangsposition/{id}",
    params = c(list(id = vorgangsposition_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Drucksachen
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_drucksache_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_drucksache_list <- function(params = list(),
                                          safe = TRUE,
                                          refresh = FALSE,
                                          flatten = FALSE,
                                          flatten_mode = "json") {
  response <- dip_bundestag_request("/drucksache", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Drucksache
#'
#' @param drucksache_id Drucksache id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_drucksache(68852)
#' }
#'
#' @return A tibble with Drucksache metadata.
#' @export
dip_bundestag_drucksache <- function(drucksache_id,
                                     params = list(),
                                     safe = TRUE,
                                     refresh = FALSE) {
  response <- dip_bundestag_request(
    "/drucksache/{id}",
    params = c(list(id = drucksache_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Drucksache texts
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_drucksache_text_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_drucksache_text_list <- function(params = list(),
                                               safe = TRUE,
                                               refresh = FALSE,
                                               flatten = FALSE,
                                               flatten_mode = "json") {
  response <- dip_bundestag_request("/drucksache-text", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Drucksache text
#'
#' @param drucksache_id Drucksache id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_drucksache_text(68852)
#' }
#'
#' @return A tibble with Drucksache text metadata.
#' @export
dip_bundestag_drucksache_text <- function(drucksache_id,
                                          params = list(),
                                          safe = TRUE,
                                          refresh = FALSE) {
  response <- dip_bundestag_request(
    "/drucksache-text/{id}",
    params = c(list(id = drucksache_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Plenarprotokolle
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_plenarprotokoll_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_plenarprotokoll_list <- function(params = list(),
                                               safe = TRUE,
                                               refresh = FALSE,
                                               flatten = FALSE,
                                               flatten_mode = "json") {
  response <- dip_bundestag_request("/plenarprotokoll", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Plenarprotokoll
#'
#' @param plenarprotokoll_id Plenarprotokoll id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_plenarprotokoll(908)
#' }
#'
#' @return A tibble with Plenarprotokoll metadata.
#' @export
dip_bundestag_plenarprotokoll <- function(plenarprotokoll_id,
                                          params = list(),
                                          safe = TRUE,
                                          refresh = FALSE) {
  response <- dip_bundestag_request(
    "/plenarprotokoll/{id}",
    params = c(list(id = plenarprotokoll_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Plenarprotokoll texts
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_plenarprotokoll_text_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_plenarprotokoll_text_list <- function(params = list(),
                                                    safe = TRUE,
                                                    refresh = FALSE,
                                                    flatten = FALSE,
                                                    flatten_mode = "json") {
  response <- dip_bundestag_request("/plenarprotokoll-text", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Plenarprotokoll text
#'
#' @param plenarprotokoll_id Plenarprotokoll id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_plenarprotokoll_text(908)
#' }
#'
#' @return A tibble with Plenarprotokoll text metadata.
#' @export
dip_bundestag_plenarprotokoll_text <- function(plenarprotokoll_id,
                                               params = list(),
                                               safe = TRUE,
                                               refresh = FALSE) {
  response <- dip_bundestag_request(
    "/plenarprotokoll-text/{id}",
    params = c(list(id = plenarprotokoll_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Aktivitäten
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_aktivitaet_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_aktivitaet_list <- function(params = list(),
                                          safe = TRUE,
                                          refresh = FALSE,
                                          flatten = FALSE,
                                          flatten_mode = "json") {
  response <- dip_bundestag_request("/aktivitaet", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Aktivität
#'
#' @param aktivitaet_id Aktivität id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_aktivitaet(1493545)
#' }
#'
#' @return A tibble with Aktivität metadata.
#' @export
dip_bundestag_aktivitaet <- function(aktivitaet_id,
                                     params = list(),
                                     safe = TRUE,
                                     refresh = FALSE) {
  response <- dip_bundestag_request(
    "/aktivitaet/{id}",
    params = c(list(id = aktivitaet_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

#' List DIP Personen
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_person_list()
#' }
#'
#' @return A tibble with DIP response metadata.
#' @export
dip_bundestag_person_list <- function(params = list(),
                                      safe = TRUE,
                                      refresh = FALSE,
                                      flatten = FALSE,
                                      flatten_mode = "json") {
  response <- dip_bundestag_request("/person", params, safe, refresh)
  dip_bundestag_tidy_list(response, flatten, flatten_mode)
}

#' Get a DIP Person
#'
#' @param person_id Person id.
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @examples
#' \dontrun{
#' dip_bundestag_person(1728)
#' }
#'
#' @return A tibble with person metadata.
#' @export
dip_bundestag_person <- function(person_id,
                                 params = list(),
                                 safe = TRUE,
                                 refresh = FALSE) {
  response <- dip_bundestag_request(
    "/person/{id}",
    params = c(list(id = person_id), params),
    safe = safe,
    refresh = refresh
  )
  dip_bundestag_tidy_detail(response)
}

dip_bundestag_request <- function(path,
                                  params = list(),
                                  safe = TRUE,
                                  refresh = FALSE,
                                  parse = "json") {
  # Set default format parameter
  if (is.null(params$format)) {
    params$format <- "json"
  }

  # bunddev_call() handles auth via bunddev_auth_get/header automatically
  # Configure auth via: bunddev_auth_set("dip_bundestag", type = "api_key",
  #                                      env_var = "DIP_BUNDESTAG_API_KEY", scheme = "ApiKey")

  auth <- bunddev_auth_get("dip_bundestag")
  if (auth$type == "none") {
    # Legacy fallback: check params or env var directly
    api_key <- dip_bundestag_api_key(params)
    if (!is.null(api_key)) {
      bunddev_call(
        "dip_bundestag",
        path = path,
        method = "GET",
        params = params,
        headers = list(Authorization = paste("ApiKey", api_key)),
        parse = parse,
        safe = safe,
        refresh = refresh
      )
    } else {
      bunddev_call(
        "dip_bundestag",
        path = path,
        method = "GET",
        params = params,
        parse = parse,
        safe = safe,
        refresh = refresh
      )
    }
  } else {
    bunddev_call(
      "dip_bundestag",
      path = path,
      method = "GET",
      params = params,
      parse = parse,
      safe = safe,
      refresh = refresh
    )
  }
}

dip_bundestag_api_key <- function(params) {
  if (!is.null(params$apikey) && params$apikey != "") {
    return(params$apikey)
  }

  auth <- bunddev_auth_get("dip_bundestag")
  if (auth$type == "api_key") {
    api_key <- Sys.getenv(auth$env_var)
    if (api_key == "") {
      cli::cli_abort("Environment variable '{auth$env_var}' is not set.")
    }
    return(api_key)
  }

  api_key <- Sys.getenv("DIP_BUNDESTAG_API_KEY")
  if (api_key == "") {
    cli::cli_abort("DIP_BUNDESTAG_API_KEY is not set.")
  }
  api_key
}

dip_bundestag_tidy_list <- function(response, flatten, flatten_mode) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  data <- tibble::tibble(
    num_found = response$numFound %||% NA_integer_,
    cursor = response$cursor %||% NA_character_,
    documents = list(response$documents %||% list())
  )

  if (flatten) {
    return(bunddev_flatten_list_cols(data, cols = "documents", mode = flatten_mode))
  }

  data
}

dip_bundestag_tidy_detail <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  cleaned <- purrr::map(response, function(x) {
    if (is.null(x)) return(NA)
    if (is.list(x) || length(x) > 1) return(list(x))
    x
  })
  tibble::as_tibble(cleaned)
}

bunddev_dip_bundestag_vorgang_list <- function(params = list(),
                                               safe = TRUE,
                                               refresh = FALSE,
                                               flatten = FALSE,
                                               flatten_mode = "json") {
  dip_bundestag_vorgang_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_vorgang <- function(vorgang_id,
                                          params = list(),
                                          safe = TRUE,
                                          refresh = FALSE) {
  dip_bundestag_vorgang(
    vorgang_id = vorgang_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_vorgangsposition_list <- function(params = list(),
                                                        safe = TRUE,
                                                        refresh = FALSE,
                                                        flatten = FALSE,
                                                        flatten_mode = "json") {
  dip_bundestag_vorgangsposition_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_vorgangsposition <- function(vorgangsposition_id,
                                                   params = list(),
                                                   safe = TRUE,
                                                   refresh = FALSE) {
  dip_bundestag_vorgangsposition(
    vorgangsposition_id = vorgangsposition_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_drucksache_list <- function(params = list(),
                                                  safe = TRUE,
                                                  refresh = FALSE,
                                                  flatten = FALSE,
                                                  flatten_mode = "json") {
  dip_bundestag_drucksache_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_drucksache <- function(drucksache_id,
                                             params = list(),
                                             safe = TRUE,
                                             refresh = FALSE) {
  dip_bundestag_drucksache(
    drucksache_id = drucksache_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_drucksache_text_list <- function(params = list(),
                                                       safe = TRUE,
                                                       refresh = FALSE,
                                                       flatten = FALSE,
                                                       flatten_mode = "json") {
  dip_bundestag_drucksache_text_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_drucksache_text <- function(drucksache_id,
                                                  params = list(),
                                                  safe = TRUE,
                                                  refresh = FALSE) {
  dip_bundestag_drucksache_text(
    drucksache_id = drucksache_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_plenarprotokoll_list <- function(params = list(),
                                                       safe = TRUE,
                                                       refresh = FALSE,
                                                       flatten = FALSE,
                                                       flatten_mode = "json") {
  dip_bundestag_plenarprotokoll_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_plenarprotokoll <- function(plenarprotokoll_id,
                                                  params = list(),
                                                  safe = TRUE,
                                                  refresh = FALSE) {
  dip_bundestag_plenarprotokoll(
    plenarprotokoll_id = plenarprotokoll_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_plenarprotokoll_text_list <- function(params = list(),
                                                            safe = TRUE,
                                                            refresh = FALSE,
                                                            flatten = FALSE,
                                                            flatten_mode = "json") {
  dip_bundestag_plenarprotokoll_text_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_plenarprotokoll_text <- function(plenarprotokoll_id,
                                                       params = list(),
                                                       safe = TRUE,
                                                       refresh = FALSE) {
  dip_bundestag_plenarprotokoll_text(
    plenarprotokoll_id = plenarprotokoll_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_aktivitaet_list <- function(params = list(),
                                                  safe = TRUE,
                                                  refresh = FALSE,
                                                  flatten = FALSE,
                                                  flatten_mode = "json") {
  dip_bundestag_aktivitaet_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_aktivitaet <- function(aktivitaet_id,
                                             params = list(),
                                             safe = TRUE,
                                             refresh = FALSE) {
  dip_bundestag_aktivitaet(
    aktivitaet_id = aktivitaet_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}

bunddev_dip_bundestag_person_list <- function(params = list(),
                                              safe = TRUE,
                                              refresh = FALSE,
                                              flatten = FALSE,
                                              flatten_mode = "json") {
  dip_bundestag_person_list(
    params = params,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_dip_bundestag_person <- function(person_id,
                                         params = list(),
                                         safe = TRUE,
                                         refresh = FALSE) {
  dip_bundestag_person(
    person_id = person_id,
    params = params,
    safe = safe,
    refresh = refresh
  )
}
