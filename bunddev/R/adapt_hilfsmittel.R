#' List Hilfsmittel tree nodes
#'
#' @param level Tree level to retrieve (1-4).
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#'
#' @details
#' Returns nodes from the Hilfsmittel product tree up to the selected level.
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_tree(level = 1)
#' }
#'
#' @return A tibble with tree nodes.
#' @export
hilfsmittel_tree <- function(level, safe = TRUE, refresh = FALSE) {
  response <- hilfsmittel_request(
    path = "/VerzeichnisTree/{level}",
    params = list(level = level),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_tidy_response(response)
}

#' Get Hilfsmittel product group details
#'
#' @param id Produktgruppe id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns details for a product group (Produktgruppe).
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' tree <- hilfsmittel_tree(level = 1)
#' hilfsmittel_produktgruppe(tree$id[[1]])
#' }
#'
#' @return A tibble with product group details.
#' @export
hilfsmittel_produktgruppe <- function(id,
                                      safe = TRUE,
                                      refresh = FALSE,
                                      flatten = FALSE,
                                      flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Produktgruppe/{id}",
    params = list(id = id),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

#' Get Hilfsmittel subgroup details
#'
#' @param id Untergruppe id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns details for a subgroup (Untergruppe).
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_untergruppe("c92d1976-d3cb-4b9f-bcdf-805272a9ea86")
#' }
#'
#' @return A tibble with subgroup details.
#' @export
hilfsmittel_untergruppe <- function(id,
                                    safe = TRUE,
                                    refresh = FALSE,
                                    flatten = FALSE,
                                    flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Untergruppe/{id}",
    params = list(id = id),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

#' Get Hilfsmittel product type details
#'
#' @param id Produktart id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns details for a product type (Produktart).
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_produktart("e6b913ef-cf21-4c5f-826d-f866516c3c65")
#' }
#'
#' @return A tibble with product type details.
#' @export
hilfsmittel_produktart <- function(id,
                                   safe = TRUE,
                                   refresh = FALSE,
                                   flatten = FALSE,
                                   flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Produktart/{id}",
    params = list(id = id),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

#' List Hilfsmittel products
#'
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns the full product list (large payload). Official docs:
#' https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_produkte()
#' }
#'
#' @return A tibble with products.
#' @export
hilfsmittel_produkte <- function(safe = TRUE,
                                 refresh = FALSE,
                                 flatten = FALSE,
                                 flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Produkt",
    params = list(),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

#' Get Hilfsmittel product details
#'
#' @param id Produkt id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns detail information for a single product.
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_produkt("f41f52a6-5d2d-4dd3-9d0e-39675ceca7f3")
#' }
#'
#' @return A tibble with product details.
#' @export
hilfsmittel_produkt <- function(id,
                                safe = TRUE,
                                refresh = FALSE,
                                flatten = FALSE,
                                flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Produkt/{id}",
    params = list(id = id),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

#' Get Hilfsmittel verification schema details
#'
#' @param id Nachweisschema id.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @details
#' Returns detail information for a Nachweisschema.
#' Official docs: https://github.com/bundesAPI/hilfsmittel-api.
#'
#' @examples
#' \dontrun{
#' hilfsmittel_nachweisschema("a3d37017-2c91-4d6d-bbbe-4002d2868044")
#' }
#'
#' @return A tibble with Nachweisschema details.
#' @export
hilfsmittel_nachweisschema <- function(id,
                                       safe = TRUE,
                                       refresh = FALSE,
                                       flatten = FALSE,
                                       flatten_mode = "json") {
  response <- hilfsmittel_request(
    path = "/Nachweisschema/{id}",
    params = list(id = id),
    safe = safe,
    refresh = refresh
  )

  hilfsmittel_maybe_flatten(hilfsmittel_tidy_response(response), flatten, flatten_mode)
}

hilfsmittel_request <- function(path,
                              params = list(),
                              safe = TRUE,
                              refresh = FALSE,
                              parse = "json") {
  bunddev_call(
    "hilfsmittel",
    path = path,
    method = "GET",
    params = params,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

bunddev_hilfsmittel_tree <- function(level, safe = TRUE, refresh = FALSE) {
  hilfsmittel_request("/api/v1/tree", safe = safe, refresh = refresh)
}

hilfsmittel_tidy_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(tibble::tibble())
  }

  is_list_of_lists <- function(value) {
    is.list(value) && length(value) > 0 && all(purrr::map_lgl(value, is.list))
  }

  if (is_list_of_lists(response)) {
    data <- purrr::map_dfr(response, function(item) {
      cleaned <- purrr::map(item, ~ if (is.null(.x)) NA else .x)
      tibble::as_tibble(cleaned)
    })
  } else {
    cleaned <- purrr::map(response, ~ if (is.null(.x)) NA else .x)
    data <- tibble::as_tibble(cleaned)
  }

  hilfsmittel_add_time_cols(data)
}

hilfsmittel_add_time_cols <- function(data) {
  time_fields <- c("aufnahmeDatum", "aenderungsDatum")
  fields <- intersect(time_fields, names(data))
  if (length(fields) == 0) {
    return(data)
  }

  for (field in fields) {
    data[[paste0(field, "_time")]] <- as.POSIXct(data[[field]], tz = "Europe/Berlin")
  }

  data
}

hilfsmittel_maybe_flatten <- function(data, flatten, flatten_mode) {
  if (!isTRUE(flatten)) {
    return(data)
  }

  list_cols <- names(data)[purrr::map_lgl(data, is.list)]
  bunddev_flatten_list_cols(data, cols = list_cols, mode = flatten_mode)
}

bunddev_hilfsmittel_tree <- function(level, safe = TRUE, refresh = FALSE) {
  hilfsmittel_tree(level = level, safe = safe, refresh = refresh)
}

bunddev_hilfsmittel_produktgruppe <- function(id,
                                              safe = TRUE,
                                              refresh = FALSE,
                                              flatten = FALSE,
                                              flatten_mode = "json") {
  hilfsmittel_produktgruppe(
    id = id,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_hilfsmittel_untergruppe <- function(id,
                                            safe = TRUE,
                                            refresh = FALSE,
                                            flatten = FALSE,
                                            flatten_mode = "json") {
  hilfsmittel_untergruppe(
    id = id,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_hilfsmittel_produktart <- function(id,
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  hilfsmittel_produktart(
    id = id,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_hilfsmittel_produkte <- function(safe = TRUE,
                                         refresh = FALSE,
                                         flatten = FALSE,
                                         flatten_mode = "json") {
  hilfsmittel_produkte(
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_hilfsmittel_produkt <- function(id,
                                        safe = TRUE,
                                        refresh = FALSE,
                                        flatten = FALSE,
                                        flatten_mode = "json") {
  hilfsmittel_produkt(
    id = id,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_hilfsmittel_nachweisschema <- function(id,
                                               safe = TRUE,
                                               refresh = FALSE,
                                               flatten = FALSE,
                                               flatten_mode = "json") {
  hilfsmittel_nachweisschema(
    id = id,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}
