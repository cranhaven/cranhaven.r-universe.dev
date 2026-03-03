#' Search the Handelsregister portal
#'
#' This adapter scrapes the public Handelsregister search form because no
#' official OpenAPI specification is available for this service.
#'
#' @details
#' The Handelsregister search is provided via a public web form. This helper
#' automates the form flow and parses the result table into a tidy tibble.
#' Official docs: https://github.com/bundesAPI/handelsregister.
#'
#' The registry notes that more than 60 requests per hour may violate the terms
#' of use. Use `safe = TRUE` to respect the built-in rate limiting.
#'
#' @seealso
#' [bunddev_rate_limit_get()] to inspect the configured limit.
#'
#' @examples
#' \dontrun{
#' handelsregister_search("Deutsche Bahn", schlagwort_optionen = "all")
#' }
#'
#' @param schlagwoerter Search terms.
#' @param schlagwort_optionen Keyword options.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble with search results.
#' @export
handelsregister_search <- function(schlagwoerter,
                                   schlagwort_optionen = c("all", "min", "exact"),
                                   safe = TRUE,
                                   refresh = FALSE,
                                   flatten = FALSE,
                                   flatten_mode = "json") {
  schlagwort_optionen <- rlang::arg_match(schlagwort_optionen)

  if (isTRUE(safe)) {
    bunddev_rate_limit_wait("handelsregister")
  }

  params <- list(
    schlagwoerter = schlagwoerter,
    schlagwort_optionen = schlagwort_optionen
  )

  cache_path <- NULL
  if (isTRUE(safe)) {
    cache_path <- bunddev_response_cache_path("handelsregister", "search", params)
    if (!isTRUE(refresh) && file.exists(cache_path)) {
      raw_body <- readBin(cache_path, "raw", n = file.info(cache_path)$size)
      return(bunddev_handelsregister_parse(rawToChar(raw_body)))
    }
  }

  cookie_path <- tempfile(fileext = ".cookie")
  start_req <- httr2::request("https://www.handelsregister.de") |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_timeout(10)
  start_resp <- httr2::req_perform(start_req)
  start_html <- httr2::resp_body_string(start_resp)

  navi_form <- bunddev_handelsregister_find_form(start_html, "naviForm")
  navi_fields <- bunddev_handelsregister_form_fields(navi_form)
  navi_fields[["naviForm:erweiterteSucheLink"]] <- "naviForm:erweiterteSucheLink"
  navi_fields[["target"]] <- "erweiterteSucheLink"

  navi_url <- bunddev_handelsregister_form_action(navi_form, "https://www.handelsregister.de")
  navi_req <- httr2::request(navi_url) |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_timeout(10) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(!!!navi_fields)
  search_resp <- httr2::req_perform(navi_req)
  search_html <- httr2::resp_body_string(search_resp)

  form <- bunddev_handelsregister_find_form(search_html, "form")
  form_fields <- bunddev_handelsregister_form_fields(form)
  form_fields[["form:schlagwoerter"]] <- schlagwoerter
  form_fields[["form:schlagwortOptionen"]] <- bunddev_handelsregister_option_id(schlagwort_optionen)
  form_fields[["form:btnSuche"]] <- ""

  form_url <- bunddev_handelsregister_form_action(form, "https://www.handelsregister.de")
  form_req <- httr2::request(form_url) |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_timeout(10) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(!!!form_fields)
  results_resp <- httr2::req_perform(form_req)
  results_html <- httr2::resp_body_string(results_resp)

  if (!is.null(cache_path)) {
    writeBin(charToRaw(results_html), cache_path)
  }

  data <- bunddev_handelsregister_parse(results_html)
  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("history", "documents_links"),
      mode = flatten_mode
    ))
  }

  data
}

bunddev_handelsregister_search <- function(schlagwoerter,
                                           schlagwort_optionen = c("all", "min", "exact"),
                                           safe = TRUE,
                                           refresh = FALSE,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  handelsregister_search(
    schlagwoerter,
    schlagwort_optionen = schlagwort_optionen,
    safe = safe,
    refresh = refresh,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_handelsregister_option_id <- function(option) {
  switch(
    option,
    all = "1",
    min = "2",
    exact = "3"
  )
}

bunddev_handelsregister_form_action <- function(form, base_url) {
  action <- xml2::xml_attr(form, "action")
  bunddev_handelsregister_url_absolute(action, base_url)
}

bunddev_handelsregister_find_form <- function(html, name) {
  doc <- xml2::read_html(html)
  form <- xml2::xml_find_first(doc, paste0("//form[@name='", name, "']"))
  if (is.na(form)) {
    cli::cli_abort("Could not find form '{name}' on handelsregister page.")
  }
  form
}

bunddev_handelsregister_form_fields <- function(form) {
  inputs <- xml2::xml_find_all(form, ".//input")
  fields <- list()

  for (input in inputs) {
    name <- xml2::xml_attr(input, "name")
    if (is.na(name) || name == "") {
      next
    }
    value <- xml2::xml_attr(input, "value")
    if (is.na(value)) {
      value <- ""
    }
    fields[[name]] <- value
  }

  fields
}

bunddev_handelsregister_parse <- function(html) {
  if (stringr::str_detect(html, "Your session has expired")) {
    cli::cli_abort("Handelsregister session expired; please retry.")
  }

  doc <- xml2::read_html(html)

  # Check for actual error state: the error-message div is always in the HTML

  # template, so only treat it as an error if there is no result grid with data
  table <- xml2::xml_find_first(doc, "//table[@role='grid']")
  if (is.na(table)) {
    return(tibble::tibble())
  }

  rows <- xml2::xml_find_all(table, ".//tr[@data-ri]")
  if (length(rows) == 0) {
    return(tibble::tibble())
  }

  results <- purrr::map(rows, bunddev_handelsregister_parse_row)
  tibble::tibble(
    name = purrr::map_chr(results, "name"),
    court = purrr::map_chr(results, "court"),
    register_num = purrr::map_chr(results, "register_num"),
    state = purrr::map_chr(results, "state"),
    status = purrr::map_chr(results, "status"),
    status_current = purrr::map_chr(results, "status_current"),
    documents_text = purrr::map_chr(results, "documents_text"),
    documents_count = purrr::map_int(results, "documents_count"),
    documents_links = purrr::map(results, "documents_links"),
    history = purrr::map(results, "history")
  )
}

bunddev_handelsregister_parse_row <- function(row) {
  cells <- xml2::xml_find_all(row, ".//td")
  values <- xml2::xml_text(cells, trim = TRUE)

  court <- bunddev_handelsregister_value(values, 2)
  name <- bunddev_handelsregister_value(values, 3)
  state <- bunddev_handelsregister_value(values, 4)
  status <- bunddev_handelsregister_value(values, 5)
  documents_text <- bunddev_handelsregister_value(values, 6)
  documents_links <- bunddev_handelsregister_document_links(row)
  documents_count <- length(documents_links)

  register_num <- bunddev_handelsregister_extract_register(court)
  status_current <- stringr::str_to_upper(stringr::str_replace_all(status, " ", "_"))
  history <- bunddev_handelsregister_history(values)

  list(
    name = name,
    court = court,
    register_num = register_num,
    state = state,
    status = status,
    status_current = status_current,
    documents_text = documents_text,
    documents_links = documents_links,
    documents_count = documents_count,
    history = history
  )
}

bunddev_handelsregister_document_links <- function(row) {
  links <- xml2::xml_find_all(row, ".//td[6]//a")
  hrefs <- xml2::xml_attr(links, "href")
  hrefs <- hrefs[!is.na(hrefs) & hrefs != ""]
  if (length(hrefs) == 0) {
    return(list())
  }
  purrr::map_chr(hrefs, ~ bunddev_handelsregister_url_absolute(.x, "https://www.handelsregister.de"))
}

bunddev_handelsregister_url_absolute <- function(path, base_url) {
  if (is.na(path) || path == "") {
    return(base_url)
  }
  if (stringr::str_starts(path, "http")) {
    return(path)
  }
  if (stringr::str_starts(path, "//")) {
    return(paste0("https:", path))
  }
  base_url <- stringr::str_remove(base_url, "/$")
  if (stringr::str_starts(path, "/")) {
    return(paste0(base_url, path))
  }
  paste0(base_url, "/", path)
}

bunddev_handelsregister_value <- function(values, index) {
  if (length(values) >= index) {
    return(values[[index]])
  }
  NA_character_
}

bunddev_handelsregister_extract_register <- function(court) {
  if (is.na(court)) {
    return(NA_character_)
  }
  match <- stringr::str_match(court, "(HRA|HRB|GnR|VR|PR)\\s*\\d+(\\s+[A-Z])?(?!\\w)")
  register_num <- match[, 1]
  if (is.na(register_num)) {
    return(NA_character_)
  }

  if (stringr::str_detect(court, "Berlin") && stringr::str_starts(register_num, "HRB")) {
    if (!stringr::str_ends(register_num, " B")) {
      register_num <- paste0(register_num, " B")
    }
  }

  if (stringr::str_detect(court, "Bremen") && stringr::str_detect(register_num, "^(HRA|HRB|GnR|VR|PR)")) {
    if (!stringr::str_ends(register_num, " HB")) {
      register_num <- paste0(register_num, " HB")
    }
  }

  register_num
}

bunddev_handelsregister_history <- function(values) {
  if (length(values) < 8) {
    return(list())
  }

  history <- list()
  for (i in seq(9, length(values), by = 3)) {
    if (i + 1 > length(values)) {
      break
    }
    if (stringr::str_detect(values[[i]], "Branches") || stringr::str_detect(values[[i]], "Niederlassungen")) {
      break
    }
    history[[length(history) + 1]] <- list(
      name = values[[i]],
      location = values[[i + 1]]
    )
  }

  history
}
