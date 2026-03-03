#' Read the bundled API registry
#'
#' @details
#' The registry is bundled with the package and contains metadata such as the
#' API title, provider, documentation URL, OpenAPI spec URL, authentication
#' type, and any declared rate limits. The data originates from bund.dev and
#' the bundesAPI registry.
#'
#' @seealso
#' [bunddev_list()] for filtered listings, [bunddev_search()] for keyword
#' searches, and [bunddev_info()] for a single entry.
#'
#' @examples
#' registry <- bunddev_registry()
#' head(registry, 3)
#' 
#' @return A tibble with registry entries.
#' @export
bunddev_registry <- function() {
  registry_path <- system.file("registry", "registry.yml", package = "bunddev")
  if (registry_path == "") {
    cli::cli_abort("Registry file not found.")
  }

  registry <- yaml::read_yaml(registry_path)
  if (is.null(registry) || !is.list(registry)) {
    cli::cli_abort("Registry file is empty or invalid.")
  }

  required_fields <- c("id", "title", "provider", "spec_url", "docs_url", "auth", "tags")
  missing_fields <- purrr::map_lgl(registry, ~ !all(required_fields %in% names(.x)))
  if (any(missing_fields)) {
    cli::cli_abort("Registry entries must include {required_fields}.")
  }

  ids <- purrr::map_chr(registry, "id")
  if (anyDuplicated(ids)) {
    cli::cli_abort("Registry ids must be unique.")
  }

  auth_values <- c("none", "api_key", "oauth2")
  invalid_auth <- purrr::map_lgl(registry, ~ !.x$auth %in% auth_values)
  if (any(invalid_auth)) {
    cli::cli_abort("Registry auth must be one of: {auth_values}.")
  }

  null_chr <- function(value) {
    if (is.null(value) || identical(value, "")) NA_character_ else as.character(value)
  }

  registry_tbl <- tibble::tibble(
    id = ids,
    title = purrr::map_chr(registry, ~ null_chr(.x$title)),
    provider = purrr::map_chr(registry, ~ null_chr(.x$provider)),
    spec_url = purrr::map_chr(registry, ~ null_chr(.x$spec_url)),
    docs_url = purrr::map_chr(registry, ~ null_chr(.x$docs_url)),
    auth = purrr::map_chr(registry, ~ null_chr(.x$auth)),
    rate_limit = purrr::map_chr(registry, ~ null_chr(.x$rate_limit)),
    tags = purrr::map(registry, "tags")
  )

  dplyr::as_tibble(registry_tbl)
}

#' List registry entries
#'
#' @param tag Optional tag to filter on.
#' @param auth Optional auth type to filter on.
#'
#' @details
#' Use this to quickly narrow down APIs by topic or authentication type. Tags
#' correspond to the taxonomy in the bundled registry.
#'
#' @seealso
#' [bunddev_registry()] for the full table and [bunddev_info()] for one entry.
#'
#' @examples
#' bunddev_list(tag = "jobs")
#' bunddev_list(auth = "api_key")
#'
#' @return A tibble of registry entries.
#' @export
bunddev_list <- function(tag = NULL, auth = NULL) {
  registry <- bunddev_registry()

  if (!is.null(tag)) {
    registry <- registry[purrr::map_lgl(registry$tags, ~ tag %in% .x), , drop = FALSE]
  }

  if (!is.null(auth)) {
    registry <- registry[registry$auth == auth, , drop = FALSE]
  }

  registry
}

#' Search registry entries
#'
#' @param q Search query.
#'
#' @details
#' Searches across registry ids, titles, providers, and tags using a simple
#' substring match.
#'
#' @seealso
#' [bunddev_list()] to filter by tag or auth, and [bunddev_info()] for details
#' on a single API.
#'
#' @examples
#' bunddev_search("weather")
#'
#' @return A tibble of matching registry entries.
#' @export
bunddev_search <- function(q) {
  registry <- bunddev_registry()
  query <- stringr::str_to_lower(q)

  tag_text <- purrr::map_chr(registry$tags, ~ paste(.x, collapse = " "))
  search_text <- stringr::str_to_lower(
    paste(registry$id, registry$title, registry$provider, tag_text)
  )

  matches <- stringr::str_detect(search_text, query)
  registry[matches, , drop = FALSE]
}

#' Get a registry entry by id
#'
#' @param id Registry id.
#'
#' @details
#' Use this to access the spec URL, documentation URL, and authentication
#' requirements for a single API.
#'
#' @seealso
#' [bunddev_list()] for discovery and [bunddev_registry()] for the full table.
#'
#' @examples
#' bunddev_info("smard")
#'
#' @return A tibble with a single registry entry.
#' @export
bunddev_info <- function(id) {
  registry <- bunddev_registry()
  match <- registry[registry$id == id, , drop = FALSE]

  if (nrow(match) == 0) {
    cli::cli_abort("No registry entry found for id '{id}'.")
  }

  match
}
