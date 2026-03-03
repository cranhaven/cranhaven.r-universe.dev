#' Locate the bunddev cache directory
#'
#' @details
#' The cache directory is used to store downloaded OpenAPI specs and cached
#' API responses. Use this to inspect or clean cached files.
#'
#' @seealso
#' [bunddev_spec()] to download specs, and [bunddev_spec_path()] to locate
#' a specific spec file.
#'
#' @examples
#' \dontrun{
#' bunddev_cache_dir()
#' }
#'
#' @return Cache directory path.
#' @export
bunddev_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("bunddev", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

#' Build a cache path for an API spec
#'
#' @param id Registry id.
#'
#' @details
#' Determines the cache file name based on the spec URL extension (.yaml/.json).
#'
#' @seealso
#' [bunddev_spec()] to download and parse specs.
#'
#' @examples
#' \dontrun{
#' bunddev_spec_path("smard")
#' }
#'
#' @return File path for the cached spec.
#' @export
bunddev_spec_path <- function(id) {
  info <- bunddev_info(id)
  spec_url <- info$spec_url[[1]]
  url_path <- httr2::url_parse(spec_url)$path
  ext <- tools::file_ext(url_path)
  if (identical(ext, "")) {
    ext <- "json"
  }
  file.path(bunddev_cache_dir(), paste0(id, ".", ext))
}

#' Retrieve a cached API spec
#'
#' @param id Registry id.
#' @param refresh Logical; refresh cached spec.
#'
#' @details
#' Downloads the OpenAPI spec from the registry if it is missing or when
#' `refresh = TRUE`. Parsed specs are returned as lists.
#'
#' @seealso
#' [bunddev_endpoints()] to list operations and [bunddev_parameters()] to
#' inspect parameters.
#'
#' @examples
#' \dontrun{
#' bunddev_spec("smard")
#' }
#'
#' @return Parsed OpenAPI spec.
#' @export
bunddev_spec <- function(id, refresh = FALSE) {
  info <- bunddev_info(id)
  spec_url <- info$spec_url[[1]]
  spec_path <- bunddev_spec_path(id)

  if (refresh || !file.exists(spec_path)) {
    request <- httr2::request(spec_url)
    response <- httr2::req_perform(request)
    raw_body <- httr2::resp_body_raw(response)
    writeBin(raw_body, spec_path)
  }

  ext <- tools::file_ext(spec_path)
  if (ext %in% c("yaml", "yml")) {
    spec <- tryCatch(
      suppressWarnings(yaml::read_yaml(spec_path)),
      error = function(e) {
        cli::cli_abort(c(
          "Failed to parse OpenAPI spec for {.val {id}}.",
          "i" = "The upstream YAML at {.url {spec_url}} may be malformed.",
          "i" = "Cached file: {.file {spec_path}}",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(spec)
  }
  if (ext == "json") {
    return(jsonlite::fromJSON(spec_path, simplifyVector = FALSE))
  }

  cli::cli_abort("Unsupported spec file extension '{ext}'.")
}

bunddev_response_cache_dir <- function() {
  cache_dir <- file.path(tools::R_user_dir("bunddev", "cache"), "responses")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

bunddev_response_cache_key <- function(api, operation_id, params,
                                       path = NULL) {
  if (length(params) > 0) {
    params <- params[order(names(params))]
  }
  payload <- jsonlite::toJSON(
    list(api = api, operation_id = operation_id, path = path, params = params),
    auto_unbox = TRUE,
    null = "null"
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(payload, tmp)
  tools::md5sum(tmp)
}

bunddev_response_cache_path <- function(api, operation_id, params,
                                        path = NULL) {
  key <- bunddev_response_cache_key(api, operation_id, params, path = path)
  file.path(bunddev_response_cache_dir(), paste0(key, ".bin"))
}
