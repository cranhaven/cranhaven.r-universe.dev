# Cache directory (platform-aware, base R)
obr_cache_dir <- function() {
  d <- getOption("obr.cache_dir", default = tools::R_user_dir("obr", "cache"))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# Download a file and cache it; return local path
obr_fetch <- function(url, filename, refresh = FALSE) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    cli::cli_abort("{.arg refresh} must be a single {.cls logical} value.")
  }

  path <- file.path(obr_cache_dir(), filename)

  if (file.exists(path) && !refresh) {
    cli::cli_inform(c("i" = "Loading from cache. Use {.code refresh = TRUE} to re-download."))
    return(path)
  }

  cli::cli_inform(c("i" = "Downloading {.file {filename}} from OBR..."))

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("obr R package (https://github.com/charlescoverdale/obr)") |>
      httr2::req_throttle(rate = 5 / 10) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)
      ) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_abort(
        c("Failed to download {.url {url}}.",
          "x" = conditionMessage(e)),
        call = NULL
      )
    }
  )

  writeBin(httr2::resp_body_raw(resp), path)
  cli::cli_inform(c("v" = "Saved to cache."))
  path
}

#' Clear cached OBR files
#'
#' Deletes all files downloaded and cached by the obr package. The next
#' function call will re-download fresh data from the OBR website.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' clear_cache()
#' options(op)
#' }
#'
#' @family data access
#' @export
clear_cache <- function() {
  files <- list.files(obr_cache_dir(), full.names = TRUE)
  n <- length(files)
  if (n > 0) file.remove(files)
  cli::cli_inform("Removed {n} cached file{?s}.")
  invisible(NULL)
}
