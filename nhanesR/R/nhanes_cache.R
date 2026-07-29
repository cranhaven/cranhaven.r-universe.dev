# R/nhanes_cache.R
# Local cache management for nhanesR
# Handles directory structure, file hashing, and RDS cache validation

# -- Cache directory ------------------------------------------------------------

#' Get or set the nhanesR local cache directory
#'
#' nhanesR stores downloaded and parsed NHANES files in a local cache to avoid
#' redundant downloads. By default the cache is placed in the standard user
#' data directory for your operating system (see below). Use this function to
#' view or change the location for the current session, or set it permanently
#' in your `.Rprofile`.
#'
#' ## Package options
#'
#' Three options control nhanesR behavior. Set any of them in your `.Rprofile`
#' to make the change permanent across sessions; changes made during a session
#' (via `nhanes_cache_dir()` or `options()` directly) last only until the
#' session ends.
#'
#' | Option | Default | Purpose |
#' |--------|---------|---------|
#' | `nhanesR.cache_dir` | `file.path(tempdir(), "nhanesR")` | Root directory for all cached files |
#' | `nhanesR.verbose` | `TRUE` | Print progress messages during downloads |
#' | `nhanesR.timeout` | `120L` | HTTP request timeout in seconds |
#'
#' ### Default cache location
#'
#' By default the cache lives inside R's session-temporary directory
#' (`tempdir()`), so nhanesR never writes to your home directory without
#' your consent. Downloaded files are re-fetched in each new R session. To
#' keep a persistent cache, set `nhanesR.cache_dir` in your `~/.Rprofile`.
#'
#' ### Setting options permanently
#'
#' Add lines like these to your `~/.Rprofile`:
#'
#' ```r
#' options(
#'   nhanesR.cache_dir = "/data/nhanes_cache",  # shared lab server path
#'   nhanesR.verbose   = FALSE,                  # suppress progress messages
#'   nhanesR.timeout   = 300L                    # 5-minute timeout for slow connections
#' )
#' ```
#'
#' Options set in `.Rprofile` take precedence over package defaults: nhanesR
#' only sets an option at load time if it is not already defined.
#'
#' @param path Optional character. New path to use as the cache directory for
#'   the current session. If `NULL`, returns the current setting without
#'   changing it.
#' @param create Logical. If `TRUE` (default), create the directory if it
#'   does not exist.
#'
#' @return The current (or newly set) cache directory path, invisibly.
#' @seealso [nhanes_download()] and [nhanes_download_analyte()], whose caching
#'   behavior is controlled by the options described above.
#' @export
#' @examples
#' # View current cache location (defaults to a subdirectory of tempdir())
#' nhanes_cache_dir()
#'
#' # Change to a persistent location for this session only
#' \donttest{
#' nhanes_cache_dir("~/my_nhanes_cache")
#' }
#'
#' # Suppress download messages for this session
#' options(nhanesR.verbose = FALSE)
#'
#' # View all current nhanesR option values
#' Filter(function(x) startsWith(x, "nhanesR."), names(options()))
nhanes_cache_dir <- function(path = NULL, create = TRUE) {
  if (!is.null(path)) {
    path <- normalizePath(path, mustWork = FALSE)
    options(nhanesR.cache_dir = path)
  }
  dir <- getOption("nhanesR.cache_dir")
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(getOption("nhanesR.verbose"))) {
      cli::cli_inform("Created nhanesR cache directory: {.path {dir}}")
    }
  }
  dir
}

#' Return a cache subdirectory path, creating it if needed
#' @keywords internal
.nhanes_cache_subdir <- function(...) {
  path <- file.path(nhanes_cache_dir(), ...)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

# -- File hashing ---------------------------------------------------------------

#' Compute the MD5 hash of a file
#' @keywords internal
.nhanes_file_hash <- function(path) {
  tools::md5sum(path)[[1L]]
}

#' Write an MD5 hash sidecar file alongside a cached RDS
#'
#' The sidecar file has the same path as the RDS with `.md5` appended.
#' @keywords internal
.nhanes_write_hash <- function(rds_path) {
  hash_path <- paste0(rds_path, ".md5")
  writeLines(.nhanes_file_hash(rds_path), hash_path)
  invisible(hash_path)
}

# -- Cache validation -----------------------------------------------------------

#' Check whether a cached RDS file is present and hash-validated
#'
#' Returns `TRUE` if the RDS exists and its MD5 matches the sidecar.
#' Returns `FALSE` if either file is missing or the hash does not match,
#' indicating the cache should be regenerated.
#'
#' @keywords internal
.nhanes_cache_valid <- function(rds_path) {
  hash_path <- paste0(rds_path, ".md5")
  if (!file.exists(rds_path) || !file.exists(hash_path)) return(FALSE)
  recorded <- trimws(readLines(hash_path, warn = FALSE))
  current  <- .nhanes_file_hash(rds_path)
  identical(recorded, current)
}

#' Invalidate a cached RDS and its hash sidecar
#'
#' Removes both the RDS and its `.md5` sidecar if they exist.
#' Called when a source file is refreshed.
#' @keywords internal
.nhanes_cache_invalidate <- function(rds_path) {
  hash_path <- paste0(rds_path, ".md5")
  if (file.exists(rds_path))  file.remove(rds_path)
  if (file.exists(hash_path)) file.remove(hash_path)
  invisible(NULL)
}
