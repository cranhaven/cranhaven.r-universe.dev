#' Manage Cache Directory
#'
#' @description Find, create, or clear the cache directory.
#'   Defaults to the temporary directory if the \pkg{rappdirs} package is unavailable.
#'   You can specify the path to the cache directory by setting an environment variable named "CACHE_DIR".
#'
#' @param name 'character' string.
#'   Name of cache directory.
#'
#' @return Path to the cache directory.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' dir <- get_cache_dir("test")
#'
#' clear_cache_dir("test")

get_cache_dir <- function(name = "inldata") {

  # check arguments
  checkmate::assert_string(name)

  # set directory
  dir <- Sys.getenv("CACHE_DIR")
  if (identical(dir, "")) {
    is <- requireNamespace("rappdirs", quietly = TRUE)
    if (is) {
      dir <- rappdirs::user_cache_dir(name)
    } else {
      dir <- file.path(tempdir(), "cache", name)
    }
  }

  # format path
  dir <- path.expand(dir) |>
    normalizePath(winslash = "/", mustWork = FALSE)

  # create directory
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # check directory
  checkmate::assert_directory_exists(dir, access = "rw")

  dir
}

#' @rdname get_cache_dir
#' @export
#' @keywords internal

clear_cache_dir <- function(name = "inldata") {
  checkmate::assert_string(name)
  dir <- get_cache_dir(name)
  is <- checkmate::test_directory(dir, access = "w")
  if (is) {
    unlink(dir, recursive = TRUE)
  }
  invisible(NULL)
}
