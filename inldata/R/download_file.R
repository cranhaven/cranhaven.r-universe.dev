#' Download File from the Internet
#'
#' @description Download a file from the Internet and save to a cache directory.
#'
#' @param url 'character' string.
#'   URL of a resource to be downloaded.
#' @param cachedir 'character' string.
#'   Path to the cache directory, see [`get_cache_dir`] function for the default value.
#'   The file will not be downloaded if the file exists in the cache directory.
#' @param ...
#'   Additioanl arguments to be passed to the [`download.file`][utils::download.file] function.
#'   The destination of the downloaded file may not be specified.
#' @param quiet 'logical' flag.
#'   Whether to supress status messages (if any), and the progress bar.
#' @param mode 'character' string.
#'   Mode with which to write the file. Useful values are "w", "wb" (binary), "a" (append) and "ab".
#'
#' @return Returns the path to the downloaded file, or extracted files when decompression occurs.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal

download_file <- function(url,
                          cachedir = get_cache_dir(),
                          ...,
                          quiet = FALSE,
                          mode = "wb") {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_flag(quiet)
  checkmate::assert_string(mode)

  # download file
  file <- file.path(cachedir, basename(url))
  is <- checkmate::test_file_exists(file, access = "r")
  if (!is) {
    url <- assert_url(url)
    if (is.null(url)) {
      message("Download failed.")
      return(invisible(NULL))
    }
    utils::download.file(
      url = url,
      destfile = file,
      ...,
      quiet = quiet,
      mode = mode
    )
  }

  file
}
