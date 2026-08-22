#' Kew Tree of Life base URL
#'
#' @return A character scalar with the default public Kew data URL.
#' @export
tol_base_url <- function() {
  getOption(
    "rtreeoflife.base_url",
    "https://sftp.kew.org/pub/treeoflife"
  )
}

#' Set the Kew Tree of Life base URL for the current R session
#'
#' @param base_url Base URL without the release directory.
#'
#' @return The previous value of option `rtreeoflife.base_url`, invisibly.
#' @export
tol_set_base_url <- function(base_url = "https://sftp.kew.org/pub/treeoflife") {
  if (length(base_url) != 1L || !nzchar(base_url)) {
    stop("`base_url` must be a non-empty character scalar.", call. = FALSE)
  }

  old <- getOption("rtreeoflife.base_url")
  options(rtreeoflife.base_url = sub("/+$", "", base_url))
  invisible(old)
}

#' Local data directory for downloaded files
#'
#' The default can be overridden with option `rtreeoflife.data_dir` or
#' environment variable `RTREEOFLIFE_DATA_DIR`.
#'
#' @param create Create the directory if it does not exist.
#'
#' @return A normalized path.
#' @export
tol_data_dir <- function(create = TRUE) {
  dir <- getOption("rtreeoflife.data_dir", Sys.getenv("RTREEOFLIFE_DATA_DIR"))

  if (!nzchar(dir)) {
    dir <- file.path(tools::R_user_dir("rtreeoflife", which = "data"), "kew")
  }

  if (isTRUE(create) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

normalize_release <- function(release) {
  if (length(release) != 1L || !nzchar(release)) {
    stop("`release` must be a non-empty character scalar.", call. = FALSE)
  }

  if (identical(release, "current")) {
    release <- "current_release"
  }

  release
}

normalize_remote_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  path <- gsub("/+", "/", path)
  path <- sub("^/+", "", path)
  path
}

url_join <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  parts <- gsub("^/+|/+$", "", parts)
  paste(parts, collapse = "/")
}
