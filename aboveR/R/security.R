# security — aboveR
# Input sanitization for file paths, URLs, and download safety

#' Sanitize a Filename Derived from a URL
#'
#' Strips path separators, null bytes, and special characters from a
#' URL-derived filename to prevent path traversal attacks.
#'
#' @param url Character. A URL from which to derive a safe filename.
#' @returns Character. A sanitized filename safe for local file creation.
#' @noRd
sanitize_filename <- function(url) {
  # Extract the basename from the URL path
  parsed <- basename(sub("\\?.*$", "", url))

  # Remove null bytes (use regex character class to avoid zero-length pattern)
  parsed <- gsub("[\x01-\x1f]", "", parsed)

  # Remove path separators and dangerous characters
  parsed <- gsub("[/\\\\:*?\"<>|]", "_", parsed)

  # Remove leading dots (hidden files / directory traversal)
  parsed <- sub("^[\\.]+", "", parsed)

  # Collapse multiple underscores
  parsed <- gsub("_+", "_", parsed)

  # Ensure non-empty
  if (!nzchar(parsed)) {
    parsed <- paste0("tile_", substr(digest_simple(url), 1, 8))
  }

  parsed
}

#' Validate That a URL Points to the KyFromAbove S3 Bucket
#'
#' Ensures that a URL originates from the expected KyFromAbove S3
#' endpoint. Prevents SSRF by rejecting URLs pointing to other hosts.
#'
#' @param url Character. URL to validate.
#' @returns `TRUE` invisibly if valid; throws an error otherwise.
#' @noRd
validate_kfa_url <- function(url) {
  allowed_patterns <- c(
    "^https://kyfromabove\\.s3\\.us-west-2\\.amazonaws\\.com/",
    "^https://kyfromabove\\.s3\\.amazonaws\\.com/",
    "^https://s3\\.us-west-2\\.amazonaws\\.com/kyfromabove/"
  )
  if (!any(vapply(allowed_patterns, grepl, logical(1), x = url))) {
    stop(
      "URL does not point to the KyFromAbove S3 bucket:\n  ", url, "\n",
      "Only URLs from kyfromabove.s3.us-west-2.amazonaws.com are allowed.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate a Vector of URLs Against the KyFromAbove Bucket
#'
#' @param urls Character vector of URLs.
#' @returns `TRUE` invisibly.
#' @noRd
validate_kfa_urls <- function(urls) {
  for (u in urls) validate_kfa_url(u)
  invisible(TRUE)
}

#' Simple String Hash (No External Dependency)
#'
#' @param x Character string.
#' @returns 8-character hex hash.
#' @noRd
digest_simple <- function(x) {
  # Use R's built-in hashing via rlang if available, else fallback
  sprintf("%08x", abs(sum(utf8ToInt(x) * seq_along(utf8ToInt(x)))))
}

#' Download a File with Timeout and Size Limits
#'
#' Wraps [utils::download.file()] with timeout protection, size
#' validation, and retry logic.
#'
#' @param url Character. URL to download.
#' @param destfile Character. Local destination path.
#' @param max_size_mb Numeric. Maximum allowed file size in MB.
#'   Default 500.
#' @param timeout_sec Integer. Download timeout in seconds.
#'   Default 300.
#' @param retries Integer. Number of retry attempts. Default 3.
#' @returns The destination file path (invisibly).
#' @noRd
safe_download <- function(url, destfile, max_size_mb = 500,
                          timeout_sec = 300L, retries = 3L) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout_sec)

  for (attempt in seq_len(retries)) {
    result <- tryCatch(
      {
        utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
        0L
      },
      error = function(e) {
        if (attempt < retries) {
          Sys.sleep(min(2^attempt, 30))
        }
        -1L
      }
    )

    if (result == 0L && file.exists(destfile)) {
      # Check file size
      size_mb <- file.size(destfile) / (1024 * 1024)
      if (size_mb > max_size_mb) {
        unlink(destfile)
        stop(
          "Downloaded file exceeds size limit (",
          round(size_mb, 1), " MB > ", max_size_mb, " MB): ",
          basename(destfile),
          call. = FALSE
        )
      }
      return(invisible(destfile))
    }
  }

  stop(
    "Failed to download after ", retries, " attempts:\n  ", url,
    call. = FALSE
  )
}
