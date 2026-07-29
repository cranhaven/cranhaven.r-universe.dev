# R/nhanes_http.R
# URL construction and HTTP download helpers for nhanesR

# -- URL construction -----------------------------------------------------------

#' Build the CDC URL for an NHANES XPT data file
#'
#' @param file_code Character. The NHANES file code without suffix or extension
#'   (e.g. "DEMO", "BPX", "TRIGLY").
#' @param cycle Character. A cycle string from [nhanes_cycles()]
#'   (e.g. "2015-2016").
#' @param suffix Character. Cycle letter suffix (e.g. "_I"). If `NULL`,
#'   looked up automatically from the internal cycle registry.
#' @return A character URL.
#' @keywords internal
.nhanes_xpt_url <- function(file_code, cycle, suffix = NULL) {
  if (is.null(suffix)) {
    suffix <- .nhanes_cycle_field(cycle, "suffix")
  }
  begin_year <- .nhanes_cycle_field(cycle, "begin_year")
  sprintf(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%s/DataFiles/%s%s.xpt",
    begin_year, toupper(file_code), suffix
  )
}

#' Build the CDC URL for an NHANES component documentation page
#' @keywords internal
.nhanes_doc_url <- function(file_code, cycle, suffix = NULL) {
  if (is.null(suffix)) {
    suffix <- .nhanes_cycle_field(cycle, "suffix")
  }
  begin_year <- .nhanes_cycle_field(cycle, "begin_year")
  sprintf(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%s/DataFiles/%s%s.htm",
    begin_year, toupper(file_code), suffix
  )
}

#' Build the CDC FTP URL for a public-use LMF .dat file
#' @keywords internal
.nhanes_lmf_url <- function(cycle) {
  reg <- .lmf_registry[.lmf_registry$cycle == cycle, ]
  if (nrow(reg) == 0L) {
    cli::cli_abort(
      "No public-use LMF found for cycle {.val {cycle}}. \\
       Available cycles: {.val {.lmf_registry$cycle}}"
    )
  }
  paste0(reg$ftp_base, reg$filename)
}

# -- Cycle registry helpers -----------------------------------------------------

#' Extract a single field from the internal cycle registry
#' @keywords internal
.nhanes_cycle_field <- function(cycle, field) {
  row <- .nhanes_cycles[.nhanes_cycles$cycle == cycle, ]
  if (nrow(row) == 0L) {
    if (cycle == "1988-1994") {
      row <- .nhanes_iii
    } else {
      cli::cli_abort(
        "Cycle {.val {cycle}} not found in registry. \\
         Use {.fn nhanes_cycles} to see available cycles."
      )
    }
  }
  row[[field]]
}

# -- HTTP download --------------------------------------------------------------

#' Download a file from a URL with retry logic and progress reporting
#'
#' @param url Character URL to download.
#' @param dest_path Destination file path.
#' @param desc Short description for the progress message.
#' @keywords internal
.nhanes_download_file <- function(url, dest_path, desc = NULL) {
  desc    <- desc %||% basename(url)
  timeout <- getOption("nhanesR.timeout", 120L)
  verbose <- getOption("nhanesR.verbose", TRUE)

  if (verbose) cli::cli_progress_step("Downloading {desc}")

  req <- httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3L, backoff = ~ 2 ^ .x) |>
    httr2::req_error(is_error = \(resp) FALSE)  # handle status manually below

  resp <- httr2::req_perform(req, path = dest_path)

  status <- httr2::resp_status(resp)
  if (status != 200L) {
    if (file.exists(dest_path)) file.remove(dest_path)
    cli::cli_abort("HTTP {status} downloading {.url {url}}")
  }

  ct <- httr2::resp_content_type(resp)
  if (grepl("text/html", ct, fixed = TRUE)) {
    if (file.exists(dest_path)) file.remove(dest_path)
    cli::cli_abort(
      "CDC returned an HTML page instead of a data file for {.url {url}}.\\n
       The file may have been moved or renamed. \\
       Check {.fn nhanes_manifest} for the current URL."
    )
  }

  if (verbose) cli::cli_progress_done()
  invisible(dest_path)
}

# -- Misc helpers ---------------------------------------------------------------

# Backport of base R's %||% for R < 4.4
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Check that a suggested package is installed
#' @keywords internal
.nhanes_check_pkg <- function(pkg, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg {pkg}} is required. Install with: \\
       {.code install.packages('{pkg}')}",
      call = call
    )
  }
}
