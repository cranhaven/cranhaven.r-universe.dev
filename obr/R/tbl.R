# obr_tbl: a data.frame with attached OBR provenance.
# All data-returning user functions wrap their output in this class so that
# users (and OBR researchers in particular) can audit which OBR publication
# produced which numbers, when they were retrieved, and from what URL.

# Internal constructor. Not exported.
new_obr_tbl <- function(data,
                        publication,
                        source_url,
                        retrieved   = Sys.time(),
                        vintage     = NA_character_,
                        file_md5    = NA_character_,
                        notes       = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a {.cls data.frame}, not a {.cls {class(data)[1]}}.")
  }
  obj <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(obj) <- NULL

  pkg_version <- tryCatch(
    as.character(utils::packageVersion("obr")),
    error = function(e) NA_character_
  )

  attr(obj, "obr_provenance") <- list(
    publication     = publication,
    vintage         = vintage,
    source_url      = source_url,
    retrieved       = retrieved,
    file_md5        = file_md5,
    package_version = pkg_version,
    notes           = notes
  )
  class(obj) <- c("obr_tbl", "data.frame")
  obj
}

# Map a publication code to a human-readable label for printing.
obr_publication_label <- function(code) {
  switch(code %||% "",
    PFD = "Public Finances Databank",
    HFD = "Historical Official Forecasts Database",
    EFO = "Economic and Fiscal Outlook",
    "EFO-MP" = "EFO Monthly Profiles",
    WTR = "Welfare Trends Report",
    FSR = "Fiscal Risks and Sustainability Report",
    PMD = "Policy Measures Database",
    FRD = "Forecast Revisions Database",
    code
  )
}

# Extract a "Month YYYY" vintage label from an OBR download slug, if present.
# OBR slugs follow patterns like "march-2026-economic-and-fiscal-outlook-..."
obr_url_vintage <- function(url) {
  if (is.null(url) || is.na(url)) return(NA_character_)
  pattern <- "(january|february|march|april|may|june|july|august|september|october|november|december)-([0-9]{4})"
  m <- regmatches(url, regexpr(pattern, url, ignore.case = TRUE))
  if (length(m) == 0L) return(NA_character_)
  parts <- strsplit(m[1], "-", fixed = TRUE)[[1]]
  month <- paste0(toupper(substring(parts[1], 1, 1)),
                  tolower(substring(parts[1], 2)))
  paste(month, parts[2])
}

# Null-coalesce
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

#' Print an obr_tbl
#'
#' Prints the data with a provenance header that names the OBR publication,
#' vintage, source URL, retrieval time, and underlying file fingerprint.
#'
#' @param x An `obr_tbl` object.
#' @param n Number of rows to display (default 10).
#' @param ... Further arguments passed to [print.data.frame()].
#'
#' @return The input, returned invisibly.
#' @export
print.obr_tbl <- function(x, n = 10L, ...) {
  prov <- attr(x, "obr_provenance")
  cat(sprintf("# obr_tbl: %d rows x %d cols\n", nrow(x), ncol(x)))
  if (!is.null(prov)) {
    pub_label <- obr_publication_label(prov$publication)
    vintage   <- prov$vintage %||% NA_character_
    head_line <- if (!is.na(vintage)) {
      sprintf("# Source:       OBR %s, %s", pub_label, vintage)
    } else {
      sprintf("# Source:       OBR %s", pub_label)
    }
    cat(head_line, "\n", sep = "")
    if (!is.null(prov$source_url) && !is.na(prov$source_url)) {
      cat(sprintf("# URL:          %s\n", prov$source_url))
    }
    if (!is.null(prov$retrieved) && !is.na(prov$retrieved)) {
      cat(sprintf("# Retrieved:    %s\n",
                  format(prov$retrieved, "%Y-%m-%d %H:%M:%S %Z")))
    }
    if (!is.null(prov$file_md5) && !is.na(prov$file_md5) && nzchar(prov$file_md5)) {
      cat(sprintf("# File MD5:     %s\n", substr(prov$file_md5, 1L, 12L)))
    }
    if (!is.null(prov$package_version) && !is.na(prov$package_version)) {
      cat(sprintf("# Package:      obr %s\n", prov$package_version))
    }
    if (!is.null(prov$notes) && nzchar(prov$notes)) {
      cat(sprintf("# Note:         %s\n", prov$notes))
    }
  }
  cat("\n")
  df <- as.data.frame(x)
  if (nrow(df) > n) {
    print(utils::head(df, n = n), ...)
    cat(sprintf("# ... with %d more row%s\n", nrow(df) - n,
                if (nrow(df) - n == 1L) "" else "s"))
  } else {
    print(df, ...)
  }
  invisible(x)
}

#' Summary of an obr_tbl
#'
#' Returns the full provenance card and a structural summary of the data.
#'
#' @param object An `obr_tbl` object.
#' @param ... Unused.
#'
#' @return Invisibly returns the provenance list.
#' @export
summary.obr_tbl <- function(object, ...) {
  prov <- attr(object, "obr_provenance")
  cat("obr_tbl provenance\n")
  cat("------------------\n")
  if (!is.null(prov)) {
    fields <- c(
      "publication"     = "Publication",
      "vintage"         = "Vintage",
      "source_url"      = "Source URL",
      "retrieved"       = "Retrieved",
      "file_md5"        = "File MD5",
      "package_version" = "Package",
      "notes"           = "Notes"
    )
    for (key in names(fields)) {
      val <- prov[[key]]
      if (is.null(val) || (length(val) == 1L && is.na(val))) next
      if (!nzchar(as.character(val))) next
      label <- fields[[key]]
      if (key == "publication") {
        val <- sprintf("%s (%s)", val, obr_publication_label(val))
      }
      if (inherits(val, "POSIXt")) {
        val <- format(val, "%Y-%m-%d %H:%M:%S %Z")
      }
      cat(sprintf("  %-13s %s\n", paste0(label, ":"), val))
    }
  } else {
    cat("  (no provenance attached)\n")
  }
  cat("\nData\n")
  cat("----\n")
  cat(sprintf("  Rows:    %d\n", nrow(object)))
  cat(sprintf("  Columns: %d (%s)\n", ncol(object),
              paste(names(object), collapse = ", ")))
  invisible(prov)
}

#' Subset an obr_tbl, preserving provenance
#'
#' Standard data.frame subsetting drops the `obr_tbl` class. This method
#' preserves it (and the provenance attributes) when the result is still a
#' data.frame.
#'
#' @param x An `obr_tbl`.
#' @param ... Subsetting arguments passed to [`[.data.frame`].
#'
#' @return An `obr_tbl` if the subset is two-dimensional, otherwise the
#'   underlying vector.
#' @export
`[.obr_tbl` <- function(x, ...) {
  prov <- attr(x, "obr_provenance")
  out <- NextMethod()
  if (is.data.frame(out)) {
    attr(out, "obr_provenance") <- prov
    class(out) <- c("obr_tbl", "data.frame")
  }
  out
}

#' Coerce an obr_tbl to a plain data.frame
#'
#' Strips the provenance attributes and the `obr_tbl` class so the result
#' interacts smoothly with packages that dispatch on `data.frame` directly.
#'
#' @param x An `obr_tbl`.
#' @param ... Unused.
#'
#' @return A plain `data.frame` with no provenance.
#' @export
as.data.frame.obr_tbl <- function(x, ...) {
  attr(x, "obr_provenance") <- NULL
  class(x) <- "data.frame"
  x
}

#' Extract OBR provenance metadata
#'
#' Returns the provenance list attached to an `obr_tbl`: the OBR publication
#' it came from, the publication vintage, the source URL, retrieval time,
#' file fingerprint, and package version.
#'
#' @param x An `obr_tbl` (or any object; returns `NULL` if none attached).
#'
#' @return A named list of provenance fields, or `NULL` if no provenance is
#'   attached. Fields:
#' \describe{
#'   \item{publication}{Short code: `"PFD"`, `"HFD"`, `"EFO"`, `"WTR"`, `"FSR"`, `"PMD"`.}
#'   \item{vintage}{Publication vintage label, e.g. `"March 2026"`.}
#'   \item{source_url}{Canonical OBR download URL the data came from.}
#'   \item{retrieved}{`POSIXct` timestamp of when the file was downloaded or
#'     last validated in the cache.}
#'   \item{file_md5}{MD5 fingerprint of the underlying spreadsheet.}
#'   \item{package_version}{`obr` package version that produced the object.}
#'   \item{notes}{Optional free-text notes, or `NULL`.}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' psnb <- get_psnb()
#' obr_provenance(psnb)
#' options(op)
#' }
#'
#' @family provenance
#' @export
obr_provenance <- function(x) {
  attr(x, "obr_provenance")
}
