# OBR Forecast Revisions Database (FRD).
# A workbook decomposing each EFO-to-EFO change in the headline PSNB
# forecast into policy, classification, and underlying (economic
# determinants) components. Two parallel sheets: "Revisions (Per cent of
# GDP)" and "Revisions (£ billion)". Within each sheet, every forecast
# vintage occupies a block of rows: the vintage row carries the total
# revision, then sub-rows label Policy / Classifications and one-offs /
# Underlying, each with optional sub-components.

FRD_FALLBACK <- "https://obr.uk/download/forecast-revisions-database-march-2025/"
FRD_FILENAME <- "forecast_revisions.xlsx"

frd_url_candidates <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  months <- c("march", "november", "october", "july")
  candidates <- character(0)
  for (yr in seq(current_year, current_year - 3L)) {
    for (mn in months) {
      candidates <- c(candidates, paste0(
        "https://obr.uk/download/forecast-revisions-database-", mn, "-", yr, "/"
      ))
    }
  }
  candidates
}

frd_source <- function(refresh = FALSE) {
  obr_get_xlsx(
    candidates = frd_url_candidates(),
    fallback   = FRD_FALLBACK,
    filename   = FRD_FILENAME,
    refresh    = refresh,
    label      = "Forecast Revisions Database"
  )
}

frd_obr_tbl <- function(data, src, notes = NULL) {
  new_obr_tbl(
    data        = data,
    publication = "FRD",
    vintage     = obr_url_vintage(src$final_url %||% src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5,
    notes       = notes
  )
}

# Parse the "Revisions (Per cent of GDP)" or "Revisions (£ billion)" sheet
# into long format: one row per (forecast_date, component, fiscal_year).
# Top-level components are "Total", "Policy", "Classifications and one-offs",
# and "Underlying"; sub-components carry the OBR's labels (e.g.
# "of which: receipts").
parse_frd_revisions <- function(path, unit) {
  sheet_name <- switch(unit,
    pct_gdp = "Revisions (Per cent of GDP)",
    gbp_bn  = "Revisions (\u00a3 billion)",
    cli::cli_abort("Unknown FRD unit {.val {unit}}.")
  )
  raw <- readxl::read_excel(path, sheet = sheet_name,
                            col_names = FALSE, .name_repair = "minimal")

  yr_header <- as.character(unlist(raw[2, ]))
  year_cols <- which(grepl("^[0-9]{4}-[0-9]{2}$", yr_header))
  if (length(year_cols) == 0L) {
    cli::cli_abort(c(
      "Could not find fiscal-year headers in FRD sheet {.val {sheet_name}}.",
      "i" = "OBR may have changed the layout. Please file an issue."
    ))
  }
  fiscal_years <- yr_header[year_cols]

  col1 <- as.character(unlist(raw[, 1]))
  is_vintage   <- !is.na(col1) & grepl("^[A-Z][a-z]+ [0-9]{4}$", col1)
  vintage_rows <- which(is_vintage)
  if (length(vintage_rows) == 0L) {
    cli::cli_abort(c(
      "No forecast vintage rows found in FRD sheet {.val {sheet_name}}.",
      "i" = "Expected first-column entries like {.val March 2024}."
    ))
  }

  results <- list()
  for (v_idx in seq_along(vintage_rows)) {
    v_row         <- vintage_rows[v_idx]
    forecast_date <- col1[v_row]
    end_row <- if (v_idx < length(vintage_rows)) {
      vintage_rows[v_idx + 1L] - 1L
    } else {
      nrow(raw)
    }

    total_vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[v_row, year_cols])))
    )
    if (any(!is.na(total_vals))) {
      results[[length(results) + 1L]] <- data.frame(
        forecast_date = forecast_date,
        component     = "Total",
        fiscal_year   = fiscal_years,
        value         = total_vals,
        stringsAsFactors = FALSE
      )
    }

    if (v_row < end_row) {
      for (i in (v_row + 1L):end_row) {
        if (i > nrow(raw)) break
        label <- col1[i]
        if (is.na(label) || !nzchar(label)) next
        if (label == "of which:") next
        # Skip if this is the start of the next vintage (defensive: covered
        # by the end_row calculation, but cheap insurance).
        if (grepl("^[A-Z][a-z]+ [0-9]{4}$", label)) next
        vals <- suppressWarnings(
          as.numeric(as.character(unlist(raw[i, year_cols])))
        )
        if (all(is.na(vals))) next
        results[[length(results) + 1L]] <- data.frame(
          forecast_date = forecast_date,
          component     = label,
          fiscal_year   = fiscal_years,
          value         = vals,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  result <- do.call(rbind, results)
  result <- result[!is.na(result$value), ]
  rownames(result) <- NULL
  result
}

#' Get OBR forecast revisions
#'
#' Downloads (and caches) the OBR Forecast Revisions Database, which
#' decomposes each EFO-to-EFO revision in the headline Public Sector Net
#' Borrowing forecast into three top-level components - policy,
#' classifications and one-offs, and underlying (economic determinants) -
#' with sub-components for each.
#'
#' This is the dataset behind the "what changed?" attribution charts in
#' OBR fiscal commentary and IFS Green Budget chapters.
#'
#' @param unit Character. Either `"gbp_bn"` (GBP billion, the default) or
#'   `"pct_gdp"` (per cent of GDP).
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with columns:
#' \describe{
#'   \item{forecast_date}{Forecast vintage, e.g. `"November 2024"` (character).}
#'   \item{component}{Revision component. Top-level rows are `"Total"`,
#'     `"Policy"`, `"Classifications and one-offs"`, and `"Underlying"`.
#'     Sub-components carry the OBR's `"of which: ..."` labels (character).}
#'   \item{fiscal_year}{Fiscal year being revised, e.g. `"2024-25"` (character).}
#'   \item{value}{Revision value, in GBP billion or per cent of GDP per
#'     `unit` (numeric). A positive value indicates an upward revision to
#'     PSNB.}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' rev <- get_forecast_revisions()
#' # Top-level revisions only
#' rev[rev$component %in% c("Total", "Policy",
#'                          "Classifications and one-offs", "Underlying"), ]
#' options(op)
#' }
#'
#' @family forecasts
#' @export
get_forecast_revisions <- function(unit = c("gbp_bn", "pct_gdp"),
                                   refresh = FALSE) {
  unit <- match.arg(unit)
  src  <- frd_source(refresh)
  data <- parse_frd_revisions(src$path, unit)
  notes <- sprintf("Decomposition of PSNB forecast revisions in %s.",
                   if (unit == "gbp_bn") "GBP billion" else "per cent of GDP")
  frd_obr_tbl(data, src, notes = notes)
}
