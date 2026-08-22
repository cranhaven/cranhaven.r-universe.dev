# EFO Monthly Profiles.
# About a month after each EFO, the OBR publishes a workbook of monthly
# profiles for receipts, spending, and the CGNCR, consistent with that EFO's
# full-year forecast. The ONS/HMT monthly public sector finances release is
# then judged against these profiles ("borrowing so far this year vs the
# OBR profile"), making this the most-watched OBR product between fiscal
# events, and the reference point for the run-up to a Budget.

MONTHLY_PROFILES_FALLBACK <- "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-monthly-profiles/"
MONTHLY_PROFILES_FILENAME <- "efo_monthly_profiles.xlsx"

monthly_profiles_source <- function(refresh = FALSE, vintage = NULL) {
  vintage <- resolve_efo_vintage(vintage)
  if (!is.null(vintage)) {
    return(efo_pinned_source(
      vintage = vintage,
      suffix  = "monthly-profiles",
      stem    = "efo_monthly_profiles",
      refresh = refresh
    ))
  }
  obr_get_xlsx(
    candidates = efo_url_candidates("monthly-profiles"),
    fallback   = MONTHLY_PROFILES_FALLBACK,
    filename   = MONTHLY_PROFILES_FILENAME,
    refresh    = refresh,
    label      = "EFO Monthly Profiles"
  )
}

MONTH_ABBREVS <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep",
                   "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")

# Convert a month abbreviation to a "YYYY-MM" period string given the start
# year of the fiscal year it belongs to (Apr-Dec = start year, Jan-Mar = +1).
month_to_period <- function(month_abbrev, fy_start_year) {
  idx  <- match(month_abbrev, MONTH_ABBREVS)
  mnum <- ((idx + 2L) %% 12L) + 1L                # Apr -> 4, ..., Mar -> 3
  year <- ifelse(mnum >= 4L, fy_start_year, fy_start_year + 1L)
  sprintf("%d-%02d", year, mnum)
}

# Parse one monthly-profiles sheet. Layout: series names in col 1 (with
# sparse "of which:" markers), the full-year EFO forecast in the column
# headed "EFO Forecast", and monthly values under an Apr..Mar header row.
parse_monthly_profiles_sheet <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  month_regex <- paste0("^(", paste(MONTH_ABBREVS, collapse = "|"), ")$")

  # Month header row: first row with at least six month abbreviations.
  month_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    cells <- trimws(as.character(unlist(raw[i, ])))
    if (sum(grepl(month_regex, cells), na.rm = TRUE) >= 6L) {
      month_row <- i
      break
    }
  }
  if (is.na(month_row)) return(NULL)

  month_cells <- trimws(as.character(unlist(raw[month_row, ])))
  month_cols  <- which(grepl(month_regex, month_cells))
  months      <- month_cells[month_cols]

  # Fiscal year: first "YYYY-YY" anywhere at or above the month header row.
  fiscal_year <- NA_character_
  for (i in seq_len(month_row)) {
    cells <- as.character(unlist(raw[i, ]))
    m <- regmatches(cells, regexpr("[0-9]{4}-[0-9]{2}", cells))
    m <- m[!is.na(m) & nzchar(m)]
    if (length(m) > 0L) { fiscal_year <- m[1L]; break }
  }
  if (is.na(fiscal_year)) return(NULL)
  fy_start_year <- as.integer(substr(fiscal_year, 1L, 4L))

  # Full-year forecast column: the column whose header mentions "Forecast"
  # above the month row (e.g. "Mar26 EFO Forecast"). Restricted to the
  # columns between the series names (col 1, whose sheet title also says
  # "forecast") and the first month column.
  fy_col <- NA_integer_
  fy_search_cols <- setdiff(seq_len(min(month_cols) - 1L), 1L)
  for (i in seq_len(month_row)) {
    if (length(fy_search_cols) == 0L) break
    cells <- as.character(unlist(raw[i, fy_search_cols]))
    hit <- which(grepl("Forecast", cells, ignore.case = TRUE))
    if (length(hit) > 0L) { fy_col <- fy_search_cols[hit[1L]]; break }
  }

  periods <- month_to_period(months, fy_start_year)

  results <- list()
  if (month_row >= nrow(raw)) return(NULL)
  for (i in (month_row + 1L):nrow(raw)) {
    series <- trimws(as.character(unlist(raw[i, 1L])))
    if (is.na(series) || !nzchar(series)) next
    if (grepl("^of which", series, ignore.case = TRUE)) next
    if (grepl("^Note", series, ignore.case = TRUE)) next

    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[i, month_cols])))
    )
    fy_val <- if (!is.na(fy_col)) {
      suppressWarnings(as.numeric(as.character(unlist(raw[i, fy_col]))))
    } else {
      NA_real_
    }
    if (all(is.na(vals)) && is.na(fy_val)) next

    part <- obr_long(
      period      = periods,
      period_type = "month",
      series      = series,
      value       = vals,
      unit        = "gbp_bn",
      metric_type = "level"
    )
    if (!is.na(fy_val)) {
      part <- rbind(part, obr_long(
        period      = fiscal_year,
        period_type = "fiscal_year",
        series      = series,
        value       = fy_val,
        unit        = "gbp_bn",
        metric_type = "level"
      ))
    }
    results[[length(results) + 1L]] <- part
  }

  if (length(results) == 0L) return(NULL)
  out <- do.call(rbind, results)
  out[!is.na(out$value), ]
}

#' Get the OBR monthly profiles for the public finances
#'
#' Downloads (and caches) the monthly profiles workbook the OBR publishes
#' alongside each \emph{Economic and Fiscal Outlook}. The profiles apportion
#' the full-year EFO forecast for receipts, spending, and the central
#' government net cash requirement (CGNCR) across the twelve months of the
#' fiscal year, so that each month's ONS/HMT public sector finances outturn
#' can be judged against the path implied by the OBR's forecast.
#'
#' This is the reference point used every month in the run-up to a fiscal
#' event ("borrowing so far this year vs the OBR profile"). The OBR itself
#' publishes a monthly commentary against these profiles; this function
#' provides the underlying numbers in tidy long format. Pair with monthly
#' outturn data (e.g. from the ONS public sector finances release) to
#' compute in-year deviations from profile.
#'
#' The profiles workbook is typically published a few weeks after the EFO
#' itself. The OBR describes the profiles as broad-brush and illustrative;
#' see the Notes sheet of the source workbook.
#'
#' @param sheet Which profile table to return. `"profiles"` (the default)
#'   returns the receipts and spending profiles; `"cgncr"` returns the
#'   central government net cash requirement breakdown.
#' @param vintage Optional EFO vintage label such as `"March 2026"`. If
#'   `NULL` (the default), uses any pin set via [obr_pin()] or resolves the
#'   latest live profiles workbook.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard schema columns (`period`,
#' `period_type`, `series`, `metric_type`, `value`, `unit`). Monthly rows
#' have `period_type = "month"` and `period` in `"YYYY-MM"` format; each
#' series also carries one `period_type = "fiscal_year"` row holding the
#' full-year EFO forecast the profile sums to. All values are GBP billion.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#'
#' mp <- tryCatch(get_monthly_profiles(), error = function(e) NULL)
#' if (!is.null(mp)) {
#'   # Monthly profile for HMRC cash receipts
#'   mp[mp$series == "HMRC cash receipts" & mp$period_type == "month", ]
#' }
#'
#' # CGNCR breakdown by month
#' cg <- tryCatch(get_monthly_profiles("cgncr"), error = function(e) NULL)
#'
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_monthly_profiles <- function(sheet = c("profiles", "cgncr"),
                                 vintage = NULL,
                                 refresh = FALSE) {
  sheet <- match.arg(sheet)

  src <- monthly_profiles_source(refresh = refresh, vintage = vintage)

  sheet_name <- if (sheet == "profiles") {
    resolve_sheet_name(src$path, "Monthly Profiles", "[Pp]rofiles")
  } else {
    resolve_sheet_name(src$path, "CGNCR Breakdown", "CGNCR")
  }

  data <- parse_monthly_profiles_sheet(src$path, sheet_name)
  if (is.null(data)) {
    cli::cli_abort(c(
      "Could not parse sheet {.val {sheet_name}} of the monthly profiles workbook.",
      "!" = "The OBR may have changed the layout. Please file an issue at https://github.com/charlescoverdale/obr/issues."
    ))
  }

  new_obr_tbl(
    data        = data,
    publication = "EFO-MP",
    vintage     = obr_url_vintage(src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5,
    notes       = "Monthly profiles are broad-brush and illustrative (OBR). Each series also carries a fiscal_year row with the full-year EFO forecast."
  )
}
