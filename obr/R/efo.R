# EFO Detailed Forecast Tables
# URLs are resolved dynamically; the fallback is the latest known good URL
# at package release. Silent fallbacks were removed in 0.3.0; the resolver
# warns when it falls through.
EFO_AGGREGATES_FALLBACK <- "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-aggregates/"
EFO_AGGREGATES_FILENAME <- "efo_aggregates.xlsx"
EFO_ECONOMY_FALLBACK    <- "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-economy/"
EFO_ECONOMY_FILENAME    <- "efo_economy.xlsx"

efo_aggregates_source <- function(refresh = FALSE, vintage = NULL) {
  vintage <- resolve_efo_vintage(vintage)
  if (!is.null(vintage)) {
    return(efo_pinned_source(
      vintage  = vintage,
      suffix   = "detailed-forecast-tables-aggregates",
      stem     = "efo_aggregates",
      refresh  = refresh
    ))
  }
  obr_get_xlsx(
    candidates = efo_url_candidates("detailed-forecast-tables-aggregates"),
    fallback   = EFO_AGGREGATES_FALLBACK,
    filename   = EFO_AGGREGATES_FILENAME,
    refresh    = refresh,
    label      = "EFO Aggregates"
  )
}

efo_economy_source <- function(refresh = FALSE, vintage = NULL) {
  vintage <- resolve_efo_vintage(vintage)
  if (!is.null(vintage)) {
    return(efo_pinned_source(
      vintage  = vintage,
      suffix   = "detailed-forecast-tables-economy",
      stem     = "efo_economy",
      refresh  = refresh
    ))
  }
  obr_get_xlsx(
    candidates = efo_url_candidates("detailed-forecast-tables-economy"),
    fallback   = EFO_ECONOMY_FALLBACK,
    filename   = EFO_ECONOMY_FILENAME,
    refresh    = refresh,
    label      = "EFO Economy"
  )
}

# Download an EFO file pinned to a specific vintage. No URL probing: the
# vintage table determines the slug. Cache filenames carry the vintage tag
# so different vintages do not overwrite each other.
efo_pinned_source <- function(vintage, suffix, stem, refresh = FALSE) {
  url <- efo_url_for_vintage(vintage, suffix)
  filename <- sprintf("%s_%s.xlsx", stem, vintage_cache_tag(vintage))
  path <- obr_fetch(url, filename, refresh = refresh)
  list(
    path      = path,
    url       = url,
    final_url = url,
    source    = "pinned",
    retrieved = tryCatch(file.info(path)$mtime, error = function(e) Sys.time()),
    file_md5  = tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  )
}

# Build an obr_tbl from a parsed EFO frame and a source descriptor.
efo_obr_tbl <- function(data, src) {
  new_obr_tbl(
    data        = data,
    publication = "EFO",
    vintage     = obr_url_vintage(src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5
  )
}

# Parse sheet 6.5 (Components of Net Borrowing) from EFO aggregates file.
# Row 5 has fiscal year labels in columns that contain year-like strings.
# Data rows are those where col 2 is non-NA and at least one value is numeric.
# Returns the v0.4.0 schema: period, period_type, series, metric_type, value, unit.
parse_efo_fiscal <- function(path) {
  raw <- readxl::read_excel(path, sheet = "6.5",
                            col_names = FALSE, .name_repair = "minimal")

  all_row5 <- as.character(unlist(raw[5, ]))
  year_cols <- which(grepl("^[0-9]{4}-[0-9]{2}$", all_row5))
  fiscal_years <- all_row5[year_cols]

  col2 <- as.character(unlist(raw[, 2]))

  result_list <- list()
  for (i in seq_len(nrow(raw))) {
    nm <- col2[i]
    if (is.na(nm) || nm == "") next
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[i, year_cols])))
    )
    if (all(is.na(vals))) next
    result_list[[length(result_list) + 1]] <- obr_long(
      period      = fiscal_years,
      period_type = "fiscal_year",
      series      = nm,
      value       = vals,
      unit        = "gbp_bn",
      metric_type = "level"
    )
  }

  result <- do.call(rbind, result_list)
  result[!is.na(result$value), ]
}

# Generic parser for EFO economy sheets (quarterly, wide format).
# Finds the first row where col 2 has a quarterly period (e.g. "2008Q1"),
# then takes the row immediately before it as the series-name header.
#
# Returns the v0.4.0 schema: period, period_type, series, metric_type, value, unit.
#
# metric_type is set per series, in order:
#   1. heuristic classifier: detects "Index" / "deflator" → "index",
#      "rate"/"share" → "pct", "growth"/"inflation" → "yoy_pct"
#   2. for series the classifier returns "level" on, the caller's
#      `default_metric_type` is applied (e.g. "yoy_pct" for the inflation sheet,
#      where bare names like "CPI" / "RPI" denote annual rates by convention)
#
# This solves the v0.3.x bug where (e.g.) CPI YoY values and any CPI Index
# values lived in the same `value` column with no machine-readable distinction.
# The per-measure default also fixes the related issue that bare names like
# "CPI" do not contain "inflation" / "growth" / "rate" in the source sheet,
# so the classifier alone cannot tell what units they are in.
parse_efo_economy_sheet <- function(path, sheet,
                                    default_metric_type = NULL,
                                    default_unit        = NA_character_) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  col2 <- as.character(unlist(raw[, 2]))
  is_period <- grepl("^[0-9]{4}Q[1-4]$", col2)
  first_data_row <- which(is_period)[1]
  if (is.na(first_data_row)) return(NULL)

  header_row <- NA
  for (i in (first_data_row - 1):1) {
    v <- as.character(unlist(raw[i, 3]))
    if (!is.na(v) && is.na(suppressWarnings(as.numeric(v)))) {
      header_row <- i
      break
    }
  }
  if (is.na(header_row)) return(NULL)

  series <- trimws(gsub("\r\n", " ",
                        as.character(unlist(raw[header_row, 3:ncol(raw)]))))
  valid_series <- !is.na(series) & series != ""

  data_idx <- which(is_period)
  periods  <- col2[data_idx]

  metric <- classify_metric_type(series)
  if (!is.null(default_metric_type)) {
    metric[!is.na(metric) & metric == "level"] <- default_metric_type
  }
  unit <- default_unit_for_metric(metric)
  if (!is.na(default_unit)) {
    unit[is.na(unit)] <- default_unit
  }

  result_list <- list()
  for (j in which(valid_series)) {
    col_idx <- j + 2L
    if (col_idx > ncol(raw)) next
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, col_idx])))
    )
    if (all(is.na(vals))) next
    result_list[[length(result_list) + 1]] <- obr_long(
      period      = periods,
      period_type = "quarter",
      series      = series[j],
      value       = vals,
      unit        = unit[j],
      metric_type = metric[j]
    )
  }

  if (length(result_list) == 0) return(NULL)
  result <- do.call(rbind, result_list)
  result[!is.na(result$value), ]
}

# Special-case parser for sheet 1.14 (output gap).
# Fix in 0.3.0: locate the value column by scanning each column for the one
# containing the most numeric entries aligned with the quarterly periods,
# rather than hardcoding column 3. This protects us from OBR moving the
# column or inserting a leading column.
parse_efo_output_gap <- function(path) {
  raw <- readxl::read_excel(path, sheet = "1.14",
                            col_names = FALSE, .name_repair = "minimal")
  col2 <- as.character(unlist(raw[, 2]))
  is_period <- grepl("^[0-9]{4}Q[1-4]$", col2)
  data_idx  <- which(is_period)
  if (length(data_idx) == 0L) return(NULL)

  best_col <- NA_integer_
  best_n   <- -1L
  for (j in 3:ncol(raw)) {
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, j])))
    )
    n <- sum(!is.na(vals))
    if (n > best_n) {
      best_n   <- n
      best_col <- j
    }
  }
  if (is.na(best_col)) return(NULL)

  obr_long(
    period      = col2[data_idx],
    period_type = "quarter",
    series      = "Output gap",
    value       = suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, best_col])))
    ),
    unit        = "pct",
    metric_type = "pct"
  )
}

#' List available EFO economy measures
#'
#' Returns a data frame of the economy measures available via
#' [get_efo_economy()], showing the `measure` name to pass and a
#' short description of what each covers.
#'
#' @return A data frame with columns `measure`, `sheet`, and `description`.
#'
#' @examples
#' list_efo_economy_measures()
#'
#' @family EFO
#' @export
list_efo_economy_measures <- function() {
  data.frame(
    measure = c("labour", "inflation", "output_gap"),
    sheet   = c("1.6", "1.7", "1.14"),
    description = c(
      "Labour market: employment, unemployment rate, participation rate, hours worked",
      "Inflation: CPI, CPIH, RPI, RPIX, mortgage rates, rents",
      "OBR central estimate of the output gap (% of potential output)"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get EFO fiscal projections (net borrowing components)
#'
#' Downloads (and caches) the OBR \emph{Economic and Fiscal Outlook} Detailed
#' Forecast Tables - Aggregates file and returns the components of net
#' borrowing (Table 6.5) in tidy long format.
#'
#' Covers the five-year forecast horizon published at the most recent
#' fiscal event. Key series include current receipts, current expenditure,
#' depreciation, net investment, and net borrowing (PSNB). The exact
#' vintage is recorded in the returned object's provenance attribute and
#' visible in the printed header.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#' @param vintage Optional EFO vintage label such as `"October 2024"`. If
#'   supplied, the function downloads the file for that specific EFO. If
#'   `NULL` (the default), the function uses any vintage set via [obr_pin()],
#'   or falls back to the latest live EFO via the dynamic URL resolver. See
#'   [obr_efo_vintages()] for the full list of supported vintages.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema (columns:
#' `period`, `period_type`, `series`, `metric_type`, `value`, `unit`):
#' \describe{
#'   \item{period}{Fiscal year being forecast, e.g. `"2025-26"` (character)}
#'   \item{period_type}{Always `"fiscal_year"` for this function (character)}
#'   \item{series}{Component name, e.g. `"Net borrowing"` (character)}
#'   \item{metric_type}{Always `"level"` for this function (character)}
#'   \item{value}{Projected value (numeric)}
#'   \item{unit}{Always `"gbp_bn"` for this function (character)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' efo <- get_efo_fiscal()
#' efo[efo$series == "Net borrowing", ]
#' obr_provenance(efo)$vintage
#'
#' # Pin to a specific EFO for reproducibility
#' october_2024 <- get_efo_fiscal(vintage = "October 2024")
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_efo_fiscal <- function(refresh = FALSE, vintage = NULL) {
  # v0.5.0: thin wrapper over the generic dispatcher.
  get_efo_table("6.5", vintage = vintage, refresh = refresh)
}

#' Get EFO economy projections
#'
#' Downloads (and caches) the OBR \emph{Economic and Fiscal Outlook} Detailed
#' Forecast Tables - Economy file and returns quarterly economic projections
#' for a chosen measure in tidy long format.
#'
#' Data run from 2008 Q1 through the current forecast horizon. Use
#' [list_efo_economy_measures()] to see all available measures.
#'
#' @param measure Character. Which economy table to return. One of
#'   `"inflation"`, `"labour"`, or `"output_gap"`. Defaults to
#'   `"inflation"`.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#' @param vintage Optional EFO vintage label such as `"October 2024"`. If
#'   supplied, the function downloads the file for that specific EFO. If
#'   `NULL` (the default), the function uses any vintage set via [obr_pin()],
#'   or falls back to the latest live EFO via the dynamic URL resolver.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema (columns:
#' `period`, `period_type`, `series`, `metric_type`, `value`, `unit`):
#' \describe{
#'   \item{period}{Calendar quarter, e.g. `"2025Q1"` (character)}
#'   \item{period_type}{Always `"quarter"` for this function (character)}
#'   \item{series}{Variable name, e.g. `"CPI"` (character)}
#'   \item{metric_type}{One of `"index"`, `"yoy_pct"`, `"pct"`, `"level"`,
#'     classified from the series name. This is the v0.4.0 fix for the v0.3.x
#'     issue where, e.g., CPI Index values and CPI YoY values shared a single
#'     `value` column with no machine-readable distinction.}
#'   \item{value}{Numeric value in units described by `unit`}
#'   \item{unit}{One of `"index"`, `"pct"`, etc., paired with `metric_type`}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' inf <- get_efo_economy("inflation")
#' inf[inf$series == "CPI", ]
#'
#' lab <- get_efo_economy("labour")
#'
#' # Compare CPI projections from two different EFOs
#' inf_oct24 <- get_efo_economy("inflation", vintage = "October 2024")
#' inf_mar26 <- get_efo_economy("inflation", vintage = "March 2026")
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_efo_economy <- function(measure = c("inflation", "labour", "output_gap"),
                            refresh = FALSE,
                            vintage = NULL) {
  # v0.5.0: thin wrapper over the generic dispatcher. Per-measure metric
  # defaults (e.g. inflation = yoy_pct/pct) live in the catalogue.
  measure <- match.arg(measure)
  table_id <- switch(measure,
                     "inflation"  = "1.7",
                     "labour"     = "1.6",
                     "output_gap" = "1.14")
  get_efo_table(table_id, vintage = vintage, refresh = refresh)
}
