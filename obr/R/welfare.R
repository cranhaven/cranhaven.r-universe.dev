# Welfare Trends Report: charts and tables.
# URL resolved dynamically; resolver warns if it falls through to the
# hardcoded fallback.
WTR_FALLBACK <- "https://obr.uk/download/welfare-trends-report-october-2024-charts-and-tables/"
WTR_FILENAME <- "welfare_trends.xlsx"

wtr_source <- function(refresh = FALSE) {
  obr_get_xlsx(
    candidates = wtr_url_candidates(),
    fallback   = WTR_FALLBACK,
    filename   = WTR_FILENAME,
    refresh    = refresh,
    label      = "Welfare Trends Report"
  )
}

wtr_obr_tbl <- function(data, src) {
  new_obr_tbl(
    data        = data,
    publication = "WTR",
    vintage     = obr_url_vintage(src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5
  )
}

# Generic parser for WTR chart data sheets.
# Layout: col 1 = NA (except "Back to contents" in row 1),
#         col 2 = chart title (row 2) then series names in last data rows,
#         cols 3+ = fiscal year labels (year_row) then values (data rows).
#
# Returns the v0.4.0 schema: period, period_type, series, metric_type, value, unit.
# Most WTR data is denominated as a percentage of GDP; the unit defaults to
# "pct" when the heuristic classifier cannot assign a more specific unit.
# Caller can override by post-processing the returned frame (e.g. caseload
# series that are denominated in thousands of claimants).
parse_wtr_chart <- function(path, sheet, unit_default = "pct") {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")
  col2 <- as.character(unlist(raw[, 2]))

  non_na    <- which(!is.na(col2) & col2 != "")
  data_rows <- non_na[non_na > 2L]
  if (length(data_rows) == 0L) return(NULL)

  year_row_idx <- data_rows[1L] - 1L
  all_yr       <- as.character(unlist(raw[year_row_idx, ]))
  year_cols    <- which(grepl("^[0-9]{4}-[0-9]{2}$", all_yr))
  fiscal_years <- all_yr[year_cols]
  if (length(year_cols) == 0L) return(NULL)

  series_names <- col2[data_rows]

  result_list <- vector("list", length(data_rows))
  for (j in seq_along(data_rows)) {
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_rows[j], year_cols])))
    )
    metric <- classify_metric_type(series_names[j])
    derived_unit <- default_unit_for_metric(metric)
    unit <- if (is.na(derived_unit)) unit_default else derived_unit
    result_list[[j]] <- obr_long(
      period      = fiscal_years,
      period_type = "fiscal_year",
      series      = series_names[j],
      value       = vals,
      unit        = unit,
      metric_type = metric
    )
  }

  result <- do.call(rbind, result_list)
  result[!is.na(result$value), ]
}

#' Get working-age welfare spending
#'
#' Downloads (and caches) the OBR Welfare Trends Report charts and tables
#' workbook and returns annual working-age welfare spending as a share of
#' GDP, split into incapacity-related and non-incapacity spending.
#'
#' Data cover fiscal years from 1978-79 through the current forecast horizon.
#' The exact vintage is recorded in the returned object's provenance.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema (columns:
#' `period`, `period_type`, `series`, `metric_type`, `value`, `unit`).
#' Values are spending as a percentage of GDP; `metric_type` is `"pct"`,
#' `unit` is `"pct"`.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' welfare <- get_welfare_spending()
#' welfare[welfare$series == "Working-age incapacity benefits spending" &
#'         welfare$period >= "2000-01", ]
#' options(op)
#' }
#'
#' @family welfare
#' @export
get_welfare_spending <- function(refresh = FALSE) {
  src <- wtr_source(refresh)
  wtr_obr_tbl(parse_wtr_chart(src$path, "C1.3"), src)
}

#' Get incapacity benefits spending by type
#'
#' Downloads (and caches) the OBR Welfare Trends Report charts and tables
#' workbook and returns annual spending on each incapacity benefit as a
#' share of GDP, from 1978-79 to the current forecast horizon.
#'
#' Series include: Invalidity Benefit, Incapacity Benefit, Employment and
#' Support Allowance (ESA), Sickness Benefit, and Severe Disablement
#' Allowance.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema. `series` is the
#' benefit name, values are spending as a percentage of GDP, `metric_type`
#' is `"pct"`, `unit` is `"pct"`. See [get_public_finances()] for full
#' column docs.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' ib <- get_incapacity_spending()
#' unique(ib$series)
#' options(op)
#' }
#'
#' @family welfare
#' @export
get_incapacity_spending <- function(refresh = FALSE) {
  src <- wtr_source(refresh)
  wtr_obr_tbl(parse_wtr_chart(src$path, "C1.1"), src)
}

#' Get incapacity benefit caseloads
#'
#' Downloads (and caches) the OBR Welfare Trends Report charts and tables
#' workbook and returns the combined incapacity benefit caseload since
#' 2008-09, in both absolute terms (thousands of claimants) and as a share
#' of the working-age population.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema. The two series
#' (`"Claimants"` and `"Share of working age population"`) carry different
#' units: claimants are in thousands and the share is a percentage. After
#' calling, the caller may want to overwrite `unit` to `"count_k"` for the
#' claimants series, since the heuristic classifier cannot infer the
#' "thousands" denomination from the series name alone.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' cases <- get_incapacity_caseloads()
#' cases[cases$series == "Claimants", ]
#' options(op)
#' }
#'
#' @family welfare
#' @export
get_incapacity_caseloads <- function(refresh = FALSE) {
  src <- wtr_source(refresh)
  out <- parse_wtr_chart(src$path, "C3.1")
  if (!is.null(out)) {
    is_claimants <- out$series == "Claimants"
    out$unit[is_claimants] <- "count_k"
    out$metric_type[is_claimants] <- "level"
  }
  wtr_obr_tbl(out, src)
}
