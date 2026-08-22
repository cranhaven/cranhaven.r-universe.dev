# Stable OBR download URL (server redirects to the latest vintage).
PFD_URL      <- "https://obr.uk/download/public-finances-databank/"
PFD_FILENAME <- "public_finances_databank.xlsx"

pfd_source <- function(refresh = FALSE) {
  obr_get_xlsx(
    candidates = PFD_URL,
    fallback   = PFD_URL,
    filename   = PFD_FILENAME,
    refresh    = refresh,
    label      = "Public Finances Databank"
  )
}

# Strip a trailing footnote marker (1 or 2 trailing digits) from a label, but
# only when preceded by a letter, period, or whitespace, so genuinely numeric
# trailing chars (e.g. years) are preserved. This protects series names that
# legitimately end in digits from being silently corrupted.
strip_footnote <- function(x) {
  trimws(gsub("(?<=[A-Za-z\\.\\s])[0-9]{1,2}$", "", x, perl = TRUE))
}

# Parse "Aggregates (£bn)" sheet in tidy long format.
# Row 4 = series names; data starts at row 8.
parse_aggregates_bn <- function(path) {
  sheet <- resolve_sheet_name(
    path,
    primary          = "Aggregates (\u00a3bn)",
    fallback_pattern = "^Aggregates \\(.{1,2}bn\\)$"
  )
  raw <- readxl::read_excel(path, sheet = sheet,
                             col_names = FALSE, .name_repair = "minimal")

  series_names      <- as.character(unlist(raw[4, ]))
  series_names[1]   <- "year"
  series_names[series_names %in% c("NA", "NULL")] <- NA

  keep <- !is.na(series_names)
  data <- as.data.frame(raw[8:nrow(raw), keep], stringsAsFactors = FALSE)
  names(data) <- series_names[keep]

  data <- data[grepl("^[0-9]{4}-[0-9]{2}", data$year), ]

  value_cols <- setdiff(names(data), "year")
  result <- do.call(rbind, lapply(value_cols, function(col) {
    metric <- classify_metric_type(col)
    derived_unit <- default_unit_for_metric(metric)
    unit <- if (is.na(derived_unit)) "gbp_bn" else derived_unit
    obr_long(
      period      = data$year,
      period_type = "fiscal_year",
      series      = col,
      value       = suppressWarnings(as.numeric(data[[col]])),
      unit        = unit,
      metric_type = metric
    )
  }))
  result[!is.na(result$value), ]
}

# Parse "Receipts (£bn)" sheet in tidy long format.
# Row 4 = series names; data starts at row 7.
parse_receipts_bn <- function(path) {
  sheet <- resolve_sheet_name(
    path,
    primary          = "Receipts (\u00a3bn)",
    fallback_pattern = "^Receipts \\(.{1,2}bn\\)$"
  )
  raw <- readxl::read_excel(path, sheet = sheet,
                             col_names = FALSE, .name_repair = "minimal")

  series_names    <- as.character(unlist(raw[4, ]))
  series_names[1] <- "year"
  series_names[series_names %in% c("NA", "NULL")] <- NA
  series_names    <- strip_footnote(series_names)
  series_names[series_names == ""] <- NA

  keep <- !is.na(series_names)
  data <- as.data.frame(raw[7:nrow(raw), keep], stringsAsFactors = FALSE)
  names(data) <- series_names[keep]

  data <- data[grepl("^[0-9]{4}-[0-9]{2}", data$year), ]

  value_cols <- setdiff(names(data), "year")
  result <- do.call(rbind, lapply(value_cols, function(col) {
    obr_long(
      period      = data$year,
      period_type = "fiscal_year",
      series      = col,
      value       = suppressWarnings(as.numeric(data[[col]])),
      unit        = "gbp_bn",
      metric_type = "level"
    )
  }))
  result[!is.na(result$value), ]
}

# Internal: build an obr_tbl from a parsed PFD frame and a source descriptor.
pfd_obr_tbl <- function(data, src) {
  new_obr_tbl(
    data        = data,
    publication = "PFD",
    vintage     = obr_url_vintage(src$final_url %||% src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5
  )
}

#' Get all Public Finances Databank aggregates
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns all
#' aggregate fiscal series in tidy long format. Covers outturn from 1946-47
#' and OBR projections through the current forecast horizon.
#'
#' Series include: Public sector net borrowing, Public sector net debt, Total
#' managed expenditure, Public sector current receipts, Nominal GDP, GDP
#' deflator, and more.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` (a `data.frame` with attached provenance) with the
#' standard v0.4.0 schema (columns: `period`, `period_type`, `series`,
#' `metric_type`, `value`, `unit`):
#' \describe{
#'   \item{period}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{period_type}{Always `"fiscal_year"` for this function}
#'   \item{series}{Series name (character)}
#'   \item{metric_type}{Usually `"level"`; ratio or index series get a more
#'     specific value derived from the series name}
#'   \item{value}{Numeric value in units described by `unit`}
#'   \item{unit}{Usually `"gbp_bn"`; ratios and indices override}
#' }
#' Use [obr_provenance()] to extract source URL, vintage, and retrieval time.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' pf <- get_public_finances()
#' unique(pf$series)
#' obr_provenance(pf)
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_public_finances <- function(refresh = FALSE) {
  src <- pfd_source(refresh)
  pfd_obr_tbl(parse_aggregates_bn(src$path), src)
}

#' Get Public Sector Net Borrowing (PSNB)
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Public Sector Net Borrowing in \enc{£}{GBP} billion. A positive
#' value means the government is borrowing (deficit); a negative value means
#' a surplus.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema. `series` is
#' `"PSNB"`, `metric_type` is `"level"`, `unit` is `"gbp_bn"`. See
#' [get_public_finances()] for column definitions.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' psnb <- get_psnb()
#' tail(psnb)
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_psnb <- function(refresh = FALSE) {
  src <- pfd_source(refresh)
  agg <- parse_aggregates_bn(src$path)
  out <- agg[agg$series == "Public sector net borrowing", ]
  out$series <- "PSNB"
  rownames(out) <- NULL
  pfd_obr_tbl(out, src)
}

#' Get Public Sector Net Debt (PSND)
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Public Sector Net Debt in \enc{£}{GBP} billion.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema. `series` is
#' `"PSND"`, `metric_type` is `"level"`, `unit` is `"gbp_bn"`. See
#' [get_public_finances()] for column definitions.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' psnd <- get_psnd()
#' tail(psnd)
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_psnd <- function(refresh = FALSE) {
  src <- pfd_source(refresh)
  agg <- parse_aggregates_bn(src$path)
  out <- agg[agg$series == "Public sector net debt", ]
  out$series <- "PSND"
  rownames(out) <- NULL
  pfd_obr_tbl(out, src)
}

#' Get Total Managed Expenditure
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Total Managed Expenditure (TME) in \enc{£}{GBP} billion. TME is
#' the broadest measure of UK government spending.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema. `series` is
#' `"TME"`, `metric_type` is `"level"`, `unit` is `"gbp_bn"`. See
#' [get_public_finances()] for column definitions.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' tme <- get_expenditure()
#' tail(tme)
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_expenditure <- function(refresh = FALSE) {
  src <- pfd_source(refresh)
  agg <- parse_aggregates_bn(src$path)
  out <- agg[agg$series == "Total managed expenditure", ]
  out$series <- "TME"
  rownames(out) <- NULL
  pfd_obr_tbl(out, src)
}

#' Get public sector receipts by tax type
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' public sector current receipts broken down by individual tax type,
#' in tidy long format. Coverage begins in 1999-00.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema (columns:
#' `period`, `period_type`, `series`, `metric_type`, `value`, `unit`).
#' `series` is the tax or receipt category, `metric_type` is `"level"`,
#' `unit` is `"gbp_bn"`. See [get_public_finances()] for full column docs.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' receipts <- get_receipts()
#' receipts[grepl("income tax", receipts$series, ignore.case = TRUE), ]
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_receipts <- function(refresh = FALSE) {
  src <- pfd_source(refresh)
  pfd_obr_tbl(parse_receipts_bn(src$path), src)
}
