# Stable OBR download URL (redirects to latest file)
PFD_URL      <- "https://obr.uk/download/public-finances-databank/"
PFD_FILENAME <- "public_finances_databank.xlsx"

pfd_path <- function(refresh = FALSE) {
  obr_fetch(PFD_URL, PFD_FILENAME, refresh = refresh)
}

# Parse "Aggregates (£bn)" sheet → tidy long format
# Row 4 = series names; data starts at row 8
parse_aggregates_bn <- function(path) {
  raw <- readxl::read_excel(path, sheet = "Aggregates (\u00a3bn)",
                             col_names = FALSE, .name_repair = "minimal")

  series_names      <- as.character(unlist(raw[4, ]))
  series_names[1]   <- "year"
  series_names[series_names %in% c("NA", "NULL")] <- NA

  keep <- !is.na(series_names)
  data <- as.data.frame(raw[8:nrow(raw), keep], stringsAsFactors = FALSE)
  names(data) <- series_names[keep]

  # Keep only fiscal year rows (e.g. "1946-47", "2024-25")
  data <- data[grepl("^[0-9]{4}-[0-9]{2}", data$year), ]

  # Pivot to long format
  value_cols <- setdiff(names(data), "year")
  result <- do.call(rbind, lapply(value_cols, function(col) {
    data.frame(
      year   = data$year,
      series = col,
      value  = suppressWarnings(as.numeric(data[[col]])),
      stringsAsFactors = FALSE
    )
  }))
  result[!is.na(result$value), ]
}

# Parse "Receipts (£bn)" sheet → tidy long format
# Row 4 = series names; data starts at row 7
parse_receipts_bn <- function(path) {
  raw <- readxl::read_excel(path, sheet = "Receipts (\u00a3bn)",
                             col_names = FALSE, .name_repair = "minimal")

  series_names    <- as.character(unlist(raw[4, ]))
  series_names[1] <- "year"
  series_names[series_names %in% c("NA", "NULL")] <- NA
  # Strip trailing footnote digits from column names
  series_names <- trimws(gsub("[0-9]+$", "", series_names))
  series_names[series_names == ""] <- NA

  keep <- !is.na(series_names)
  data <- as.data.frame(raw[7:nrow(raw), keep], stringsAsFactors = FALSE)
  names(data) <- series_names[keep]

  data <- data[grepl("^[0-9]{4}-[0-9]{2}", data$year), ]

  value_cols <- setdiff(names(data), "year")
  result <- do.call(rbind, lapply(value_cols, function(col) {
    data.frame(
      year   = data$year,
      series = col,
      value  = suppressWarnings(as.numeric(data[[col]])),
      stringsAsFactors = FALSE
    )
  }))
  result[!is.na(result$value), ]
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
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{series}{Series name (character)}
#'   \item{value}{Value in £ billion (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' pf <- get_public_finances()
#' unique(pf$series)
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_public_finances <- function(refresh = FALSE) {
  parse_aggregates_bn(pfd_path(refresh))
}

#' Get Public Sector Net Borrowing (PSNB)
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Public Sector Net Borrowing in £ billion. A positive value means
#' the government is borrowing (deficit); a negative value means a surplus.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{psnb_bn}{PSNB in £ billion (numeric)}
#' }
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
  agg <- parse_aggregates_bn(pfd_path(refresh))
  result <- agg[agg$series == "Public sector net borrowing", c("year", "value")]
  names(result)[2] <- "psnb_bn"
  rownames(result) <- NULL
  result
}

#' Get Public Sector Net Debt (PSND)
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Public Sector Net Debt in £ billion.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{psnd_bn}{PSND in £ billion (numeric)}
#' }
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
  agg <- parse_aggregates_bn(pfd_path(refresh))
  result <- agg[agg$series == "Public sector net debt", c("year", "value")]
  names(result)[2] <- "psnd_bn"
  rownames(result) <- NULL
  result
}

#' Get Total Managed Expenditure
#'
#' Downloads (and caches) the OBR Public Finances Databank and returns
#' annual Total Managed Expenditure (TME) in £ billion. TME is the broadest
#' measure of UK government spending.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{tme_bn}{Total managed expenditure in £ billion (numeric)}
#' }
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
  agg <- parse_aggregates_bn(pfd_path(refresh))
  result <- agg[agg$series == "Total managed expenditure", c("year", "value")]
  names(result)[2] <- "tme_bn"
  rownames(result) <- NULL
  result
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
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{Fiscal year (character, e.g. `"2024-25"`)}
#'   \item{series}{Tax or receipt category (character)}
#'   \item{value}{Value in £ billion (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' receipts <- get_receipts()
#' # Filter to income tax
#' receipts[grepl("income tax", receipts$series, ignore.case = TRUE), ]
#' options(op)
#' }
#'
#' @family public finances
#' @export
get_receipts <- function(refresh = FALSE) {
  parse_receipts_bn(pfd_path(refresh))
}
