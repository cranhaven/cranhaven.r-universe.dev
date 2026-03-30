# March 2026 EFO Detailed Forecast Tables
EFO_DATE               <- "March 2026"
EFO_AGGREGATES_URL     <- "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-aggregates/"
EFO_AGGREGATES_FILENAME <- "efo_aggregates.xlsx"
EFO_ECONOMY_URL        <- "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-economy/"
EFO_ECONOMY_FILENAME   <- "efo_economy.xlsx"

efo_aggregates_path <- function(refresh = FALSE) {
  obr_fetch(EFO_AGGREGATES_URL, EFO_AGGREGATES_FILENAME, refresh = refresh)
}

efo_economy_path <- function(refresh = FALSE) {
  obr_fetch(EFO_ECONOMY_URL, EFO_ECONOMY_FILENAME, refresh = refresh)
}

# Parse sheet 6.5 (Components of Net Borrowing) from EFO aggregates file.
# Row 5 has fiscal year labels in the columns that contain year-like strings.
# Data rows are those where col 2 is non-NA and at least one value is numeric.
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
    result_list[[length(result_list) + 1]] <- data.frame(
      fiscal_year = fiscal_years,
      series      = nm,
      value_bn    = vals,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, result_list)
  result[!is.na(result$value_bn), ]
}

# Generic parser for EFO economy sheets (quarterly, wide format).
# Finds the first row where col 2 has a quarterly period (e.g. "2008Q1"),
# then takes the row immediately before it as the series name header.
parse_efo_economy_sheet <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  col2 <- as.character(unlist(raw[, 2]))
  is_period <- grepl("^[0-9]{4}Q[1-4]$", col2)
  first_data_row <- which(is_period)[1]
  if (is.na(first_data_row)) return(NULL)

  # Walk back from first data row to find the series name row
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

  result_list <- list()
  for (j in which(valid_series)) {
    col_idx <- j + 2L  # series[1] == col 3, series[2] == col 4, etc.
    if (col_idx > ncol(raw)) next
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, col_idx])))
    )
    if (all(is.na(vals))) next
    result_list[[length(result_list) + 1]] <- data.frame(
      period = periods,
      series = series[j],
      value  = vals,
      stringsAsFactors = FALSE
    )
  }

  if (length(result_list) == 0) return(NULL)
  result <- do.call(rbind, result_list)
  result[!is.na(result$value), ]
}

# Special-case parser for sheet 1.14 (output gap): data starts at row 3 with
# no series name header row; single series.
parse_efo_output_gap <- function(path) {
  raw <- readxl::read_excel(path, sheet = "1.14",
                            col_names = FALSE, .name_repair = "minimal")
  col2 <- as.character(unlist(raw[, 2]))
  is_period <- grepl("^[0-9]{4}Q[1-4]$", col2)
  data_idx  <- which(is_period)
  data.frame(
    period = col2[data_idx],
    series = "Output gap",
    value  = suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, 3])))
    ),
    stringsAsFactors = FALSE
  )
}

#' List available EFO economy measures
#'
#' Returns a data frame of the economy measures available via
#' \code{\link{get_efo_economy}}, showing the `measure` name to pass and a
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
#' Forecast Tables — Aggregates file and returns the components of net
#' borrowing (Table 6.5) in tidy long format.
#'
#' Covers the five-year forecast horizon published at the most recent Budget
#' (OBR, March 2026).
#' Key series include current receipts, current expenditure, depreciation,
#' net investment, and net borrowing (PSNB).
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{fiscal_year}{Fiscal year being forecast, e.g. `"2025-26"` (character)}
#'   \item{series}{Component name, e.g. `"Net borrowing"` (character)}
#'   \item{value_bn}{Projected value in \enc{£}{GBP} billion (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' efo <- get_efo_fiscal()
#' efo[efo$series == "Net borrowing", ]
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_efo_fiscal <- function(refresh = FALSE) {
  parse_efo_fiscal(efo_aggregates_path(refresh))
}

#' Get EFO economy projections
#'
#' Downloads (and caches) the OBR \emph{Economic and Fiscal Outlook} Detailed
#' Forecast Tables — Economy file and returns quarterly economic projections
#' for a chosen measure in tidy long format.
#'
#' Data runs from 2008 Q1 through the current forecast horizon
#' (OBR, March 2026).
#' Use \code{\link{list_efo_economy_measures}} to see all available measures.
#'
#' @param measure Character. Which economy table to return. One of
#'   `"labour"`, `"inflation"`, or `"output_gap"`. Defaults to
#'   `"inflation"`.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{period}{Calendar quarter, e.g. `"2025Q1"` (character)}
#'   \item{series}{Variable name, e.g. `"CPI"` (character)}
#'   \item{value}{Value in units appropriate to the series (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' # CPI and RPI since 2008
#' inf <- get_efo_economy("inflation")
#' inf[inf$series == "CPI", ]
#'
#' # Labour market
#' lab <- get_efo_economy("labour")
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_efo_economy <- function(measure = "inflation", refresh = FALSE) {
  valid <- c("labour", "inflation", "output_gap")
  if (!measure %in% valid) {
    cli::cli_abort(c(
      "Unknown measure {.val {measure}}.",
      "i" = "Run {.fn list_efo_economy_measures} to see available options."
    ))
  }
  path <- efo_economy_path(refresh)
  if (measure == "output_gap") return(parse_efo_output_gap(path))
  sheet_map <- c(labour = "1.6", inflation = "1.7")
  parse_efo_economy_sheet(path, sheet_map[[measure]])
}
