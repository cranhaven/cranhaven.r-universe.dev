# July 2025 Fiscal Risks and Sustainability: executive summary charts and tables
FSR_DATE          <- "July 2025"
FSR_EXEC_URL      <- "https://obr.uk/download/july-2025-fiscal-risks-and-sustainability-charts-and-tables-executive-summary/"
FSR_EXEC_FILENAME <- "fsr_executive_summary.xlsx"

fsr_exec_path <- function(refresh = FALSE) {
  obr_fetch(FSR_EXEC_URL, FSR_EXEC_FILENAME, refresh = refresh)
}

# Parse C1.2: state pension spending scenarios.
# The sheet has two separate sections, each beginning with a section-header row
# whose col 2 names the section and whose cols 3+ contain fiscal year labels.
# Data rows within each section have a scenario name in col 2 and projected
# values (% of GDP) in the same column positions as the fiscal years.
parse_pension_projections <- function(path) {
  raw  <- readxl::read_excel(path, sheet = "C1.2",
                             col_names = FALSE, .name_repair = "minimal")
  col2 <- as.character(unlist(raw[, 2]))

  section_labels <- c("Demographic scenarios", "Triple lock scenarios")
  section_rows   <- which(col2 %in% section_labels)
  if (length(section_rows) == 0L) return(NULL)

  result_list <- list()

  for (s_idx in seq_along(section_rows)) {
    s_row   <- section_rows[s_idx]
    sec_name <- col2[s_row]

    end_row <- if (s_idx < length(section_rows)) {
      section_rows[s_idx + 1L] - 1L
    } else {
      nrow(raw)
    }

    # Fiscal years sit in the section-header row itself (cols 3+)
    yr_vals   <- as.character(unlist(raw[s_row, ]))
    year_cols <- which(grepl("^[0-9]{4}-[0-9]{2}$", yr_vals))
    if (length(year_cols) == 0L) next
    fiscal_years <- yr_vals[year_cols]

    # Data rows: non-NA in col 2, within this section, excluding the header row
    for (i in (s_row + 1L):end_row) {
      if (i > nrow(raw)) break
      nm <- col2[i]
      if (is.na(nm) || nm == "") next
      vals <- suppressWarnings(
        as.numeric(as.character(unlist(raw[i, year_cols])))
      )
      if (all(is.na(vals))) next
      result_list[[length(result_list) + 1L]] <- data.frame(
        scenario_type = sec_name,
        scenario      = nm,
        fiscal_year   = fiscal_years,
        pct_gdp       = vals,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(result_list) == 0L) return(NULL)
  do.call(rbind, result_list)
}

#' Get long-run state pension spending projections
#'
#' Downloads (and caches) the OBR Fiscal Risks and Sustainability Report
#' executive summary charts and tables workbook and returns 50-year
#' projections for state pension spending as a share of GDP, under
#' alternative demographic and triple-lock uprating scenarios.
#'
#' This data is unique to the Fiscal Risks and Sustainability Report
#' (OBR, July 2025)
#' and is not available in any other OBR publication. It illustrates how
#' ageing demographics and pension uprating rules interact to determine the
#' long-run cost of the state pension.
#'
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{scenario_type}{Either `"Demographic scenarios"` or
#'     `"Triple lock scenarios"` (character)}
#'   \item{scenario}{Scenario name, e.g. `"Central projection"`,
#'     `"Higher life expectancy"` (character)}
#'   \item{fiscal_year}{Fiscal year, e.g. `"2030-31"` (character)}
#'   \item{pct_gdp}{State pension spending as a percentage of GDP (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' proj <- get_pension_projections()
#'
#' # Central demographic projection over the 50-year horizon
#' central <- proj[proj$scenario_type == "Demographic scenarios" &
#'                 proj$scenario == "Central projection", ]
#' tail(central, 10)
#'
#' # How much more expensive is 'higher life expectancy' vs central?
#' dem <- proj[proj$scenario_type == "Demographic scenarios", ]
#' options(op)
#' }
#'
#' @family long-term fiscal
#' @export
get_pension_projections <- function(refresh = FALSE) {
  parse_pension_projections(fsr_exec_path(refresh))
}
