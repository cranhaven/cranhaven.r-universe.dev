# Catalogue of every detailed-forecast table the package can fetch from the
# EFO Aggregates and EFO Economy workbooks. Each row says where the table
# lives, what shape it has, and what units / metric type to default to.
#
# Layouts:
#   quarterly_wide   - cols: blank, period (e.g. "2008Q1"), one column per
#                      series. Headers may span multiple rows.
#   quarterly_single - cols: blank, period, single value. Used for one-series
#                      tables like Nominal GDP (1.4), output gap (1.14).
#   annual_year_wide - cols: blank, blank, year columns ("2020", "2021"...).
#                      Series names in col 2, values in cols 3+.
#   fiscal_year_wide - cols: blank, blank, fiscal-year columns ("2024-25"...).
#                      Series names in col 2 (flat or with "of which:" sub-
#                      indentation in col 3+). The parser flattens hierarchy
#                      so each row carries the full path as the series name.
#   cross_reference  - sheet exists but redirects to a previous EFO. Returns
#                      NULL (with an informative warning) rather than data.

efo_catalogue_table <- function() {
  # Hand-curated metadata. The unit / metric_type values follow OBR's own
  # sheet-title units; the parser may override on a per-row basis when a
  # series name strongly signals otherwise (e.g. "deflator", "Index").
  rows <- list(
    # ------------------------------------------------------------------
    # Economy workbook (sections 1.x)
    # ------------------------------------------------------------------
    list("1.1",  "economy", "GDP",        "GDP expenditure components (chain-linked volumes)",          "quarterly_wide",   "level",   "gbp_bn"),
    list("1.2",  "economy", "GDP",        "GDP expenditure components (current prices)",                "quarterly_wide",   "level",   "gbp_bn"),
    list("1.3",  "economy", "GDP",        "GDP income components",                                      "quarterly_wide",   "level",   "gbp_bn"),
    list("1.4",  "economy", "GDP",        "Nominal GDP (non-seasonally adjusted)",                      "quarterly_single", "level",   "gbp_bn"),
    list("1.5",  "economy", "GDP",        "Per capita (age 16+)",                                       "quarterly_wide",   "level",   NA_character_),
    list("1.6",  "economy", "Labour",     "Labour market",                                              "quarterly_wide",   NA,        NA_character_),
    list("1.7",  "economy", "Inflation",  "Inflation",                                                  "quarterly_wide",   "yoy_pct", "pct"),
    list("1.8",  "economy", "Trade",      "Balance of payments",                                        "quarterly_wide",   "level",   NA_character_),
    list("1.9",  "economy", "Markets",    "Market-derived assumptions (rates, FX, oil, equities)",      "quarterly_wide",   "level",   NA_character_),
    list("1.10", "economy", "Sectors",    "Financial balances by sector",                               "quarterly_wide",   "pct",     "pct"),
    list("1.11", "economy", "Households", "Household balance sheets and lending",                       "quarterly_wide",   "level",   NA_character_),
    list("1.11b","economy", "Households", "Household balance sheet (unsecured debt)",                   "quarterly_wide",   "level",   NA_character_),
    list("1.12", "economy", "Households", "Household disposable income",                                "quarterly_wide",   "level",   NA_character_),
    list("1.13", "economy", "Wages",      "National Minimum Wage and National Living Wage (annual)",    "annual_year_wide", "level",   "gbp_per_hour"),
    list("1.14", "economy", "Cycle",      "OBR central estimate of the output gap",                    "quarterly_single", "pct",     "pct"),
    list("1.15", "economy", "Cycle",      "Potential output forecast",                                  "quarterly_wide",   "level",   NA_character_),
    list("1.16", "economy", "Housing",    "Housing market",                                             "quarterly_wide",   "level",   NA_character_),
    list("1.17", "economy", "Households", "Household debt-servicing costs",                             "quarterly_wide",   "level",   NA_character_),
    list("1.18", "economy", "Housing",    "Eligible rent growth assumptions",                           "fiscal_year_wide",   "yoy_pct", "pct"),
    list("1.19", "economy", "Inflation",  "CPI category inflation",                                     "quarterly_wide",     "yoy_pct", "pct"),
    list("1.19b","economy", "Inflation",  "CPI category weights",                                       "annual_period_wide", "level",   "weight"),
    list("1.20", "economy", "Markets",    "Electricity price forecast",                                 "quarterly_single", "level",   "pence_per_mwh"),
    # ------------------------------------------------------------------
    # Aggregates workbook (sections 6.x)
    # ------------------------------------------------------------------
    list("6.1",  "aggregates", "Expenditure", "Breakdown of expenditure forecast by sector and economic category", "fiscal_year_wide", "level", "gbp_bn"),
    list("6.2",  "aggregates", "Receipts",    "Breakdown of receipts forecast by sector and economic category",    "fiscal_year_wide", "level", "gbp_bn"),
    list("6.3",  "aggregates", "Aggregates",  "General government transactions by economic category",              "fiscal_year_wide", "level", "gbp_bn"),
    list("6.4",  "aggregates", "Aggregates",  "Public sector transactions by sub-sector and economic category",    "subsector_matrix", "level", "gbp_bn"),
    list("6.5",  "aggregates", "Aggregates",  "Components of net borrowing",                                        "fiscal_year_wide", "level", "gbp_bn"),
    list("6.6",  "aggregates", "APF",         "Asset Purchase Facility annual runoff assumptions",                  "fiscal_year_wide", "level", "gbp_bn"),
    list("6.7",  "aggregates", "ONS",         "Inconsistencies between OBR forecasts and ONS outturns",             "fiscal_year_wide", "level", "gbp_bn"),
    list("6.8",  "aggregates", "Students",    "Student entrant borrowers forecast",                                 "fiscal_year_wide", "level", NA_character_),
    list("6.9",  "aggregates", "Students",    "Net flow of student loans and repayments",                           "fiscal_year_wide", "level", "gbp_bn"),
    list("6.10", "aggregates", "Students",    "Proxy for prevailing market rates (student loan interest cap)",      "quarterly_indented", "pct", "pct"),
    list("6.11", "aggregates", "Debt",        "Public sector net debt year-on-year changes",                        "cross_reference",  NA,      NA_character_),
    list("6.12", "aggregates", "Debt",        "Total gross financing",                                              "fiscal_year_wide", "level", "gbp_bn"),
    list("6.13", "aggregates", "Debt",        "Composition of public sector net debt",                              "fiscal_year_wide", "pct",   "pct"),
    list("6.14", "aggregates", "Debt",        "Composition of public sector net worth",                             "fiscal_year_wide", "level", "gbp_bn"),
    list("6.15", "aggregates", "Debt",        "Reconciliation of PSNCR and CGNCR",                                  "cross_reference",  NA,    NA_character_),
    list("6.16", "aggregates", "Debt",        "Central government debt interest by financing component",           "fiscal_year_wide", "level", "gbp_bn"),
    list("6.17", "aggregates", "Debt",        "Outstanding stocks, debt interest payments and effective rates",     "fiscal_year_wide", "level", "gbp_bn")
  )

  cols <- c("table_id", "file", "section", "title",
            "layout", "default_metric_type", "default_unit")
  out <- as.data.frame(do.call(rbind, lapply(rows, function(r) {
    stats::setNames(as.data.frame(r, stringsAsFactors = FALSE), cols)
  })))
  rownames(out) <- NULL
  out
}

#' List the EFO detailed-forecast tables this package can fetch
#'
#' Returns a data frame describing every Detailed Forecast Table in the OBR
#' Economic and Fiscal Outlook (Aggregates and Economy workbooks) that
#' [get_efo_table()] knows how to parse.
#'
#' @details
#' Use this catalogue to discover which tables are available, what each
#' contains, and the default `metric_type` / `unit` `get_efo_table()` will
#' attach. Pass any `table_id` to [get_efo_table()].
#'
#' Coverage: 17 fiscal aggregates tables (Section 6) plus 22 macro economy
#' tables (Section 1). One sheet (6.11) is currently a cross-reference to
#' a previous EFO and returns `NULL` with a warning rather than data.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{table_id}{The EFO table identifier (e.g. `"6.5"`, `"1.7"`).}
#'   \item{file}{Which EFO workbook the table sits in: `"aggregates"` or
#'     `"economy"`.}
#'   \item{section}{Theme tag (e.g. `"GDP"`, `"Labour"`, `"Debt"`).}
#'   \item{title}{Human-readable title taken from the OBR contents page.}
#'   \item{layout}{Layout family the parser uses: `"quarterly_wide"`,
#'     `"quarterly_single"`, `"annual_year_wide"`, `"fiscal_year_wide"`,
#'     or `"cross_reference"`.}
#'   \item{default_metric_type}{Default `metric_type` applied to series
#'     whose name does not signal otherwise. `NA` lets the heuristic decide
#'     per row.}
#'   \item{default_unit}{Default `unit` for the same.}
#' }
#'
#' @examples
#' head(obr_efo_catalogue())
#'
#' # All tables in the Debt section
#' cat <- obr_efo_catalogue()
#' cat[cat$section == "Debt", c("table_id", "title")]
#'
#' @family EFO
#' @export
obr_efo_catalogue <- function() {
  efo_catalogue_table()
}

# Internal: look up a single row of the catalogue by table id.
efo_catalogue_lookup <- function(table_id) {
  cat <- efo_catalogue_table()
  hit <- cat[cat$table_id == table_id, , drop = FALSE]
  if (nrow(hit) == 0L) {
    cli::cli_abort(c(
      "Unknown EFO table id {.val {table_id}}.",
      "i" = "Run {.fn obr_efo_catalogue} to see all known tables."
    ))
  }
  hit[1L, ]
}
