#' inkaR: Access the INKAR Database of the BBSR
#'
#' The inkaR package provides a user-friendly interface to access, search, and download
#' statistical indicators from the INKAR (Indikatoren und Karten zur Raum- und Stadtentwicklung)
#' database provided by the Federal Institute for Research on Building, Urban Affairs and
#' Spatial Development (BBSR).
#'
#' @section Main Functions:
#'
#' * [view_indicators()]: Opens the list of available indicators in a viewer.
#'   Supports German ("de") and English ("en") modes. Sorted by active status.
#' * [get_inkar_data()]: API wrapper to fetch data for a specific indicator.
#'   Supports auto-saving to CSV (`csv = TRUE`).
#' * [get_geographies()]: Helper to list available spatial levels (e.g., Kreise, Gemeinden)
#'   or regions within a level.
#' * [search_indicators()]: Search for indicators by text pattern.
#'
#' @section Usage Workflow:
#'
#' 1. **Explore**: Use `view_indicators()` to find the `Shortname` (e.g., "001") or `M_ID` of the desired indicator.
#' 2. **Download**: Use `get_inkar_data("001", level="KRE", year=2021)` or ranges (`year=2010:2020`) to fetch the data.
#' 3. **Export**: Add `csv = TRUE` to `get_inkar_data` to save the result immediately.
#'
#' @examples
#' if (interactive()) {
#'   # 1. View available indicators (German)
#'   view_indicators()
#'
#'   # 2. View in English
#'   view_indicators("en")
#' }
#' 
#' \donttest{
#'   # 3. Search for "GDP" (Bruttoinlandsprodukt)
#'   try(search_indicators("GDP", lang = "en"))
#'
#'   # 4. Download data for GDP (011) for Districts (KRE)
#'   #    Note: You can use Shortnames ("011"), numeric M_IDs (11), or simple codes ("bip")
#'   try(data <- get_inkar_data("011", level = "KRE", year = 2021, lang = "de", csv = FALSE))
#'
#'   # 5. Download and save directly as CSV
#'   try(get_inkar_data("011", csv = TRUE, export_dir = tempdir()))
#'
#'   # 6. Download data for a year range
#'   try(get_inkar_data("011", level = "KRE", year = 2010:2020))
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats setNames
#' @importFrom rlang .data
## usethis namespace: end
NULL
