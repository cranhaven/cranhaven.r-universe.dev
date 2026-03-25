#' Retrieves Analytics Table Data from KHIS
#'
#' `get_analytics_formatted()` fetches data from the KHIS analytics data tables
#'   for a given period and data element(s), without performing any aggregation.
#'
#' @param element_ids A vector of data element IDs for which to retrieve data. Required.
#' @param start_date The start date to retrieve data. It is required and in the format `YYYY-MM-dd`.
#' @param end_date The ending date for data retrieval (default is the current date).
#' @param level The desired data granularity: `"country"` (the default), `"county"`, `"subcounty"`, `"ward"`, or `"facility"`.
#' @param organisations A list of organization units ids to be filtered.
#' @param ... Other options that can be passed onto KHIS API.
#'
#' @details
#' * Retrieves data directly from KHIS analytics tables.
#' * Supports optional arguments for providing organization lists, data elements, and categories.
#' * Allows specifying KHIS session objects, retry attempts, and logging verbosity.
#'
#' @return A tibble with detailed information, including:
#'
#' * Geographical identifiers (country, county, subcounty, ward, facility, depending on level)
#' * Reporting period (month, year, fiscal year)
#' * Data element names
#' * Category options
#' * Reported values
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#' # Clinical Breast Examination data elements
#' # XEX93uLsAm2 = CBE Abnormal
#' # cXe64Yk0QMY = CBE Normal
#' element_id = c('cXe64Yk0QMY', 'XEX93uLsAm2')
#'
#' # Download data from February 2023 to current date
#' data <- get_analytics_formatted(element_ids = element_id,
#'                                 start_date = '2023-02-01')
#' data

get_analytics_formatted <- function(element_ids,
                                    start_date,
                                    end_date = NULL,
                                    level = c('country', 'county', 'subcounty', 'ward', 'facility'),
                                    organisations = NULL,
                                    ...) {

  period = NULL # due to NSE notes in R CMD check

  level <- arg_match(level)

  ou_level <- switch (level,
                      country = 1,
                      county = 2,
                      subcounty = 3,
                      ward = 4,
                      facility = 5)

  data <- get_analytics_by_level(element_ids = element_ids,
                                 start_date = start_date,
                                 end_date = end_date,
                                 level = ou_level,
                                 org_ids = organisations,
                                 ...)

  data %>%
    strip_organisation_suffix(ou_level) %>%
    add_fiscal_year()
}
