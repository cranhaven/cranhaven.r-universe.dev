#' Retrieves Reporting Metrics for Screening Tool
#'
#' `get_screening_reporting_analytics()` It fetches the reporting metrics for the
#' screening tool (MOH 745).
#'
#' @param start_date The start date to retrieve data. It is required and in the
#'   format `YYYY-MM-dd`.
#' @param end_date The ending date for data retrieval (default is the current date).
#' @param level The desired data granularity: `"country"` (the default), `"county"`,
#'   `"subcounty"`, `"ward"`, or `"facility"`.
#' @param organisations A list of organization units ids to be filtered.
#' @param ... Other options that can be passed onto KHIS API.
#'
#' @return A tibble with the reporting metrics.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Download screening metric from February 2023 to current date
#' data <- get_screening_reporting_analytics(start_date = '2023-02-01')
#' data

get_screening_reporting_analytics <- function(start_date,
                                              end_date = NULL,
                                              level = c('country', 'county', 'subcounty', 'ward', 'facility'),
                                              organisations = NULL,
                                              ...) {

  dataset_id = 'WWh5hbCmvND'

  level <- arg_match(level)
  ou_level <- switch (level,
                      country = 1,
                      county = 2,
                      subcounty = 3,
                      ward = 4,
                      facility = 5)

  data <- get_data_sets_by_level(dataset_ids = dataset_id,
                                 start_date = start_date,
                                 end_date = end_date,
                                 level = ou_level,
                                 org_ids = organisations,
                                 ...)
  data %>%
    strip_organisation_suffix(ou_level) %>%
    add_fiscal_year()

}
