#' Retrieve and Format Colorectal Cancer Screening Data
#'
#' `.get_colorectal_data()` retrieves colorectal cancer screening data for a specified period
#' from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing colorectal cancer screening data with the following columns:
#'
#' * country    - Name of the country.
#' * county     - Name of the county. Optional if the level is `county`, `subcounty`, `ward` or `facility`.
#' * subcounty  - Name of the subcounty. Optional if the level is `subcounty`, `ward` or `facility`.
#' * ward       - Name of the ward. Optional if the level is `ward` or `facility`.
#' * facility   - Name of the health facility. Optional if the level `facility`.
#' * period     - The month and year of the data.
#' * fiscal_year- The financial year of the report(July-June Cycle).
#' * year       - The calendar year of the report.
#' * month      - The month name of the report.
#' * category   - The age group category of the report (25-34, 35-39, 40-55, 56-74, or 75+).
#' * category2  - Additional category if available.
#' * element    - The data element.
#' * value      - The number reported.
#'
#' @examplesIf khis_has_cred()
#'
#' # FOBT screening element ids
#' w46XSyvQyYb = FOBT-Negative
#' qojd0pB1cqG = FOBT-Positive
#' fobt_element_ids <- c('w46XSyvQyYb', 'qojd0pB1cqG')
#'
#' # Download data from February 2023 to current date
#' data <- .get_colorectal_data(element_ids = element_id, start_date = '2023-02-01')
#'
#' @noRd

.get_colorectal_data <- function(element_ids,
                            start_date,
                            end_date = NULL,
                            level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                            organisations = NULL,
                            ...) {

  category = NULL # due to NSE notes in R CMD check

  data <- get_analytics_formatted(element_ids,
                                  start_date = start_date,
                                  end_date = end_date,
                                  level = level,
                                  organisations = organisations,
                                  ...) %>%
    mutate(
      category = case_when(
        str_detect(category, '45-54') ~ '45-54',
        str_detect(category, '55-64') ~ '55-64',
        str_detect(category, '65-75') ~ '65-75',
        .default = NA,
        .ptype = factor(levels = c('45-54', '55-64', '65-75'))
      ),
      source = 'MOH 745'
    ) %>%
    rename(age_group = category)

  return(data)
}
