#' Retrieves Data for Clinical Breast Examinations (CBE) Conducted
#'
#' `get_breast_cbe()` retrieves data for CBE conducted within a specified
#' period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing data for CBE conducted with the following columns:
#'
#' * country    - Name of the country country
#' * county     - Name of the county. Optional if the level is `county`, `subcounty`, `ward` or `facility`.
#' * subcounty  - Name of the subcounty. Optional if the level is `subcounty`, `ward` or `facility`.
#' * ward       - Name of the ward. Optional if the level is `ward` or `facility`.
#' * facility   - Name of the health facility. Optional if the level `facility`.
#' * period     - The month and year of the data.
#' * fiscal_year- The financial year of the report(July-June Cycle).
#' * year       - The calendar year of the report.
#' * month      - The month name of the report.
#' * age_group  - The age group category of the report (25-34, 35-39, 40-55, 56-74, or 75+).
#' * category   - Additional category if available.
#' * element    - The data element.
#' * value      - The number reported.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#' # Download data from February 2023 to current date
#' cbe_data <- get_breast_cbe(start_date = '2023-02-01')
#' cbe_data

get_breast_cbe <- function(start_date,
                           end_date = NULL,
                           level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                           organisations = NULL,
                           ...) {

  category2 = NULL # due to NSE notes in R CMD check

  # Clinical Breast Examination data elements
  # XEX93uLsAm2 = CBE Abnormal
  # cXe64Yk0QMY = CBE Normal
  cbe_element_ids <- c('cXe64Yk0QMY', 'XEX93uLsAm2')

  data <- .get_breast_data(cbe_element_ids,
                           start_date,
                           end_date = end_date,
                           level = level,
                           organisations = organisations,
                           ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'Normal') ~ 'Normal',
        .default = 'Abnormal',
        .ptype = factor(levels = c('Normal', 'Abnormal'))
      )
    )

  return(data)
}
