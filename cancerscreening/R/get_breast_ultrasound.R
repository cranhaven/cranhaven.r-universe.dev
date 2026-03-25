#' Retrieves Data for Breast Ultrasound Conducted
#'
#' `get_breast_ultrasound()` retrieves data for breast ultrasounds conducted within a
#' specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing data for breast ultrasound conducted with the following columns:
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
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Download data from February 2023 to current date
#' ultrasound_data <- get_breast_ultrasound(start_date = '2023-02-01')
#' ultrasound_data

get_breast_ultrasound <- function(start_date,
                                 end_date = NULL,
                                 level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                 organisations = NULL,
                                 ...) {

  # Ultrasound screening element ids
  # MmffJIuHxFm = Ultrasound - BIRADS -0- to 3
  # cLlPY6DlvW7 = Ultrasound - BIRADS -4
  # NPiyy6QSi7I = Ultrasound - BIRADS -5
  # mJrNfjiiNDE = Ultrasound - BIRADS -6
  ultrasound_element_ids <- c('MmffJIuHxFm', 'cLlPY6DlvW7', 'NPiyy6QSi7I', 'mJrNfjiiNDE')

  data <- .get_breast_data(ultrasound_element_ids,
                           start_date,
                           end_date = end_date,
                           level = level,
                           organisations = organisations,
                           ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'BIRADS -0- to 3') ~ 'BIRADS 0-3',
        str_detect(element, 'BIRADS -4') ~ 'BIRADS 4',
        str_detect(element, 'BIRADS -5') ~ 'BIRADS 5',
        str_detect(element, 'BIRADS -6') ~ 'BIRADS 6',
        .default = NA,
        .ptype = factor(levels = c('BIRADS 0-3', 'BIRADS 4', 'BIRADS 5', 'BIRADS 6'))
      ),
      category = case_when(
        str_detect(element, 'BIRADS 0-3') ~ 'Normal',
        .default = 'Abnormal',
        .ptype = factor(levels = c('Normal', 'Abnormal'))
      )
    )

  return(data)
}
