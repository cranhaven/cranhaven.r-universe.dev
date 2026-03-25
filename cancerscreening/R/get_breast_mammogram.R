#' Retrieves Data for Mammograms Conducted
#'
#' `get_breast_mammogram()` retrieves data for mammograms conducted within a
#' specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing data for mammograms conducted with the following columns:
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
#' mammogram_data <- get_breast_mammogram(start_date = '2023-02-01')
#' mammogram_data

get_breast_mammogram <- function(start_date,
                                 end_date = NULL,
                                 level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                 organisations = NULL,
                                 ...) {

  # Mammogram screening element ids
  # T3crNg5D3Xa = Mammogram - BIRADS-0 to 3
  # Sorvgq7NDug = Mammogram - BIRADS-4
  # bi1ipJR6zNJ = Mammogram - BIRADS-5
  # APhWHU4KLWF = Mammogram - BIRADS-6
  mammogram_element_ids <- c('T3crNg5D3Xa', 'Sorvgq7NDug', 'bi1ipJR6zNJ', 'APhWHU4KLWF')

  data <- .get_breast_data(mammogram_element_ids,
                           start_date,
                           end_date = end_date,
                           level = level,
                           organisations = organisations,
                           ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'BIRADS-0 to 3') ~ 'BIRADS 0-3',
        str_detect(element, 'BIRADS-4') ~ 'BIRADS 4',
        str_detect(element, 'BIRADS-5') ~ 'BIRADS 5',
        str_detect(element, 'BIRADS-6') ~ 'BIRADS 6',
        .default = 'BIRADS 6',
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
