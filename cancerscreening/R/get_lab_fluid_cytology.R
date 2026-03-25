#' Retrieves the Fluid Cytology Data
#'
#' `get_lab_fluid_cytology()` retrieves fluid cytology lab data for a specified period
#' from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing fluid cytology lab data with the following columns:
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
#' * category   - The age group category of the report
#' * element    - The data element.
#' * value      - The number reported.
#'
#' @examplesIf khis_has_cred()
#'
#' # Download data from February 2023 to current date
#' data <- get_lab_fluid_cytology(start_date = '2023-02-01')
#' data
#'
#' @export

get_lab_fluid_cytology <- function(start_date,
                                   end_date = NULL,
                                   level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                   organisations = NULL,
                                   ...) {

  element = category = NULL # due to NSE notes in R CMD check

  fluid_cytology_ids <- c('XpRDEf5Qdsu', 'yHkftZnekcx', 'Lk9KZvx3xkW', 'ZIgUukxYsBF')
  data <- get_analytics_formatted(fluid_cytology_ids,
                                  start_date = start_date,
                                  end_date = end_date,
                                  level = level,
                                  organisations = organisations,
                                  ...) %>%
    mutate(
      element = str_remove(element, 'MOH 706_'),
      element = factor(element),
      category = factor(category),
      source = 'MOH 706'
    )
  return(data)
}
