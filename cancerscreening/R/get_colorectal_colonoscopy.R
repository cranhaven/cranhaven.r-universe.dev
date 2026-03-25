#' Retrieves Data for Colorectal Screening using Colonoscopy
#'
#' `get_colorectal_colonoscopy()` retrieves data for colorectal screening using
#' colonoscopy within a specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing data for colorectal screening with the following columns:
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
#' * category   - The age group category of the report (45-54, 55-64, or 65-75).
#' * category2  - Additional category if available.
#' * element    - The data element.
#' * value      - The number reported.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Download data from February 2023 to current date
#' data <- get_colorectal_colonoscopy(start_date = '2023-02-01')
#' data

get_colorectal_colonoscopy <- function(start_date,
                                 end_date = NULL,
                                 level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                 organisations = NULL,
                                 ...) {

  # Colonoscopy screening element ids
  # DtbPQS4KWbo = Colonoscopy cancer
  # tp8HEHpqx3T = Colonoscopy No abnormality
  # zt7cJpfaFSx = Colonoscopy other abnormalities
  # lqoWj3bLJtY = Colonoscopy Polyps
  colonoscopy_element_ids <- c('DtbPQS4KWbo', 'tp8HEHpqx3T', 'zt7cJpfaFSx', 'lqoWj3bLJtY')

  data <- .get_colorectal_data(colonoscopy_element_ids,
                               start_date,
                               end_date = end_date,
                               level = level,
                               organisations = organisations,
                               ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'cancer') ~ 'Cancer',
        str_detect(element, 'No abnormality') ~ 'No Abnormality',
        str_detect(element, 'other abnormalities') ~ 'Other Abnormalities',
        str_detect(element, 'Polyps') ~ 'Polyps',
        .ptype = factor(levels = c('No Abnormality', 'Polyps', 'Cancer', 'Other Abnormalities'))
      )
    )

  return(data)
}
