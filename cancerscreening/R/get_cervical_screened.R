#' Retrieves Cervical Cancer Screening Data
#'
#' `get_cervical_screened()` retrieves cervical cancer screening data for a
#'    specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing cervical cancer screening data with the following columns:
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
#' * category   - The age group category of the report (<25, 25-49, 50+).
#' * category2  - Additional category if available.
#' * element    - The data element (HPV, VIA or Pap Smear).
#' * source     - The source report (MOH 711 or MOH 745).
#' * value      - The number reported.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#' # Download data from February 2023 to current date
#' screened <- get_cervical_screened(start_date = '2023-02-01')
#' screened

get_cervical_screened <- function(start_date,
                                  end_date = NULL,
                                  level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                  organisations = NULL,
                                  ...) {

  ## Cervical Cancer Screening
  # VR7vdS7P0Gb = Number of clients who received HPV Test
  # gQro1y7Rsbq = Number of clients who received Pap smear
  # rFtB3keaVWm = Number of clients who received VIA or VIA/ VILI Screening
  # ommbnTANmGo = MOH 711 Clients screened for Cervical Cancer using HPV test
  # kl4RvWOGb7x = MOH 711 Clients screened  using Pap smear
  # G9COyloYLYa = MOH 711 Clients screened using VIA /VILI /HPV VILI / HPV
  cacx_screening_ids <- c(
    'VR7vdS7P0Gb', 'gQro1y7Rsbq', 'rFtB3keaVWm',
    'ommbnTANmGo', 'kl4RvWOGb7x', 'G9COyloYLYa'
  )

  data <- .get_cervical_data(cacx_screening_ids,
                             start_date,
                             end_date = end_date,
                             level = level,
                             organisations = organisations,
                             ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'VIA') ~ 'VIA',
        str_detect(element, 'HPV') ~ 'HPV',
        str_detect(element, 'Pap') ~'Pap Smear',
        .ptype = factor(levels = c('VIA', 'HPV', 'Pap Smear'))
      )
    )

  return(data)
}
