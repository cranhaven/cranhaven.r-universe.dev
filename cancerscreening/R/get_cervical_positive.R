#' Retrieves Cervical Cancer Screening Data with Positive Results
#'
#' `get_cervical_positive()` retrieves cervical cancer screening data with positive results
#'   for a specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing cervical cancer screening data with positive results
#'   with the following columns:
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
#' positive <- get_cervical_positive(start_date = '2023-02-01')
#' positive

get_cervical_positive <- function(start_date,
                                  end_date = NULL,
                                  level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                  organisations = NULL,
                                  ...) {

  # wYHt86csbhn = Number of clients with Positive Cytology result
  # KLQZDP0ycOY = Number of clients with Positive HPV result
  # xBjAMa2KFwD = Number of clients with Positive VIA or VIA/VILI result
  # La4v1gAs5cp = Number of clients with suspicious cancer lesions
  # xbERCTpWTwi = MOH 711 Cervical cancer clients with Positive Cytology result
  # LI2g0vO0xvx = MOH 711 Cervical cancer clients with Positive HPV result
  # dBdw7Inlq2C = MOH 711 Cervical cancer clients with Positive VIA/VILI result
  # FC5BbFDsdCa = MOH 711 Clients with suspicious cancer lesion
  cacx_positive_ids <- c(
    'wYHt86csbhn', 'KLQZDP0ycOY', 'xBjAMa2KFwD', 'La4v1gAs5cp',
    'xbERCTpWTwi', 'LI2g0vO0xvx', 'dBdw7Inlq2C', 'FC5BbFDsdCa'
  )

  data <- .get_cervical_data(cacx_positive_ids,
                             start_date,
                             end_date = end_date,
                             level = level,
                             organisations = organisations,
                             ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'VIA') ~ 'VIA',
        str_detect(element, 'HPV') ~ 'HPV',
        str_detect(element, 'Cytology') ~'Pap Smear',
        .default = 'Suspicious Lesion',
        .ptype = factor(levels = c('VIA', 'HPV', 'Pap Smear', 'Suspicious Lesion'))
      )
    )

  return(data)
}
