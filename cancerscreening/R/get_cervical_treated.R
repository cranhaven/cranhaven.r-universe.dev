#' Retrieves Cervical Cancer Precancerous Treatment Data
#'
#' `get_cervical_treated()` retrieves cervical cancer precancerous treatment
#'   data for a specified period from the KHIS API server.
#'
#' @inheritParams get_analytics_formatted
#'
#' @return A tibble containing cervical cancer precancerous treatment data with the following columns:
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
#' treated <- get_cervical_treated(start_date = '2023-02-01')
#' treated

get_cervical_treated <- function(start_date,
                                 end_date = NULL,
                                 level =c('country', 'county', 'subcounty', 'ward', 'facility'),
                                 organisations = NULL,
                                 ...) {

  # Yv6LiN65lCJ = Number of clients treated using Cryotherapy
  # uXi8AjF8YR0 = Number of clients treated using LEEP
  # MIQ3HgFlHnS = Number of clients treated using Thermocoagulation
  # lx4fx2bluTm = Number of other treatment given (e.g. Hysterectomy, cone biopsy)
  # UAbmyzuI2UE = MOH 711 Cervical cancer clients treated using Cryotherapy
  # TSlyElHZw9d = MOH 711 Cervical cancer treated using LEEP
  cacx_treatment_ids <- c(
    'Yv6LiN65lCJ', 'uXi8AjF8YR0', 'MIQ3HgFlHnS', 'lx4fx2bluTm',
    'UAbmyzuI2UE', 'TSlyElHZw9d'
  )

  data <- .get_cervical_data(cacx_treatment_ids,
                             start_date,
                             end_date = end_date,
                             level = level,
                             organisations = organisations,
                             ...) %>%
    mutate(
      element = case_when(
        str_detect(element, 'LEEP') ~ 'LEEP',
        str_detect(element, 'Cryotherapy') ~ 'Cryotherapy',
        str_detect(element, 'Thermocoagulation') ~ 'Thermoablation',
        .default = 'Other',
        .ptype = factor(levels = c('Cryotherapy', 'Thermoablation', 'LEEP', 'Other'))
      )
    )

  return(data)
}
