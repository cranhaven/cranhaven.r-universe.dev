#' Screening Target Populations
#'
#' These functions subsets the Kenyan population to the desirable screening population.
#'
#' `get_cervical_target_population()` subsets the target population for cervical
#'   cancer screening: females aged between 25 years and 50 years
#'
#' `get_breast_cbe_target_population()` subsets the target population for clinical
#'   breast examination: females aged between 25 years and 74 years
#'
#' `get_breast_mammogram_target_population()` subsets the target population for
#' breast cancer screening through mammography: females aged between 40 years to 74 years
#'
#' `get_colorectal_target_population()` subsets the target population for
#' colorectal cancer screening: males and females aged between 45 years to 75 years
#'
#' @details
#' These target populations are guided by the
#' [Kenya National Cancer Screening Guidelines 2018](<https://www.iccp-portal.org/system/files/plans/KENYA%20NATIONAL%20CANCER%20CONTROL%20STRATEGY%202017-2022_1.pdf>).
#' The population projection for counties and the national level are calculated
#' based on population growth 2.2% obtained from the [Kenya National Bureau of Statistics](https://www.knbs.or.ke/).
#' The annual targets follows the guidance of screening guidelines and for cervical
#' cancer it is also guided by the WHO publication 'Planning and implementing
#' cervical cancer prevention programs: A manual for managers.'
#'
#' @param year Year for which to estimate population.
#' @param level The desired level of the organization unit hierarchy to retrieve
#'   data for: `"country"` (default) , `"county"` or `"subcounty"`.
#'
#' @return A tibble containing the target screening population
#'
#' * county     - name of the county. Optional if the level is county or subcounty
#' * subcounty  - name of the county. Optional if the level if subcounty
#' * target     - number to be screened
#'
#' @export
#'
#' @examples
#' # Get the country projection for cervical cancer screening for the year 2024
#' target_population <- get_cervical_target_population(2024)
#' target_population
#'
#' # Get the projection for cervical cancer screening for 2022 by county
#' target_population <- get_cervical_target_population(2022, level = 'county')
#' target_population
#'
#' # Get the projection for CBE for 2022 by county
#' target_population <- get_breast_cbe_target_population(2022, level = 'county')
#' target_population
#'
#' # Get the country projection of women to perform mammogram for the year 2024
#' target_population <- get_breast_mammogram_target_population(2024)
#' target_population
#'
#' # Get the country projection colorectal cancer screening for the year 2024
#' target_population <- get_colorectal_target_population(2024)
#' target_population
#'
#' @name target_population

get_cervical_target_population <- function(year, level = c('country', 'county', 'subcounty')) {

  check_numeric(year)

  year <- case_when(
    year <= 2025 ~ 2025,
    year > 2025 ~ 2030
  )

  population <- get_filtered_population(year, 25, 50, 0.7/5, level)

  return(population)
}

#' @rdname target_population
#' @return A tibble containing the target screening population
#' @export
get_breast_cbe_target_population <- function(year, level = c('country', 'county', 'subcounty')) {
  population <- .get_breast_target_population(year, min_age = 25, max_age = 75, level = level)
  return(population)
}

#' @rdname target_population
#' @return A tibble containing the target screening population
#' @export
get_breast_mammogram_target_population <- function(year, level = c('country', 'county', 'subcounty')) {
  population <- .get_breast_target_population(year, min_age = 40, max_age = 75, level = level)
  return(population)
}

#' @rdname target_population
#' @return A tibble containing the target screening population
#' @export
get_colorectal_target_population <- function(year, level = c('country', 'county', 'subcounty')) {

  check_numeric(year)

  year <- case_when(
    year < 2021 ~ 2021,
    year > 2030 ~ 2030,
    .default = year
  )

  colorectal_target_percent <- c(
    '2021' = 0.05, '2022' = 0.08, '2023' = 0.10,
    '2024' = 0.15, '2025' = 0.20, '2026' = 0.25,
    '2027' = 0.30, '2028' = 0.33, '2029' = 0.37, '2030' = 0.42
  )

  population <- get_filtered_population(year, min_age = 45, max_age = 75, modifier = colorectal_target_percent[toString(year)], level = level, pop_sex = 'both')

  return(population)
}


#' @rdname target_population
#' @noRd
.get_breast_target_population <- function(year, min_age, max_age = 75, level = c('country', 'county', 'subcounty')) {

  check_numeric(year)

  year <- case_when(
    year < 2021 ~ 2021,
    year > 2030 ~ 2030,
    .default = year
  )

  breast_target_percent <- c(
    '2021' = 0.05, '2022' = 0.08, '2023' = 0.10,
    '2024' = 0.15, '2025' = 0.20, '2026' = 0.25,
    '2027' = 0.30, '2028' = 0.33, '2029' = 0.37, '2030' = 0.42
  )

  population <- get_filtered_population(year, min_age, max_age, breast_target_percent[toString(year)], level)

  return(population)
}

#' Filters the Population
#'
#' `get_filtered_population()` filters the population based on age and level
#' and projects the population base on the year provided
#'
#' @param year The year to project the population
#' @param min_age The minimum age to include in the filtered data
#' @param max_age  The maximum age to include in the filtered data
#' @param modifier A multiplier that affect the population projection. Default 1
#' @param level The desired level of the organization unit hierarchy to retrieve
#'   data for: `"country"`, `"county"` or `"subcounty"`.
#' @param pop_sex The desired population sex: `"male"`, `"female"` (default), `"both"`
#' @param rate The population growth
#'
#' @return A tibble containing the target population
#'
#' @export
#'
#' @examples
#'
#' # Get the female population in 2022 aged 25-49 years
#' filtered_population <- get_filtered_population(2022, 25, 49, pop_sex = 'female')
#' filtered_population
#'
#'# Get 5% male population in 2022 aged 40-75 years
#' filtered_population <- get_filtered_population(2022, 40, 75, modifier = 0.05, pop_sex = 'male')
#' filtered_population

get_filtered_population <- function(year,
                                    min_age,
                                    max_age,
                                    modifier = 1,
                                    level = c('country', 'county', 'subcounty'),
                                    pop_sex = c('female', 'male', 'both'),
                                    rate = 0.022) {
  age = sex = NULL # due to NSE notes in R CMD check

  check_numeric(year)
  check_numeric(min_age)
  check_numeric(max_age)
  check_numeric(modifier)
  check_numeric(rate)

  level <- arg_match(level)
  level <- switch (level,
                   country = c('country'),
                   county = c('country', 'county'),
                   subcounty = c('country', 'county', 'subcounty'))

  pop_sex <- arg_match(pop_sex)
  pop_sex <- switch (pop_sex,
    female = c('female'),
    male = c('male'),
    both = c('female', 'male')
  )

  population <- population_data %>%
    filter(sex %in% pop_sex, age >= min_age, age < max_age)%>%
    group_by(across(any_of(level))) %>%
    summarise(
      target = .population_growth(sum(population, na.rm = TRUE), year, rate) * modifier
    )

  return(population)
}

#' Projects Population Growth
#'
#' `.population_growth()` function projects population growth based on the formula
#' `Nt = P * e^(r * t)`. The annual population growth rate, `r`, is set at 2.2% and
#' the years,`t`, calculated with the reference to the year 2020 year after the last
#' population census
#'
#' @param P Initial population size
#' @param year Year for which to estimate population
#' @param rate Annual growth rate
#'
#' @return Projected population size at the specified year
#'
#' @noRd

.population_growth <- function(P, year, rate = 0.022) {
  n <- year - 2020
  val <- P * floor(exp(rate * n) * 100) / 100
  return(val)
}
