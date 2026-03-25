check_numeric <- function(number, arg = caller_arg(number), error_call = caller_env()) {

  check_required(number, arg = arg, call = error_call)

  if (!(is_scalar_double(number) || is_scalar_integerish(number))) {
    cancerscreening_abort(
      c('x' = '{.arg {arg}} should be scalar numeric'),
      class = 'cancerscreening_invalid_numeric',
      call = error_call
    )
  }
}

strip_organisation_suffix <- function(.data, level) {

  kenya = county = sub_county = subcounty = county_assembly_ward = ward = health_facility = NULL

  if (level >= 1) {
    .data <- .data %>%
      rename(country = kenya)
  }

  if (level >= 2) {
    .data <- .data %>%
      mutate(county = str_remove(county, ' County'))
  }

  if (level >= 3) {
    .data <- .data %>%
      rename(subcounty = sub_county) %>%
      mutate(subcounty = str_remove(subcounty, ' Sub County'))
  }

  if (level >= 4) {
    .data <- .data %>%
      rename(ward = county_assembly_ward) %>%
      mutate(ward = str_remove(ward, ' Ward'))
  }

  if (level == 5) {
    .data <- .data %>%
      rename(facility = health_facility)
  }

  return(.data)
}

add_fiscal_year <- function(.data, fiscal_start = 7) {

  period = NULL

  check_numeric(fiscal_start)

  .data %>%
    mutate(
      quarter = as.integer(quarter(period, fiscal_start = fiscal_start, type = 'quarter')),
      quarter = factor(str_glue('Q{quarter}')),
      fiscal_year = as.integer(quarter(period, fiscal_start = fiscal_start, type='year.quarter')),
      fiscal_year = factor(str_glue('{fiscal_year-1}/{fiscal_year}'))
    )
}
