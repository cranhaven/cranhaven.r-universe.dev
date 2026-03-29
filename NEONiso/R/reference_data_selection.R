# process_calibration_data
# functions to help select and filter validation data.

#' Select validation data corresponding to a particular day
#'
#' @param standard_df Input reference data.frame.
#' @param analyte Are we calibrating CO2 and H2O?
#' (Use argument 'co2' or 'h2o', or else function will throw error)
#' @param min_nobs Minimum number of high-frequency
#' observations to define a peak. If not supplied,
#' defaults are 200 for `analyte = 'co2'` or 30 for
#' `analyte = 'h2o'`
#'
#' @return Smaller data.frame where only the reference data selected
#' to use in the calibration routines is returned. Assumes that we are
#' calibrating on a daily basis, and not on a longer time scale. Data
#' are selected based on two criteria: cannot be missing, and must be
#' at least a certain number of high-frequency observations in order to
#' qualify as a valid measurement. For the water system, this function
#' also keeps only the last three injections for each reference water
#' per day.
#'
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @import dplyr
#'
select_daily_reference_data <- function(standard_df,
                                        analyte,
                                        min_nobs = NA) {

  # set default min_nobs based on analyte if not user supplied
  if (is.na(min_nobs)) {
    # set to 200 if co2, 30 if water.
    min_nobs <- ifelse(analyte == "co2", 200, 30)
  }

  if (analyte == "co2") {

    standard_df <- standard_df %>%
      # get day of month
      dplyr::mutate(date = lubridate::date(.data$timeBgn)) %>%
      dplyr::group_by(.data$date, .data$verticalPosition) %>%
      # check to make sure peak sufficiently long, then slice off single.
      dplyr::filter(.data$dlta13CCo2.numSamp > min_nobs |
                    is.na(.data$dlta13CCo2.numSamp)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"date") %>%
      dplyr::arrange(.data$timeBgn)

  } else if (analyte == "h2o") {

    standard_df <- standard_df %>%
    # get day of month
      dplyr::mutate(date = lubridate::date(.data$timeBgn)) %>%
      dplyr::group_by(.data$date, .data$verticalPosition) %>%
      dplyr::filter(.data$dlta18OH2o.numSamp > min_nobs | is.na(.data$dlta18OH2o.numSamp)) %>%
      dplyr::slice(tail(dplyr::row_number(), 3)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"date") %>%
      dplyr::arrange(.data$timeBgn)

  } else {

    stop("invalid analyte selected in select_reference_data. 
         please change to 'co2' or 'h2o'")

  }

  return(standard_df)

}
