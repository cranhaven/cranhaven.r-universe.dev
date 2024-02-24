################################################################################
## Data: Daily Weather Summaries for US Counties
## Source: Daily weather summaries details from GHCND stations: https://www.ncdc.noaa.gov/cdo-web/search
## Description: Data from the Daily Global Historical Climatology Network (GHCN-Daily) through NOAA’s FTP server.
##     The data is archived at the National Centers for Environmental Information (NCEI) (formerly the National Climatic Data Center (NCDC)),
##     and spans from the 1800s to the current year.
## Format: Tibble
## Data documentation: https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
################################################################################

#' Retrieve average daily weather data per US county.
#'
#' @note The function uses the NOAA API to identify the weather monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page (\url{https://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using \code{options(noaakey = "your key")}, replacing "your key"
#'    with the API key you've requested from NOAA).
#'
#' @inheritParams daily_df
#' @inheritParams daily_stations
#'
#' @return A dataframe of daily weather data averaged across multiple stations.
#'
#' @examples
#' \dontrun{s
#' climate <- daily_climate(fips = '06001',
#'                          var = c('prcp', 'snow', 'snwd', 'tmax', 'tmin'),
#'                          date_min = '2015-01-01',
#'                          date_max = '2015-12-31',
#'                          coverage = 0.90)
#' }
#' @export
daily_climate <- function(fips, var = c('prcp', 'snow', 'snwd', 'tmax', 'tmin'), date_min = '2015-01-01', date_max = '2015-12-31', coverage = 0.90) {
  stations <- daily_stations(fips = fips, date_min = date_min, date_max = date_max)

  climate_data <- daily_df(stations = stations, var = var, date_min = date_min, date_max = date_max,
                           coverage = coverage, average_data = TRUE)

  return(climate_data$daily_data)
}

#' Retrieve average daily weather data for a list of US counties.
#'
#' @note The function uses the NOAA API to identify the weather monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page (\url{https://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using \code{options(noaakey = "your key")}, replacing "your key"
#'    with the API key you've requested from NOAA).
#'
#' @param fips_list A vector of FIPS codes of the counties to pull the daily weather data for.
#'    One can use the output from the \code{get_counties} function as follows:
#'    \code{fips_list=pull(counties, 'fips')}
#'
#' @inheritParams daily_climate
#'
#' @return A dataframe of daily weather data averaged across multiple stations for each county in list.
#'
#' @examples
#' \dontrun{
#' climate <- daily_climate_counties(fips_list = c('06001', '06003'),
#'                                   var = c('prcp', 'snow', 'snwd', 'tmax', 'tmin'),
#'                                   date_min = '2015-01-01',
#'                                   date_max = '2015-12-31',
#'                                   coverage = 0.90)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
daily_climate_counties <- function(fips_list, var = c('prcp', 'snow', 'snwd', 'tmax', 'tmin'),
                                   date_min = '2015-01-01', date_max = '2015-12-31', coverage = 0.90) {
  first_flag <- 1
  eval_str <- 'cols <- c('

  # Create cols using the variable vector provided
  for (i in 1:length(var)) {
    if (i < length(var)) {
      eval_str <- paste(eval_str, var[i], ' = NA_real_, ', sep = '')
    }
    else {
      eval_str <- paste(eval_str, var[i], ' = NA_real_)', sep = '')
    }
  }

  # Evaluate the col creation string
  eval(parse(text = eval_str))

  # For each fip code in fip_list, fetch the climate data and append to a dataframe
  for (fips in fips_list) {
    print(paste('Processing', fips, '...'))

    climate_daily <- daily_climate(fips = fips, var = var, date_min = date_min, date_max = date_max, coverage = coverage) %>%
      tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% # add cols that did not return from the data fetch
      dplyr::mutate(fips = fips) %>%
      dplyr::select('fips', 'date', one_of(var))

    if (first_flag == 1) {
      climate_df <- climate_daily
      first_flag <- 0
    }
    else {
      climate_df <- rbind(climate_df, climate_daily)
    }
  }

  return(climate_df)
}
