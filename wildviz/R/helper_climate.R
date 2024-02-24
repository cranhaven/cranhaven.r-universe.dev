################################################################################
## Title: countyweather: Compiles Meterological Data for U.S. Counties
## Author: Rachel Severson [aut, cre], Brooke Anderson [aut]
## Date: 2016-10-26
## Code version: 0.1.0
## Availability: https://cran.r-project.org/web/packages/countyweather/index.html
## Description: The package builds on functions from the rnoaa package to identify weather stations within a county based on its FIPS code
##    and then pull weather data for a specified date range from those weather stations. It then does some additional cleaning and aggregating
##    to produce a single, county-level weather dataset.
## 
## Disclaimer: We had to separate out the basic functions from the countyweather package and make some modifications,
## for a couple of reasons:
##    1. Google Maps API that the package utilizes requires an API key as of mid-2018. This is not necessary for our package,
## hence it would be a burden for the package user to create an account to get their own API key to use the package.
##    2. The package has errors for edge cases when ran as-is. Therefore, we only took the functions we need and made some bug fixes to make it functional.
################################################################################

#' Return average daily weather data for a particular county.
#'
#' Returns a list with data on weather and stations for a selected county.
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which pull weather data from all relevant stations in a county.
#' This function filters and averages data returned by \code{rnoaa} functions
#' across all weather stations in a county based on user-specified
#' coverage specifications.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{https://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @param stations A dataframe containing station metadata, returned from
#'    the function \code{daily_stations}.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors. The
#'    default is to include all monitors with any available data (i.e.,
#'    \code{coverage = 0}).)
#' @param var A character vector specifying desired weather variables. For
#'    example, \code{var = c("tmin", "tmax", "prcp")} for maximum temperature,
#'    minimum temperature, and precipitation. The default is \code{"all"},
#'    which includes all available weather variables at any weather station in
#'    the county. For a full list of all
#'    possible variable names, see NOAA's README file for the Daily Global
#'    Historical Climatology Network (GHCN-Daily) at
#'    \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}. Many of
#'    the weather variables are available for some, but not all, monitors, so
#'    your output from this function may not include all the variables
#'    specified using this argument. If you specify a variable here but it is
#'    not included in the output dataset, it means that it was not available in
#'    the time range for any monitor in the county.
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors. If you choose
#'    FALSE, the function will return a dataframe with separate entries for
#'    each monitor, while TRUE (the default) outputs a single estimate
#'    for each day in the dataset, giving the average value of the weather
#'    metric across all available monitors in the county that day.
#' @inheritParams daily_stations
#'
#' @return A list with two elements. \code{daily_data} is a dataframe of daily
#'    weather data averaged across multiple monitors and includes columns
#'    (\code{"var"_reporting}) for each weather variable showing the number of
#'    stations contributing to the average for that variable on that day.
#'    The element \code{station_df} is a dataframe of station metadata for each
#'    station contributing weather data. A weather station will have one row per
#'    weather variable to which it contributes data. In addition to information
#'    such as station id, name, latitude, and longitude, the \code{station_df}
#'    dataframe includes statistical information about weather values
#'    contributed by each station for each weather variable. These statistics
#'    include \code{calc_coverage} (the percent of non-missing values for each
#'    station-weather variable combination for the specified date range),
#'    \code{standard_dev} (standard deviation), \code{max}, and \code{min},
#'    (giving the minimum and maximum values), and \code{range}, giving the
#'    range of values in each station-weather variable combination. The
#'    element \code{radius} is the calculated radius within which stations were
#'    pulled from the county's center. Elements \code{lat_center} and
#'    \code{lon_center} are the latitude and longitude of the county's center.
#'
#' @examples
#' \dontrun{
#' stations <- daily_stations(fips = "12086", date_min = "2010-01-01",
#'                            date_max = "2010-02-01")
#' fips_list <- daily_df(stations = stations, coverage = 0.90,
#'                  var = c("tmax", "tmin", "prcp"),
#'                  date_min = "2010-01-01", date_max = "2010-02-01")
#' averaged_data <- fips_list$daily_data
#' head(averaged_data)
#' station_info <- fips_list$station_df
#' head(station_info)
#' }
#' @export
daily_df <- function(stations, coverage = NULL, var = "all", date_min = NULL,
                     date_max = NULL, average_data = TRUE) {
  
  # get tidy full dataset for all monitors
  quiet_pull_monitors <- purrr::quietly(rnoaa::meteo_pull_monitors)
  meteo_df <- quiet_pull_monitors(monitors = stations$id,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = toupper(var))$result
  
  # calculate coverage for each weather variable
  coverage_df <- rnoaa::meteo_coverage(meteo_df, verbose = FALSE)
  
  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df,
                              coverage = coverage)
  good_monitors <- unique(filtered$id)
  
  # filter weather dataset based on stations with specified coverage
  filtered_data <- dplyr::filter_(meteo_df, ~ id %in% good_monitors)
  
  # steps to filter out erroneous data from individual stations
  # precipitation
  if ("prcp" %in% var & !is.null(filtered_data$prcp)) { # 2020/03/12 - @dshchung: added additional check to not fail on non-existing prcp column
    filtered_data$prcp <- filtered_data$prcp / 10
    if (max(filtered_data$prcp, na.rm = TRUE) > 1100) {
      bad_prcp <- which(with(filtered_data, prcp > 1100))
      filtered_data <- filtered_data[-bad_prcp,]
    }
  }
  
  # snowfall
  if ("snow" %in% var & !is.null(filtered_data$snow)) { # 2020/03/12 - @dshchung: added additional check to not fail on non-existing snow column
    if(max(filtered_data$snow, na.rm = TRUE) > 1600) {
      bad_snow <- which(with(filtered_data, snow > 1600))
      filtered_data <- filtered_data[-bad_snow,]
    }
  }
  
  # snow depth
  if ("snwd" %in% var & !is.null(filtered_data$snwd)) { # 2020/03/12 - @dshchung: added additional check to not fail on non-existing snwd column
    if (max(filtered_data$snwd, na.rm = TRUE) > 11500) {
      bad_snwd <- which(with(filtered_data, snwd > 11500))
      filtered_data <- filtered_data[-bad_snwd,]
    }
  }
  
  # tmax
  if ("tmax" %in% var & !is.null(filtered_data$tmax)) { # 2020/03/12 - @dshchung: added additional check to not fail on non-existing tmax column
    filtered_data$tmax <- filtered_data$tmax / 10
    if (max(filtered_data$tmax, na.rm = TRUE) > 57) {
      bad_tmax <- which(with(filtered_data, tmax > 57))
      filtered_data <- filtered_data[-bad_tmax,]
    }
  }
  
  # tmin
  if ("tmin" %in% var & !is.null(filtered_data$tmin)) { # 2020/03/12 - @dshchung: added additional check to not fail on non-existing tmin column
    filtered_data$tmin <- filtered_data$tmin / 10
    if (min(filtered_data$tmin, na.rm = TRUE) < -62) {
      bad_tmin <- which(with(filtered_data, tmin < -62))
      filtered_data <- filtered_data[-bad_tmin,]
    }
  }
  
  all_cols <- colnames(filtered_data)
  not_vars <- c("id", "date")
  g_cols <- all_cols[!all_cols %in% not_vars]
  
  group_cols <- c("id", "key")
  
  stats <- filtered_data %>%
    dplyr::select_(quote(-date)) %>%
    tidyr::gather_(key_col = "key", value_col = "value", gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = group_cols) %>%
    dplyr::summarize_(standard_dev = ~ sd(value, na.rm = TRUE),
                      min = ~ min(value, na.rm = TRUE),
                      max = ~ max(value, na.rm = TRUE),
                      range = ~ max - min)
  
  filtered <- dplyr::filter_(filtered, ~ id %in% good_monitors)
  stats <- dplyr::full_join(stats, filtered, by = c("id", "key"))
  
  stations <- dplyr::filter_(stations, ~ id %in% good_monitors)
  
  stations <- dplyr::full_join(stats, stations, by = "id") %>%
    dplyr::select_(quote(id), quote(name), quote(key), quote(latitude),
                   quote(longitude), quote(calc_coverage), quote(standard_dev),
                   quote(min), quote(max), quote(range))
  
  colnames(stations)[3] <- "var"
  
  if (average_data == TRUE) {
    filtered_data <- ave_daily(filtered_data)
  }
  
  out <- list("daily_data" = filtered_data, "station_df" = stations)
  
  return(out)
  
}

#' NOAA NCDC station IDs per county.
#'
#' Returns a dataframe with NOAA NCDC station IDs for
#' a single U.S. county. This function has options to filter stations based on
#' maximum and minimum dates, as well as percent data coverage.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{https://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @param fips A string with the five-digit U.S. FIPS code of a county
#'    in numeric, character, or factor format.
#' @param date_min A string with the desired starting date in character, ISO
#'    format ("yyyy-mm-dd"). The dataframe returned will include only stations
#'    that have data for dates including and after the specified date.
#' @param date_max A string with the desired ending date in character, ISO
#'    format ("yyyy-mm-dd"). The dataframe returned will include only stations
#'    that have data for dates up to and including the specified date.
#'
#' @return A dataframe with NOAA NCDC station IDs for a single U.S. county.
#'
#' @examples
#' \dontrun{
#' stations_36005 <- daily_stations("36005")
#' stations_36005
#'
#' miami_stations <- daily_stations("12086", date_min = "1999-01-01",
#'                                  date_max = "2012-12-31")
#' miami_stations
#' }
#'
#' @importFrom dplyr %>%
#' @export
daily_stations <- function(fips, date_min = NULL, date_max = NULL) {
  
  FIPS <- paste0('FIPS:', fips)
  station_ids <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = FIPS,
                                      limit = 10)
  
  station_df <- station_ids$data
  if (station_ids$meta$totalCount > 10) {
    how_many_more <- station_ids$meta$totalCount - 10
    more_stations <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                          locationid = FIPS,
                                          limit = how_many_more,
                                          offset = 10 + 1)
    station_df <- rbind(station_df, more_stations$data)
  }
  
  # If either `min_date` or `max_date` option was null, set to a date that
  # will keep all monitors in the filtering.
  if (is.null(date_max)) {
    date_max <- min(station_df$maxdate)
  }
  if (is.null(date_min)) {
    date_min <- max(station_df$mindate)
  }
  
  date_max <- lubridate::ymd(date_max)
  date_min <- lubridate::ymd(date_min)
  
  tot_df <- dplyr::mutate_(station_df,
                           mindate = ~ lubridate::ymd(mindate),
                           maxdate = ~ lubridate::ymd(maxdate)) %>%
    dplyr::filter_(~ maxdate >= date_min & mindate <= date_max) %>%
    dplyr::select_(.dots = c("id", "latitude", "longitude", "name")) %>%
    dplyr::mutate_(id = ~ gsub("GHCND:", "", id))
  
  return(tot_df)
}

#' Average daily weather data across multiple stations.
#'
#' Returns a dataframe with daily weather averaged across
#' stations, as well as columns showing the number of stations contributing
#' to the average for each variable and each day.
#' @return A dataframe of daily weather averaged across weather stations
#' @param weather_data A dataframe with daily weather observations. This
#'    dataframe is returned from the \code{rnoaa} function
#'    \code{meteo_pull_monitors}.
#'
#' @importFrom dplyr %>%
ave_daily <- function(weather_data) {
  
  all_cols <- colnames(weather_data)
  not_vars <- c("id", "date")
  g_cols <- all_cols[!all_cols %in% not_vars]
  
  #not sure about -id -date cols - how to do NSE here
  averaged_data <- tidyr::gather_(weather_data, key_col = "key",
                                  value_col = "value",
                                  gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "mean") %>%
    dplyr::ungroup()
  
  n_reporting <- tidyr::gather_(weather_data, key_col = "key",
                                value_col = "value",
                                gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")
  
  averaged_data <- dplyr::left_join(averaged_data, n_reporting,
                                    by = "date")
  return(averaged_data)
}

#' Filter stations based on "coverage" requirements.
#'
#' Filters available weather stations based on a specified required minimum
#' coverage (i.e., percent non-missing daily observations). Weather stations
#' with non-missing data for fewer days than specified by \code{coverage} will
#' be excluded from the county average.
#'
#' @param coverage_df A dataframe as returned by the \code{meteo_coverage}
#'    function in the \code{rnoaa} package
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors).
#'
#' @return A dataframe with stations that meet the specified coverage
#'    requirements for weather variables included in the \code{coverage_df}
#'    dataframe passed to the function.
#'
#' @importFrom dplyr %>%
filter_coverage <- function(coverage_df, coverage = 0) {
  
  if (is.null(coverage)) {
    coverage <- 0
  }
  
  all_cols <- colnames(coverage_df)
  not_vars <- c("id", "start_date", "end_date", "total_obs")
  g_cols <- all_cols[!all_cols %in% not_vars]
  
  filtered <- dplyr::select_(coverage_df,
                             .dots = list("-start_date", "-end_date",
                                          "-total_obs")) %>%
    tidyr::gather_(key_col = "key", value_col = "covered",
                   gather_cols = g_cols)  %>%
    dplyr::filter_(~ covered >= coverage) %>%
    dplyr::mutate_(covered_n = ~ 1) %>%
    dplyr::group_by_(.dots = list("id")) %>%
    dplyr::mutate_(good_monitor = ~ sum(!is.na(covered_n)) > 0) %>%
    dplyr::ungroup() %>%
    dplyr::filter_(~ good_monitor) %>%
    dplyr::select_(.dots = list("-good_monitor", "-covered_n"))
  
  colnames(filtered)[3] <- "calc_coverage"
  
  return(filtered)
}