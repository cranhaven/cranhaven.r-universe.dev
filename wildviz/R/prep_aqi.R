################################################################################
## Data: Air Quality System (AQS) API
## Source: AQS API details - https://aqs.epa.gov/aqsweb/documents/data_api.html
## Description: AQS contains ambient air sample data collected by state, local, tribal, and federal air pollution control agencies from thousands of monitors around the nation.
## Format: JSON
##
## Sample request for getting all states in the US
## https://aqs.epa.gov/data/api/list/states?email=test@aqs.api&key=test
##
## Sample request for getting all counties in California
## https://aqs.epa.gov/data/api/list/countiesByState?email=test@aqs.api&key=test&state=37
##
## Sample request for getting daily summaries for a county in California
## https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=88101&bdate=20160101&edate=20160228&state=37&county=183
################################################################################

#' Get state code from the state names.
#'
#' @note The function uses the AQS API to fetch the data from the API endpoints. Use the following service to register as a user:
#' A verification email will be sent to the email account specified. To register using the email address
#' create and request this link (Replace \email{myemail@@example.com} in the example with your email address.):
#' (\url{https://aqs.epa.gov/data/api/signup?email=myemail@example.com})
#' You then need to set the email and the key in the .Renviron file as follows, i.e.
#' aqs_api_email=\email{myemail@@example.com}
#' aqs_api_key=testkey1234
#'
#' @param aqs_api_email AQS API email
#' @param aqs_api_key AQS API key
#' @param state_names A vector containing state names.
#'
#' @return A vector of state code per state name provided.
#'
#' @examples
#' \dontrun{
#' aqs_api_email = Sys.getenv('aqs_api_email')
#' aqs_api_key = Sys.getenv('aqs_api_key')
#'
#' state_codes <- get_state_code(aqs_api_email = aqs_api_email,
#'                               aqs_api_key = aqs_api_key,
#'                               state_names = c('California', 'New York'))
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
get_state_code <- function(aqs_api_email, aqs_api_key, state_names) {
  # Define endpoint
  base_path <- 'https://aqs.epa.gov/data/api/'
  list_states_endpoint <- 'list/states'
  list_states_path <- paste(base_path, list_states_endpoint, sep = '')

  # Make a request to get the state codes for the provided states list
  states_request <- httr::GET(url = list_states_path,
                              query = list(
                                email = aqs_api_email,
                                key = aqs_api_key
                                )
                              )

  # Re-format JSON response as dataframe
  states_response <- httr::content(states_request, as = 'text', encoding = 'UTF-8')
  states_df <- jsonlite::fromJSON(states_response, flatten = TRUE) %>% data.frame()

  # Produce list of counties
  state_codes <- states_df %>%
    dplyr::filter(Data.value_represented %in% state_names) %>%
    dplyr::pull('Data.code')

  return(state_codes)
}

#' Get list of county FIPS codes by providing the state codes, i.e. California = '06'.
#'
#' @note The function uses the AQS API to fetch the data from the API endpoints. Use the following service to register as a user:
#' A verification email will be sent to the email account specified. To register using the email address
#' create and request this link (Replace \email{myemail@@example.com} in the example with your email address.):
#' (\url{https://aqs.epa.gov/data/api/signup?email=myemail@example.com})
#' You then need to set the email and the key in the .Renviron file as follows, i.e.
#' aqs_api_email=\email{myemail@@example.com}
#' aqs_api_key=testkey1234
#'
#' @note AQS AQI has request limits and ToS: (\url{https://aqs.epa.gov/aqsweb/documents/data_api.html#terms}). The function intentionally
#' adds a 10 second delay between each call (per year, per county) but pay attention to the limits as the account may be disabled.
#'
#' @param aqs_api_email AQS API email
#' @param aqs_api_key AQS API key
#' @param state_codes A vector of standard state codes according to the Federal Information Processing Standard Publication.
#'     This is an output of the \code{\link{get_state_code}} function.
#'
#' @return A dataframe of combined, 5 digit state + county FIPS codes ('fips') and their corresponding county names ('name').
#'
#' @examples
#' \dontrun{
#' aqs_api_email = Sys.getenv('aqs_api_email')
#' aqs_api_key = Sys.getenv('aqs_api_key')
#'
#' state_codes <- get_state_code(aqs_api_email = aqs_api_email,
#'                               aqs_api_key = aqs_api_key,
#'                               state_names = c('California', 'New York'))
#' counties <- get_counties(aqs_api_email = aqs_api_email,
#'                          aqs_api_key = aqs_api_key,
#'                          state_codes = state_codes)
#' }
#' @importFrom dplyr %>%
#'
#' @export
get_counties <- function(aqs_api_email, aqs_api_key, state_codes) {
  # Define endpoint
  base_path <- 'https://aqs.epa.gov/data/api/'
  list_counties_endpoint <- 'list/countiesByState'
  list_counties_path <- paste(base_path, list_counties_endpoint, sep = '')

  # Create an empty counties dataframe
  counties <- data.frame(fips = integer(), name = character())

  for (code in state_codes) {
    # Make a request to get the list of counties for state
    print(paste('Fetching counties for', code, '...'))

    counties_request <- httr::GET(url=list_counties_path,
                                  query=list(
                                    email=aqs_api_email,
                                    key=aqs_api_key,
                                    state=code
                                    )
                                  )

    # Re-format JSON response as dataframe
    counties_response <- httr::content(counties_request, as = 'text', encoding = 'UTF-8')
    counties_df <- jsonlite::fromJSON(counties_response, flatten = TRUE) %>% data.frame()

    # Produce dataframe of counties
    counties_df <- counties_df %>%
      dplyr::transmute(fips = paste(code, Data.code, sep = ''), name = Data.value_represented)

    # Append rows of counties to the dataframe
    counties <- rbind(counties, counties_df)

    Sys.sleep(10) # wait 10 seconds in between requests to avoid frequency limit
  }

  return(counties)
}

#' Retrieve the air quality data.
#'
#' @note The function uses the AQS API to fetch the data from the API endpoints. Use the following service to register as a user:
#' A verification email will be sent to the email account specified. To register using the email address
#' create and request this link (Replace \email{myemail@@example.com} in the example with your email address.):
#' (\url{https://aqs.epa.gov/data/api/signup?email=myemail@example.com})
#' You then need to set the email and the key in the .Renviron file as follows, i.e.
#' aqs_api_email=\email{myemail@@example.com}
#' aqs_api_key=testkey1234
#'
#' @note AQS AQI has request limits and ToS: (\url{https://aqs.epa.gov/aqsweb/documents/data_api.html#terms}). The function intentionally
#' adds a 10 second delay between each call (per year, per county) but pay attention to the limits as the account may be disabled.
#' It is also recommended to process no more than five years at a time, as the API request occasionally times out and data may be lost.
#'
#' @param aqs_api_email AQS API email
#' @param aqs_api_key AQS API key
#' @param fips_list A vector of FIPS codes per county. One can use the output from the \code{\link{get_counties}} function as follows:
#'    \code{fips_list=pull(counties, 'fips')}
#' @param metric_list list of codes that represent different air quality metrics
#'    81101 - PM10, 88101 - PM2.5, 42101 - CO, 42602 - NO2, 44201 - Ozone.
#' @param year_min earliest year to pull the air quality data for, starts January 1st.
#' @param year_max latest year to pull the air quality data for, ends December 31st.
#'
#' @return A dataframe of daily AQI data, including the metrics specified.
#'
#' @examples
#' \dontrun{
#' aqs_api_email = Sys.getenv("aqs_api_email")
#' aqs_api_key = Sys.getenv("aqs_api_key")
#'
#' state_codes <- get_state_code(aqs_api_email = aqs_api_email,
#'                               aqs_api_key = aqs_api_key,
#'                               state_names = c('California'))
#' counties <- get_counties(aqs_api_email = aqs_api_email,
#'                          aqs_api_key = aqs_api_key,
#'                          state_codes = state_codes)
#' aqi <- daily_aqi(aqs_api_email = aqs_api_email,
#'                  aqs_api_key = aqs_api_key,
#'                  fips_list = counties$fips,
#'                  year_min = 2015,
#'                  year_max = 2015)
#' }
#' @importFrom dplyr %>%
#'
#' @export
daily_aqi <- function(aqs_api_email, aqs_api_key, fips_list,
                      metric_list = c(81102, 88101, 42101, 44201, 42602), year_min = 2015, year_max = 2015) {
  # Define endpoint
  base_path <- 'https://aqs.epa.gov/data/api/'
  daily_summ_endpoint <- 'dailyData/byCounty'
  daily_summ_path <- paste(base_path, daily_summ_endpoint, sep = '')

  metrics <- paste(metric_list, collapse = ',')
  cols <- c('Carbon monoxide' = NA_real_, 'Ozone' = NA_real_, 'Nitrogen dioxide (NO2)' = NA_real_, 'PM2.5 - Local Conditions' = NA_real_, 'PM10 Total 0-10um STP' = NA_real_)
  first_flag <- 1

  for (year in year_min:year_max) {
    for (fips in fips_list) {
      print(paste('Processing', year, 'and', fips, '...'))

      # Parse the state code and the county code
      state = substr(fips, 1, 2)
      county = substr(fips, 3, 5)

      daily_summ_request <- httr::GET(url = daily_summ_path,
                                      query = list(
                                        email = aqs_api_email,
                                        key = aqs_api_key,
                                        param = metrics,
                                        bdate = paste(year, '0101', sep=''),
                                        edate = paste(year, '1231', sep=''),
                                        state = state,
                                        county = county
                                        )
                                      )

      daily_summ_response <- httr::content(daily_summ_request, as = 'text', encoding = 'UTF-8')

      if (length(jsonlite::fromJSON(daily_summ_response)) > 1) {
        if (jsonlite::fromJSON(daily_summ_response)$Header[4] != 0) {
        # Daily summaries per metric (long)
        daily_summ_df <- jsonlite::fromJSON(daily_summ_response, flatten = TRUE) %>% data.frame() %>%
          dplyr::select(state_code = 'Data.state_code', county_code = 'Data.county_code', county_name = 'Data.county', metric_code = 'Data.parameter_code',
                 metric = 'Data.parameter', standard = 'Data.pollutant_standard', date = 'Data.date_local', unit = 'Data.units_of_measure',
                 value = 'Data.arithmetic_mean', aqi = 'Data.aqi') %>%
          dplyr::filter(standard %in% c('CO 8-hour 1971', 'Ozone 8-hour 2015', 'NO2 Annual 1971', 'PM25 Annual 2012', 'PM10 24-hour 2006'))

        # Create aggregated daily summaries for each state, county, metric
        daily_summ_pivoted <- daily_summ_df %>%
          dplyr::group_by(state_code, county_code, county_name, metric, date) %>%
          dplyr::select(value, aqi) %>%
          dplyr::summarize(mean_aqi = mean(aqi), mean_value = mean(value)) %>%
          tidyr::pivot_wider(names_from = metric, values_from = mean_value) %>%
          tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% # add cols that do not exist to avoid error in the rename step
          dplyr::rename(co = 'Carbon monoxide', ozone = 'Ozone', no2 = 'Nitrogen dioxide (NO2)', pm25 = 'PM2.5 - Local Conditions', pm10 = 'PM10 Total 0-10um STP') %>%
          dplyr::group_by(state_code, county_code, county_name, date) %>%
          dplyr::summarize(aqi = mean(mean_aqi, na.rm = TRUE), co = mean(co, na.rm = TRUE), ozone = mean(ozone, na.rm = TRUE), no2 = mean(no2, na.rm = TRUE),
                    pm25 = mean(pm25, na.rm = TRUE), pm10 = mean(pm10, na.rm = TRUE))
        }
      }
      else {
        print(paste(year, 'and', fips, 'returned zero rows.'))
        Sys.sleep(10) # wait 10 seconds in between requests to avoid frequency limit
        next
      }

      if (first_flag == 1) {
        aqi_df <- daily_summ_pivoted
        first_flag <- 0
      }
      else {
        aqi_df <- rbind(aqi_df, daily_summ_pivoted)
      }

      Sys.sleep(10) # wait 10 seconds in between requests to avoid frequency limit
    }
  }

  return(aqi_df)
}
