## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  db_name = 'data-raw/FPA_FOD_20170508.sqlite'

## ----eval = FALSE-------------------------------------------------------------
#  options(noaakey = "your key")

## ----eval = FALSE-------------------------------------------------------------
#  aqs_api_email=myemail@example.com
#  aqs_api_key=testkey1234

## ----eval = FALSE-------------------------------------------------------------
#  fires_df <- create_wildfire(db_name = 'data-raw/FPA_FOD_20170508.sqlite',
#                              state_abbrev = c('CA', 'NY'),
#                              cols=c('FIRE_NAME', 'DISCOVERY_DATE', 'CONT_DATE', 'STAT_CAUSE_DESCR',
#                                     'FIRE_SIZE', 'FIRE_SIZE_CLASS', 'LATITUDE', 'LONGITUDE',
#                                     'STATE', 'FIPS_CODE', 'FIPS_NAME'),
#                              year_min = 1992,
#                              year_max = 2015)

## ----eval = FALSE-------------------------------------------------------------
#  climate <- daily_climate_counties(fips_list = c('06001', '06003'),
#                                    var_list = c('prcp', 'snow', 'snwd', 'tmax', 'tmin'),
#                                    date_min = '2015-01-01',
#                                    date_max = '2015-12-31',
#                                    coverage = 0.90)

## ----eval = FALSE-------------------------------------------------------------
#  aqs_api_email = Sys.getenv("aqs_api_email")
#  aqs_api_key = Sys.getenv("aqs_api_key")
#  
#  state_codes <- get_state_code(aqs_api_email = aqs_api_email,
#                                aqs_api_key = aqs_api_key,
#                                state_names = c('California'))
#  counties <- get_counties(aqs_api_email = aqs_api_email,
#                           aqs_api_key = aqs_api_key,
#                           state_codes = state_codes)
#  aqi <- daily_aqi(aqs_api_email = aqs_api_email,
#                   aqs_api_key = aqs_api_key,
#                   fips_list = counties$fips,
#                   year_min = 2015,
#                   year_max = 2015)

