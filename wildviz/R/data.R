#' Geo-referenced California wildfire records from 2011 through 2015
#'
#' A dataframe containing Calfornia wildfire records at the county level.
#' It is created from the Kaggle US wildfire records data by filtering on
#' the date (2011- 2015), state (CA), and key cols and cleaning up the
#' missing counties and reformatting the dates.
#'
#' This data publication contains a spatial database of wildfires that
#' occurred in the United States from 1992 to 2015.
#'
#' @format A dataframe with 31579 rows and 11 variables:
#' \describe{
#'   \item{FIRE_NAME}{Name of the incident, from the fire report (primary) or ICS-209 report (secondary)}
#'   \item{DISCOVER_DATE}{Date on which the fire was discovered or confirmed to exist}
#'   \item{CONT_DATE}{Date on which the fire was declared contained or otherwise controlled}
#'   \item{STAT_CAUSE_DESCR}{Description of the (statistical) cause of the fire}
#'   \item{FIRE_SIZE}{Estimate of acres within the final perimeter of the fire}
#'   \item{FIRE_SIZE_CLASS}{Code for fire size based on the number of acres within the final fire
#'                          perimeter expenditures (A=greater than 0 but less than or equal to 0.25 acres,
#'                          B=0.26-9.9 acres, C=10.0-99.9 acres, D=100-299 acres, E=300 to 999 acres,
#'                          F=1000 to 4999 acres, and G=5000+ acres)}
#'   \item{LATITUDE}{Latitude (NAD83) for point location of the fire (decimal degrees)}
#'   \item{LONGITUDE}{Longitude (NAD83) for point location of the fire (decimal degrees)}
#'   \item{STATE}{Two-letter alphabetic code for the state in which the fire burned}
#'   \item{FIPS_CODE}{Three-digit code from the FIPS publication 6-4 for counties}
#'   \item{FIPS_NAME}{County name from the FIPS publication 6-4 for counties}
#' }
#' @source \url{https://www.kaggle.com/rtatman/188-million-us-wildfires}
"wildfires"

#' Air quality data records for California from 2011 through 2015
#'
#' A dataframe containing Calfornia air quality records at the county level.
#' It is created from the EPA's Air Quality System (AQS) API by fetching the
#' AQI along with other atmospheric measurements like CO, Ozone, NO2, PM2.5,
#' and PM10 and reforrmatting the tibble as a tidy data consisting of a row
#' per county per day.
#'
#' AQS contains ambient air sample data collected by state, local, tribal, and
#' federal air pollution control agencies from thousands of monitors around the nation.
#'
#' @format A dataframe with 102752 rows and 10 variables:
#' \describe{
#'   \item{state_code}{Two-digit code from the FIPS publication 6-4 for states}
#'   \item{county_code}{Three-digit code from the FIPS publication 6-4 for counties}
#'   \item{county_name}{County name from the FIPS publication 6-4 for counties}
#'   \item{date}{Date on which the air quality measures were recorded}
#'   \item{aqi}{AQI level}
#'   \item{co}{Carbon monoxide, in Parts per million}
#'   \item{ozone}{Ozone, in Parts per million}
#'   \item{no2}{Nitrogen dioxide (NO2), in Parts per billion}
#'   \item{pm25}{PM2.5, in Micrograms/cubic meter (LC)}
#'   \item{pm10}{PM10, in Micrograms/cubic meter (25 C)}
#' }
#' @source \url{https://aqs.epa.gov/aqsweb/documents/data_api.html}
"aqi"

#' Daily weather summaries for California from 2011 through 2015
#'
#' A dataframe containing daily weather summaries for California at the county level.
#' It is created from summarizing the measurements from GHCND stations to the daily level
#' and by filtering on the date (2011 - 2015), state (CA), and key cols.
#'
#' Data from the Daily Global Historical Climatology Network (GHCN-Daily) through the NOAA FTP server.
#' The data is archived at the National Centers for Environmental Information (NCEI)
#' (formerly the National Climatic Data Center (NCDC)), and spans from the 1800s to the current year.
#'
#' @format A dataframe with 105850 rows and 7 variables:
#' \describe{
#'   \item{fips}{Five-digit code combining the state and county codes from the FIPS publication 6-4 for counties}
#'   \item{date}{Date on which the climate measures were recorded}
#'   \item{prcp}{precipitation, in mm}
#'   \item{snow}{snowfall, in mm}
#'   \item{snwd}{snow depth, in	mm}
#'   \item{tmax}{maximum temperature, in degrees Celsius}
#'   \item{tmin}{minumum temperature, in degrees Celsius}
#' }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf}
"climate"

#' Master dataset combining the wildfires, AQI, and climate data for California from 2011 through 2015
#'
#' The master dataframe takes each wildfire occurrence in California
#' between 2011 through 2015, and joins the AQI and climate data for 30 days
#' prior to the DISCOVERY_DATE (fire discovery date) and 30 days post
#' CONT_DATE (fire contained date). The data is utilized by the Shiny app
#' to create dashboards.
#'
#' @format A dataframe with 1105972 rows and 23 variables:
#' \describe{
#'   \item{FIRE_NAME}{Name of the incident, from the fire report (primary) or ICS-209 report (secondary)}
#'   \item{DISCOVER_DATE}{Date on which the fire was discovered or confirmed to exist}
#'   \item{CONT_DATE}{Date on which the fire was declared contained or otherwise controlled}
#'   \item{STAT_CAUSE_DESCR}{Description of the (statistical) cause of the fire}
#'
#'   \item{FIRE_SIZE}{Estimate of acres within the final perimeter of the fire}
#'   \item{FIRE_SIZE_CLASS}{Code for fire size based on the number of acres within the final fire
#'                          perimeter expenditures (A=greater than 0 but less than or equal to 0.25 acres,
#'                          B=0.26-9.9 acres, C=10.0-99.9 acres, D=100-299 acres, E=300 to 999 acres,
#'                          F=1000 to 4999 acres, and G=5000+ acres)}
#'   \item{LATITUDE}{Latitude (NAD83) for point location of the fire (decimal degrees)}
#'   \item{LONGITUDE}{Longitude (NAD83) for point location of the fire (decimal degrees)}
#'   \item{STATE}{Two-letter alphabetic code for the state in which the fire burned}
#'   \item{FIPS_NAME}{County name from the FIPS publication 6-4 for representation of counties}
#'   \item{fips}{Five-digit code combining the state and county codes from the FIPS publication 6-4 for counties}
#'   \item{clim_date}{Date on which the climate measures were recorded}
#'   \item{prcp}{precipitation, in mm}
#'   \item{snow}{snowfall, in mm}
#'   \item{snwd}{snow depth, in	mm}
#'   \item{tmax}{maximum temperature, in degrees Celsius}
#'   \item{tmin}{minumum temperature, in degrees Celsius}
#'   \item{aqi}{AQI level}
#'   \item{co}{Carbon monoxide, in Parts per million}
#'   \item{ozone}{Ozone, in Parts per million}
#'   \item{no2}{Nitrogen dioxide (NO2), in Parts per billion}
#'   \item{pm25}{PM2.5, in Micrograms/cubic meter (LC)}
#'   \item{pm10}{PM10, in Micrograms/cubic meter (25 C)}
#' }
"master"
