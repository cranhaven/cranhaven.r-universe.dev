################################################################################
## Data: 1.88 Million US Wildfires - 24 years of geo-referenced wildfire records (1992 - 2015)
## Source: https://www.kaggle.com/rtatman/188-million-us-wildfires
## Description: This data publication contains a spatial database of wildfires that occurred in the United States from 1992 to 2015.
## Format: SQLite db
################################################################################

#' Create US wildfires dataframe from the Kaggle US Wildfire SQLite db
#'
#' @note The function relies on the SQLite db available on Kaggle: (\url{https://www.kaggle.com/rtatman/188-million-us-wildfires})
#'     Unfortunately, Kaggle API does not support R at this time. Download the '188-million-us-wildfires.zip' file,
#'     and provide the path to the function as db_name, i.e. \code{db_name = 'data-raw/FPA_FOD_20170508.sqlite'}
#'
#' @param db_name File path of the SQLite wildfire database.
#' @param state_abbrev Abbreviations of the states to retrieve the wildfire data for.
#' @param cols Columns to select from the wildfire database. For more info: (\url{https://www.kaggle.com/rtatman/188-million-us-wildfires})
#' @param year_min earliest year to pull the air quality data for, starts January 1st.
#' @param year_max latest year to pull the air quality data for, ends December 31st.
#'
#' @return A dataframe of wildfires that occurred in the United States.
#'
#' @examples
#' \dontrun{
#' fires <- create_wildfire(db_name = 'data-raw/FPA_FOD_20170508.sqlite',
#'                          state_abbrev = c('CA', 'NY'),
#'                          cols=c('FIRE_NAME', 'DISCOVERY_DATE', 'CONT_DATE',
#'                                 'STAT_CAUSE_DESCR', 'FIRE_SIZE', 'FIRE_SIZE_CLASS',
#'                                 'LATITUDE', 'LONGITUDE', 'STATE', 'FIPS_CODE', 'FIPS_NAME'),
#'                          year_min = 1992,
#'                          year_max = 2015)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
create_wildfire <- function(db_name, state_abbrev = NULL,
                            cols=c('FIRE_NAME', 'DISCOVERY_DATE', 'CONT_DATE', 'STAT_CAUSE_DESCR', 'FIRE_SIZE', 'FIRE_SIZE_CLASS',
                                   'LATITUDE', 'LONGITUDE', 'STATE', 'FIPS_CODE', 'FIPS_NAME'),
                            year_min = 1992, year_max = 2015) {
  # Connect to the wildfire SQLite db
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = db_name)

  # List all tables in db
  tables <- DBI::dbListTables(con)

  # Create a dataframe from the Fires table
  if (length(state_abbrev) == 0) {
    select_query = 'SELECT * FROM Fires'
  }
  else {
    select_query = paste('SELECT * FROM Fires
                         WHERE STATE IN (', paste(as.character(paste0('"', state_abbrev, '"')), collapse=', '), ')', sep='')
  }

  wildfires_res <- DBI::dbSendQuery(conn = con, statement = select_query)
  wildfires_df <- DBI::dbFetch(wildfires_res)

  # Clear query result and disconnect from db
  DBI::dbClearResult(wildfires_res)
  DBI::dbDisconnect(con)

  # Prepare the wildfire tibble

  # Basic manipulation - filter rows with COUNTY = NA and format the DISCOVERY_DATE
  wildfires <- wildfires_df %>%
    dplyr::filter(!is.na(COUNTY)) %>%
    dplyr::mutate(DISCOVERY_DATE = as.Date(DISCOVERY_DATE, origin = structure(-2440588, class = 'Date')),
           CONT_DATE = as.Date(CONT_DATE, origin = structure(-2440588, class = 'Date'))) %>%
    dplyr::filter((format(DISCOVERY_DATE, '%Y') >= year_min) & (format(DISCOVERY_DATE, '%Y') <= year_max)) %>% # filter by year_min and year_max
    dplyr::select(all_of(cols))

  return(wildfires)
}
