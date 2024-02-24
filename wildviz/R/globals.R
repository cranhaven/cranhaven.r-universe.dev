# Global variable for package wildviz
utils::globalVariables(names = c(".",
                                 "CONT_DATE",
                                 "COUNTY",
                                 "DISCOVERY_DATE",
                                 "Data.code",
                                 "Data.value_represented",
                                 "all_of",
                                 "aqi",
                                 "co",
                                 "cols",
                                 "county_code",
                                 "county_name",
                                 "mean_aqi",
                                 "mean_value",
                                 "metric",
                                 "no2",
                                 "one_of",
                                 "ozone",
                                 "pm10",
                                 "pm25",
                                 "standard",
                                 "state_code",
                                 "value"))

wildviz_defaults  <- list(db_name = 'data-raw/FPA_FOD_20170508.sqlite',
                          aqs_api_email = Sys.getenv("aqs_api_email"),
                          aqs_api_key = Sys.getenv("aqs_api_key"),
                          year_min = 2001,
                          year_max = 2015)

#' Function to reset defaults
#'
#' @param year_min minimum year to process for datasets
#' @param year_max minimum year to process for datasets
#' @param db_name file path for the wildfires SQLite database
#' @param aqs_api_email API email for the AQS API
#' @param aqs_api_key API key for the AQS API
#'
#' @return a list of default values invisibly
#'
#' @examples
#' resetDefaults()
#'
#' @export
resetDefaults <- function(db_name = 'data-raw/FPA_FOD_20170508.sqlite',
                          aqs_api_email = Sys.getenv("aqs_api_email"),
                          aqs_api_key = Sys.getenv("aqs_api_key"),
                          year_min = 2001,
                          year_max = 2015) {
  wildviz_defaults  <- as.list(environment())

  invisible(wildviz_defaults)
}

#' Set the default param names to values
#'
#' @param name the parameter name
#' @param value the value to assign
#'
#' @return the new list of defaults invisibly
#'
#' @examples
#' setDefaults('year_min', 2001)
#'
#' @export
setDefaults <- function(name, value) {
  # Check if param name is known
  if (!(name %in% names(wildviz_defaults))) {
    stop(sprintf("Bad parameter name %s", name))
  }
  wildviz_defaults[[name]] <- value

  invisible(wildviz_defaults)
}

# Load the defaults when package is loaded
.onLoad <- function(libname, pkgname) {
  # Set default options
  options('noaakey' = Sys.getenv('noaakey'))
}

# Unset the defaults when package is unloaded
.onUnload <- function(libname, pkgname) {
  options('noaakey' = NULL) # unset the noaakey option
  wildviz_defaults <- NULL
}
