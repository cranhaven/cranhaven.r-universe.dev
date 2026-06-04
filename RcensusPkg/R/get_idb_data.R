#' @title get_idb_data
#'
#' @description Get data from the Census Bureau's Time Series International Database
#'   for a specific dataset: *Single Year of Age and Sex*("1year") or *5-Year Age Groups and Sex*("5year"),
#'   year, variables, and country. See Census Bureau's publicly available
#'  \href{https://www.census.gov/data/developers/data-sets/international-database.html}{International Database} for
#'  dataset descriptions.
#'
#' @details
#' For the "1year", a \code{data.table} is returned with the following columns:
#' \tabular{ll}{
#'    \strong{GEO_ID} \tab char - geo id string\cr
#'    \strong{NAME} \tab char - name of the country\cr
#'    \strong{GENC} \tab char - GENC country abbreviation\cr
#'    \strong{POP} \tab char - the total population for a specific age and sex in the country\cr
#'    \strong{YR} \tab char - the year of the estimate\cr
#'    \strong{AGE} \tab char - the specific age are values from 1 to 100\cr
#'    \strong{SEX} \tab char - the specific sex with values 0 - Both, 1 - Male, 2 - Female
#' }
#' More details are available \href{https://api.census.gov/data/timeseries/idb/1year/variables.html}{here}.
#'
#' For the "5year", see \href{https://api.census.gov/data/timeseries/idb/5year/variables.html}{5year} and the 119 variable
#'    names and descriptions.
#'
#' @param dataset A required string that selects the dataset of interest. Acceptable values are
#'   "1year" or "5year".
#' @param years An optional numeric vector that sets the years of interest. The default is 2023.
#' @param vars An optional string vector of variable names to be acquired. The default is GEO_ID,NAME,GENC,POP
#'   for either "1year" or "5year".
#' @param group A logical that if \code{TRUE} returns all the variables from the "5year" dataset. The default is \code{FALSE}.
#' @param wide_to_long The returned \code{data.table} is normally in a wide format with all the variables as columns.
#'   If this logical parameter is \code{TRUE} then a long format is returned with variable names in one column (named "estimate") and
#'   their respective values in another column (named "value").
#' @param countries An optional string vector of country/area abbreviations (GENC Standard Countries and Areas) of interest.
#'   GENC codes are available \href{https://nsgreg.nga.mil/registries/browse/results.jsp?registryType=genc&registerField=IE4&browseType=genc}{here}.
#' @param ages An optional numeric that selects specific ages(0 to 100 range) from the 1year dataset. Default is all ages.
#' @param sex An optional numeric vector to select the sex of interest from the 1year dataset. Acceptable values
#'   0 - Both, 1 - Male, 2 - Female. Default is all three values for sex.
#' @param key A string that sets the access key. All Census Bureau API requests require an access key.
#'   Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
#'   The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
#'   Run \code{usethis::edit_r_environ()} and edit your .Renviron file with the line: CENSUS_KEY=your key
#'   to create the global association. This is a required parameter.
#'
#' @return A \code{data.table}
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   library(data.table)
#'   library(httr2)
#'   library(jsonlite)
#'   library(RcensusPkg)
#'
#'   # 1year wide form, default variables, 2 countries
#'   # (Botswana, Norway)
#'   one_year_dt <- RcensusPkg::get_idb_data(
#'     dataset = "1year",
#'     years = c(2023, 2024),
#'     countries = c("BW", "NO")
#'   )
#'
#'   # 5year long form, all variables, 2023, country = US
#'   five_year_US_long_group_dt <- RcensusPkg::get_idb_data(
#'     dataset = "5year",
#'     years = 2023,
#'     group = TRUE,
#'     countries = "US",
#'     wide_to_long = TRUE
#'   )
#' }
#'
#' @import data.table
#' @import httr2
#' @import jsonlite
#'
#' @export
get_idb_data <- function(
  dataset = NULL,
  years = 2023,
  vars = NULL,
  group = FALSE,
  wide_to_long = FALSE,
  countries = NULL,
  ages = 1:100,
  sex = c(0, 1, 2),
  key = Sys.getenv("CENSUS_KEY")
){
  dt <- NULL

  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  if(is.null(dataset)){
    stop("The parameter 'dataset' must be specified.")
  }

  # Create a string url based on the submitted parameters
  a_url <- NULL

  # initial assignment
  if(dataset == "1year"){
    a_url <- "https://api.census.gov/data/timeseries/idb/1year?"
  }else if(dataset == "5year"){
    a_url <- "https://api.census.gov/data/timeseries/idb/5year?"
  }

  # vars
  if(is.null(vars)){
    vars <- c("GEO_ID", "NAME", "GENC", "POP")
  }else {
    vars <- c("GEO_ID", "NAME", "GENC", "POP", vars)
  }
  a_url <- paste0(a_url, "get=", paste(vars, collapse = ","))
  if(dataset == "5year" & group){
      a_url <- paste0(a_url, ",group(IDB5YEAR)")
  }

  # years
  a_url <- paste0(a_url, "&YR=", paste(years, collapse = ","))

  # ages, sex
  if(dataset == "1year"){
    # ages
    a_url <- paste0(a_url, "&AGE=", paste(ages, collapse = ","))
    # sex
    a_url <- paste0(a_url, "&SEX=", paste(sex, collapse = ","))
  }

  # countries
  if(!is.null(countries)){
    a_url <- paste0(a_url,"&for=genc+standard+countries+and+areas:", paste(countries, collapse = ","))
  }else {
    a_url <- paste0(a_url,"&for=genc+standard+countries+and+areas:*")
  }

  # key
  a_url <- paste0(a_url, "&key=", key)

  url_coded <- utils::URLencode(a_url)

  # Make a web request
  tryCatch({
    resp <- httr2::request(url_coded) |> httr2::req_perform()
    content_json <- resp |> httr2::resp_body_string()
    content_mt <- jsonlite::fromJSON(content_json)

    # Create data,table
    dt <- data.table::as.data.table(content_mt)
    colnames(dt) <- content_mt[1,]
    dt <- dt[-1]
    dt$`genc standard countries and areas` <- NULL

    if(wide_to_long){
      long_dt <- data.table::melt(
        data = dt,
        id.vars = c("GEO_ID", "NAME")
      )
      return(long_dt)
    }else{
      return(dt)
    }
  },error = function(err){
    stop("Error downloading raw json text: ", err$message, "\n")
  })
}
