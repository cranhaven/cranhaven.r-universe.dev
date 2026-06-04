#' @title get_multi_vintage_data
#'
#' @description Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a \code{data.table} for multiple vintages. The function requires
#'   a Census Bureau access key.
#'
#' @param dataset A required string that sets the name of the dataset of interest (e.g. "acs/acs5").
#' @param vintage_v A required numeric vector that sets the vintages of interest.
#' @param vars A required string vector of variable acronym names to be acquired (e.g. "B15002_015").
#'   See RcensusPkg::get_variable_names() for obtaining acronym names.
#' @param region A string that specifies the geography of the request. See \code{Rcensus::get_geography()} for
#'   assistance in obtaining these values.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param key A string that sets the access key. All Census Bureau API requests require an access key.
#'   Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
#'   The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
#'   This is a required parameter.
#'
#' @return A \code{data.table}
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   # Obtain the median home value (“B25077_001E”) for Deschutes County, Oregon
#'   #   back to 2005 through 2019.
#'   library(jsonlite)
#'   library(data.table)
#'   library(httr2)
#'   library(usmap)
#'   library(RcensusPkg)
#'
#'   # Get the fips codes for state and county
#'   deschutes_fips <- usmap::fips("OR","Deschutes")
#'   state <- substr(deschutes_fips,1,2)
#'   county <- substr(deschutes_fips,3,5)
#'
#'   RcensusPkg::get_multi_vintage_data(
#'     dataset = "acs/acs1",
#'     vintage_v = 2005:2019,
#'     vars = c("B25077_001E", "B25077_001M"),
#'     region = paste0("county:", county),
#'     regionin = paste0("state:", state)
#'   )
#' }
#' @import data.table
#' @import httr2
#' @import jsonlite
#'
#' @export
get_multi_vintage_data <- function(
  dataset = NULL,
  vintage_v = NULL,
  vars = NULL,
  region = NULL,
  regionin = NULL,
  key = Sys.getenv("CENSUS_KEY")
) {
  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  if(is.null(dataset)){
    stop("The parameter 'dataset' must be specified.")
  }
  if(is.null(vintage_v)){
    stop("The parameter 'vintage_v' must be specified")
  }
  if(is.null(vars)){
    stop("The parameter 'vars' must be specified")
  }

  GEO_ID <- vintage <- NULL

  getDT <- function(vintage){
    # Create a string url based on the submitted parameters
    a_url <- .get_url(dataset, vintage)

    a_url <- paste0(a_url, "?get=")

    get_vars <- paste(vars, collapse = ",")
    a_url = paste0(a_url, get_vars)

    if(!is.null(region)){
      a_url <- paste0(a_url, "&for=", region)
    }
    if(!is.null(regionin)){
      a_url <- paste0(a_url, "&in=", regionin)
    }

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

      if("GEO_ID" %in% names(dt)){
        dt <- dt[, c("pre", "GEOID") := tstrsplit(GEO_ID, "US")] |>
          _[,`:=`(pre = NULL, GEO_ID = NULL)]
      }

      return(dt)
    },error = function(err){
      stop("Error downloading raw json text: ", err$message, "\n")
    })
  }

  dt <- getDT(vintage_v[[1]])

  dt[, vintage := vintage_v[[1]]]

  for(i in 2:length(vintage_v)){
    a_dt <- getDT(vintage_v[[i]])

    a_dt[, vintage := vintage_v[[i]]]

    dt <- rbind(dt, a_dt)
  }

  return(dt)
}
