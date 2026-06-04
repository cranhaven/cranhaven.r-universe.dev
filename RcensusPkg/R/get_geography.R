#' @title get_geography
#'
#' @description Get the list of geography entities available (state, county, tract, etc)
#' for a specific dataset.
#'
#' Function produces a \code{data.table} of all the geography "name" and "geoLevelDisplay" variables
#'   available for a specific dataset and optionally a vintage.
#'
#' @param dataset A required string that sets the acronym name of the data set of interest (e.g. "acs/acs5")
#' @param vintage An optional numeric that sets the year of interest.
#'
#' @return A \code{data.table}
#'
#' @examples
#' library(jsonlite)
#' library(data.table)
#' library(httr2)
#' library(RcensusPkg)

#' # Get the geographies available for dataset "acs/acs1/profile" with vintage 2019
#' acs1_profile_geo_dt <- RcensusPkg::get_geography(
#'   dataset = "acs/acs1/profile",
#'   vintage = 2019
#' )
#'
#' @import data.table
#' @import httr2
#' @import jsonlite
#'
#' @export
get_geography <- function(dataset, vintage = NULL){
  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)
  a_url <- paste(a_url, "geography.json", sep="/")

  # Make a web request
  tryCatch({
    resp <- httr2::request(a_url) |> httr2::req_perform()
    content_json <- resp |> httr2::resp_body_string()
    content_ls <- jsonlite::fromJSON(content_json)
    # Return a data.table
    geo_dt <- data.table(
      name = content_ls$fips$name,
      geoLevelDisplay = content_ls$fips$geoLevelDisplay
    )
    return(geo_dt)
  },error = function(err){
    stop("Error downloading raw json text: ", err$message, "\n")
  })
}
