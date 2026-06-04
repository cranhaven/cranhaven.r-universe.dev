#' @title remove_area_water
#'
#' @description  Function removes water area geometries from downloaded Census Bureau shapefiles
#'
#' @param x An sf shapefile object with possible areas of water downloaded from the Census Bureau
#' @param vintage An integer that specifies the year of the shapefile. The default is 2020.
#' @param output_dir A full directory path where supportive shapefiles and their associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is TRUE.
#'
#' @return An sf object with the water area geometries removed.
#'
#' @examples
#' \dontrun{
#'   # Request for Census Bureau data plus CPU processing can exceed well over 10
#'   #  seconds in some cases.
#'   library(downloader)
#'   library(sf)
#'   library(usmap)
#'   library(withr)
#'   library(data.table)
#'   library(RcensusPkg)
#'
#'   # Remove water areas from New York, New York sf object
#'   ny_state_county_fips <- usmap::fips(state = "New York", county = "New York")
#'   ny_state_fips <- substr(ny_state_county_fips, 1,2)
#'   ny_county_fips <- substr(ny_state_county_fips, 3, 5)
#'
#'   # Define a temporary output folder for the downloaded shapefiles
#'   output_dir <- withr::local_tempdir()
#'   if(!dir.exists(output_dir)){
#'     dir.create(output_dir)
#'   }
#'
#'   # Define an expression for filtering just the New York county
#'   express <- parse(text = paste0("COUNTYFP == ", '"', ny_county_fips, '"'))
#'
#'   # Get the New York,New York county tract sf geometries object
#'   ny_tracts_sf <- RcensusPkg::tiger_tracts_sf(
#'     state = ny_state_fips,
#'     vintage = 2020,
#'     general = FALSE,
#'     transform_crs = 6538,
#'     express = express,
#'     output_dir = output_dir,
#'     delete_files = FALSE
#'   )
#'
#'   # Remove the area waters from New York, New York sf object
#'     ny_without_water_sf <- RcensusPkg::remove_area_water(
#'     ny_tracts_sf,
#'     output_dir = output_dir,
#'     delete_files = FALSE
#'   )
#'}
#' @importFrom sf st_transform
#' @importFrom sf st_filter
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_difference
#' @importFrom sf st_union
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#'
#' @export
remove_area_water <- function(
    x,
    vintage = 2020,
    output_dir = tempdir(),
    delete_files = TRUE){

  if (!"sf" %in% class(x)) {
    stop("The input dataset is not an sf object.")
  }

  # Get the US counties shapefile
  us_counties_sf <- RcensusPkg::tiger_counties_sf(
    output_dir = output_dir,
    vintage = vintage,
    general = TRUE,
    delete_files = delete_files
  )

  # Identify the counties that overlap the input sf object
  filtered_county_sf <- us_counties_sf |>
    sf::st_transform(sf::st_crs(x)) |>
    sf::st_filter(x)

  # If us_county_sf has no observations then exit
  if (nrow(filtered_county_sf) == 0) {
    stop("Your sf geometries do not appear to be in the United States.")
  }

  # Get a vector of GEOIDs
  county_GEOID_v <- filtered_county_sf$GEOID
  get_area_water_fun <- function(geoid){
    water_sf <- RcensusPkg::tiger_water_sf(
      state = substr(geoid, 1, 2),
      county = substr(geoid, 3, 5),
      vintage = vintage,
      output_dir = output_dir,
      delete_files = delete_files
    )
   return(water_sf)
  }

  # Download the area water shapefile geometries for the above GEOID's
  water_lst <- purrr::map(county_GEOID_v, get_area_water_fun)
  water_dt <- data.table::rbindlist(water_lst)

  sf_with_water_sf <- sf::st_as_sf(water_dt) |>
    sf::st_transform(sf::st_crs(x)) |>
    sf::st_filter(x)
  union_sf <- sf::st_union(sf_with_water_sf)
  difference_sf <- suppressWarnings(sf::st_difference(x, union_sf))
  return(difference_sf)
}
