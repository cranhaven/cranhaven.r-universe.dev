#' @title tiger_zctas_sf
#'
#' @description
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to an output directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' @details
#' Returns simple feature (sf) shapefile Zip Code Tabulation Areas(ZCTA) shapefile of the entire US.
#'
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is TRUE.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if \code{TRUE} will download a less detailed, more generalized version of the county geometries.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from 'datafile' dataframe used to key with the 'sf_key' column of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the 'datafile' dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe.
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Florida (which has a fips code of "12"),
#'   then you assign 'express' equal to: expression(STATEFP == "12"). The expression will be
#'   evaluated and only the geometries for Florida will be returned.
#'
#' @return A data frame object of class sf
#'
#' @examples
#' \dontrun{
#'   # Beware that downloading and processing Zip Code Tabulation Areas(ZCTA)
#'   #   shapefiles of the entire US can be time consuming
#'   library(downloader)
#'   library(sf)
#'   library(data.table)
#'   library(withr)
#'   library(RcensusPkg)
#'
#'   # Get the sf geometries for the Zip Code Tabulation Area in the county of
#'   #   Middlesex, MA near Boston
#'   # Define a temporary, self deleting output folder for the downloaded shapefiles
#'   output_dir <- withr::local_tempdir()
#'   if(!dir.exists(output_dir)){
#'     dir.create(output_dir)
#'   }
#'
#'   # Define the codes of interest in a dataframe
#'   mun_zcta_df <- data.frame(
#'     name = c(
#'       "Carlisle", "Concord", "Bedford",
#'       "Lexington", "Lexington", "Lincoln",
#'       "Lincoln", "Sudbury", "Wayland",
#'       "Weston","Waltham"
#'     ),
#'     zcta = c(
#'       "01741","01742","01730",
#'       "02420","02421","01773",
#'       "01731","01776","01778",
#'       "02493","02451"
#'     )
#'   )
#'
#'   # Define an expression to filter the downloaded shapefile
#'   express <- expression(ZCTA5CE20 %in% mun_zcta_df$zcta)
#'
#'   # Get the sf geometries
#'   mun_zcta_sf <- RcensusPkg::tiger_zctas_sf(
#'     vintage = 2020,
#'     general = TRUE,
#'     do_progress = TRUE,
#'     express = express,
#'     output_dir = output_dir,
#'     delete_files = FALSE
#'   )
#' }
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#'
#' @export
tiger_zctas_sf <- function(
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  general = FALSE,
  sf_info = FALSE,
  do_progress = FALSE,
  datafile = NULL,
  datafile_key = NULL,
  sf_key = NULL,
  express = NULL
){
  vintage_char <- as.character(vintage)
  a_url <- NULL

  if(general){
    if(vintage >= 2020){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta520_500k.zip",
                       vintage_char, vintage_char)
    }else if(vintage < 2020){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta510_500k.zip",
                       vintage_char, vintage_char)
    }
  }else{
    if(vintage >= 2020){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ZCTA520/tl_%s_us_zcta520.zip",
                       vintage_char, vintage_char)
    }else {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ZCTA5/tl_%s_us_zcta510.zip",
                     vintage_char, vintage_char)
    }
  }

  tiger_sf <- .send_tiger_url(
    a_url = a_url,
    output_dir = output_dir,
    delete_files = delete_files,
    sf_info = sf_info,
    do_progress = do_progress,
    caller = "tiger_zctas_sf")

  if(!is.null(tiger_sf)){
    if(!is.null(datafile)){
      tiger_sf <- RcensusPkg::join_it(
        df_1 = datafile,
        df_2 = tiger_sf,
        key_1 = datafile_key,
        key_2 = sf_key,
        return_sf = TRUE
      )
    }

    if(!is.null(express)){
      tiger_dt <- data.table::as.data.table(tiger_sf)
      tiger_dt <- tiger_dt[eval(express), ]
      tiger_sf <- sf::st_as_sf(tiger_dt)
    }
  }
  return(tiger_sf)
}
