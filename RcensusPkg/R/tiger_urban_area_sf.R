#' @title tiger_urban_area_sf
#'
#' @description
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to an output directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#' The function returns geometries for the Census Bureau's defined "urban areas"
#' across the entire US
#'
#' @details
#' Returns simple feature(sf) of the Census Bureau's defined "urban area" boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features.
#'
#' A more generalized, recognizable version of the CBSA geometries that has less download size is also available.  For more information on cartographic boundary files see
#'   \href{https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file.html}{Cartographic Boundary File Description}.
#'   These files are available for vintages greater than 2009 with resolution 1:500k, 1:5m, 1:20m meters.
#'   For descriptive information on CBSA see \href{https://www.census.gov/programs-surveys/metro-micro/about.html}{About}.
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::create_sf_plot()}) or
#' joined with US Census Bureau demographic data. To help incorporate data files, this function
#' has a 'datafile' parameter which will be joined with the resultant simple feature object. The only
#' requirement is that a common "key" for joining exist between the data dataframe and the simple feature dataframe.
#'
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is TRUE.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if \code{TRUE} will download a less detailed, more generalized version of the state geometries.
#' @param resol If 'general' is \code{TRUE}, then the resolution to return. Acceptable values are strings
#'   "500k", "5m", "20m".
#' @param set_crs A numeric or character string which if non-NULL calls \code{sf::st_crs()} to set the crs of the geometries and transforms them.
#' @param transform_crs A numeric or character string which if non-NULL calls \code{sf::st_transform()}
#'   to perform a crs transform of the geometries. Note that the crs of the shapefile must not be \code{NA}.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if TRUE displays a progress bar during the download.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from 'datafile' dataframe used to key with the 'sf_key' column of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the 'datafile' dataframe.
#' @param check_na A logical which if \code{TRUE} will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for \code{NA} values.
#'
#' @return A data frame object of class sf
#'
#' @examples
#' library(downloader)
#' library(sf)
#' library(data.table)
#' library(withr)
#' library(RcensusPkg)
#'
#' # Get the defined urban areas for 2019
#' # Define a temporary, self deleting output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#'
#' us_urban_areas_sf <- RcensusPkg::tiger_urban_area_sf(
#'   output_dir = output_dir,
#'   vintage = 2019,
#'   general = TRUE,
#'   delete_files = FALSE
#' )
#'
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#' @importFrom data.table tstrsplit
#'
#' @export
tiger_urban_area_sf <- function(
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  general = FALSE,
  resol = "500k",
  set_crs = NULL,
  transform_crs = NULL,
  sf_info = FALSE,
  do_progress = FALSE,
  shapefile = NULL,
  datafile = NULL,
  datafile_key = NULL,
  sf_key = "GEOID",
  check_na = FALSE
){
  if(is.null(shapefile) & !(resol %in% c("500k", "5m", "20m"))){
    stop("Acceptable values for resolution are '500k', '5m', '20m'.")
  }

  if(!is.null(shapefile)){ # Reading shapefile
    if(!file.exists(shapefile)){
      stop(paste0("Shapefile folder ", shapefile, " does not exists."))
    }
    tiger_sf <- sf::st_read(dsn = shapefile)

    if(!is.null(set_crs)){
      sf::st_crs(tiger_sf) <- set_crs

      tiger_sf <- tiger_sf |>
        sf::st_transform(set_crs)
    }

    if(!is.null(transform_crs)){
      tiger_sf <- tiger_sf |>
        sf::st_transform(transform_crs)
    }

    return(tiger_sf)
  }else {  # Downloading shapefile
    vintage_char <- as.character(vintage)
    if(general){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_ua10_%s.zip",
                         vintage_char, vintage_char, resol)
      if(vintage == 2013) a_url <- gsub("shp/", "", a_url)
    }else{
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/UAC/t1_%s_us_uac10.zip", vintage_char, vintage_char)
    }
  }

  tiger_sf <- .send_tiger_url(
      a_url = a_url,
      output_dir = output_dir,
      delete_files = delete_files,
      set_crs = set_crs,
      transform_crs = transform_crs,
      sf_info = sf_info,
      do_progress = do_progress,
      caller = "tiger_urban_area_sf"
  )

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

    if(check_na){
      tiger_sf <- na.omit(tiger_sf)
    }
  }
  return(tiger_sf)
}
