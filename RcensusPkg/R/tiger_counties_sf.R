#' @title tiger_counties_sf
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
#' Returns simple feature (sf) of the entire US county boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional county related
#'   variables are provided.  See \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix I-3. Record Layout: County Subdivision State-based Shapefile)}
#'   for a description of county related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.7 Counties and Equivalent Enties} --
#'   "Counties and equivalent entities are primary legal divisions of states. In most states, these entities are
#'    termed 'counties.'"
#'
#'  A more generalized, recognizable version of the county geometries that has less download size is also available.  For more information on cartographic boundary files see
#'     \href{https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file.html}{Cartographic Boundary File Description}.
#'     These files are available for vintages greater than 2013 with resolution 1:500k, 1:5m, 1:20m meters.
#'
#'  Some earlier vintages may have NA for the crs so you may need to specify the 'crs_transform' to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::create_sf_plot()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is TRUE.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if \code{TRUE} will download a less detailed, more generalized version of the county geometries.
#' @param resol If 'general' is \code{TRUE}, then the resolution to return. Acceptable values are strings
#'   "500k", "5m", "20m".
#' @param set_crs A numeric or character string which if non-NULL calls sf::st_crs() to set the crs of the geometries and transforms them.
#' @param transform_crs A numeric or character string which if non-NULL calls \code{sf::st_transform()}
#'   to perform a crs transform of the geometries. Note that the crs of the shapefile must not be \code{NA}.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from 'datafile' dataframe used to key with the 'sf_key' column
#'   of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the 'datafile' dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe.
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Florida (which has a fips code of "12"),
#'   then you assign 'express' equal to: expression(STATEFP == "12"). The expression will be
#'   evaluated and only the geometries for Florida will be returned.
#' @param check_na A logical which if \code{TRUE} will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for \code{NA} values.
#'
#' @return An object of class sf
#'
#' @examples
#' library(downloader)
#' library(sf)
#' library(data.table)
#' library(usmap)
#' library(withr)
#' library(RcensusPkg)
#'
#' # Get the geometries for Ohio counties
#' # Determine the fips code for Ohio (returns "39")
#' ohio_fips <- usmap::fips(state = "ohio")
#' # Create an expression to filter out just Ohio counties
#' #   from the simple feature dataframe
#' express <- parse(text = paste0("STATEFP == ", '"', ohio_fips, '"'))
#' # Define a temporary output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#' # Get the Ohio county's generalized geometries
#' ohio_counties_sf <- RcensusPkg::tiger_counties_sf(
#'   general = TRUE,
#'   express = express,
#'   output_dir = output_dir,
#'   delete_files = FALSE
#' )
#'
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#'
#' @export
tiger_counties_sf <- function(
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
  sf_key = "COUNTYFP",
  express = NULL,
  check_na = FALSE
){
  if(is.null(output_dir)){
    stop("An output folder for shapefiles must be created and specified(output_dir).")
  }else if(!dir.exists(output_dir)){
    stop(paste0("The shapefiles output directory ", output_dir, " does not exist."))
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
  }else { # Downloading shapefile
    vintage_char <- as.character(vintage)
    a_url <- NULL
    if(!(resol %in% c("500k", "5m", "20m"))){
      stop("Acceptable values for resolution are '500k', '5m', '20m'.")
    }

    if(general){
      if(vintage %in% c(1990, 2000)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/co/co%sshp/co99_d%s_shp.zip",
                       sub_year, sub_year)
      }else if(vintage == 2010){
        a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_%s.zip",
                       resol)
      }else {
        if(vintage > 2013){
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_county_%s.zip",
                         vintage_char, vintage_char, resol)
        } else {
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_us_county_%s.zip",
                         vintage_char, vintage_char, resol)
        }
      }
    }else {
      if(vintage %in% c(2000, 2010)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/%s/tl_2010_us_county%s.zip",
                         vintage_char, sub_year)
      }else {
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COUNTY/tl_%s_us_county.zip",
                         vintage_char, vintage_char)
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
      caller = "tiger_counties_sf")

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

      if(check_na){
        tiger_sf <- na.omit(tiger_sf)
      }
    }
    return(tiger_sf)
  }
}
