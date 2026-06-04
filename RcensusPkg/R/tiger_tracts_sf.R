#' @title tiger_tracts_sf
#'
#' @description This function performs three tasks:
#' \enumerate{
#'   \item Download to an output directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' @details
#' Returns simple feature (sf) of US Census tract boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional tract related
#'   variables are provided.  See \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix G-3. Record Layout: Census Tract State-based Shapefile)}
#'   for a description of tract related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.4 Census Tracts} --
#'   "Census tracts are small and relatively permanent statistical subdivisions of a county or equivalent entity.
#'   Local participants review and update census tracts prior to each decennial census as part of the Census
#'   Bureau’s PSAP."
#'
#'  A more generalized, recognizable version of the tract geometries that has less download size is also available.  For more information on cartographic boundary files see
#'     \href{https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file.html}{Cartographic Boundary File Description}.
#'     These files are available for vintages greater than 2013 with resolution 1:500k meters.
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::create_sf_plot()}) or
#' joined with US Census Bureau demographic data.  To help incorporate data files, this function
#' has a \code{datafile} parameter which will be joined with the resultant simple feature object. The only
#' requirement is that a common "key" for joining exist between the data dataframe and the simple feature dataframe.
#'
#' Some earlier vintages may have NA for the crs so you may need to specify the 'crs_transform' to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' @param state A required two-digit FIPS code for the state of interest.
#'   See \href{https://cran.r-project.org/package=usmap}{usmap::fips function} for finding FIPS codes.
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is \code{TRUE}.
#' @param vintage A required numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if \code{TRUE} will download a less detailed, more generalized version of the tract geometries.
#' @param set_crs A numeric or character string which if non-NULL calls \code{sf::st_crs()} to set the crs of the geometries and transforms them.
#' @param transform_crs A numeric or character string which if non-NULL calls \code{sf::st_transform()}
#'   to perform a crs transform of the geometries. Note that the crs of the shapefile must not be NA.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from 'datafile' dataframe used to key with the 'sf_key' column of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the 'datafile' dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe.
#'   For example, one of the columns of the resultant simple feature dataframe is "COUNTYFP".
#'   If you wanted to return just the geometries for Los Alamos, New Mexico (which has a fips code of "028"),
#'   then you assign 'express' equal to: expression(COUNTYFP == "028"). The expression will be
#'   evaluated and only the tract geometries for Los Alamos will be returned.
#' @param check_na A logical which if \code{TRUE} will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for \code{NA} values.
#'
#' @return A data frame object of class sf (simple feature)
#'
#' @examples
#' library(downloader)
#' library(sf)
#' library(data.table)
#' library(usmap)
#' library(withr)
#' library(RcensusPkg)
#'
#' # Get the generalized tract geometries for Los Alamos, New Mexico
#' # Determine the fips codes for New Mexico and Los Alamos
#' nm_los_alamos_fips <- usmap::fips(state = "new mexico", county = "los alamos")
#' nm_fips <- substr(nm_los_alamos_fips, 1, 2)
#' los_alamos_fips <- substr(nm_los_alamos_fips, 3, 5)
#'
#' # Create an expression to filter just the Los Alamos geometries
#' express <- parse(text = paste0("COUNTYFP == ", '"', los_alamos_fips, '"'))
#'
#' # Define a temporary output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#'
#' # Get the tract geometries for just Los Alamos
#' losalamos_tracts_sf <- RcensusPkg::tiger_tracts_sf(
#'   state = nm_fips,
#'   general = TRUE,
#'   express = express,
#'   output_dir = output_dir,
#'   delete_files = FALSE
#' )
#'
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#' @importFrom stats na.omit
#'
#' @export
tiger_tracts_sf <- function(
  state = NULL,
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  general = FALSE,
  set_crs = NULL,
  transform_crs = NULL,
  sf_info = FALSE,
  do_progress = FALSE,
  shapefile = NULL,
  datafile = NULL,
  datafile_key = NULL,
  sf_key = "GEOID",
  express = NULL,
  check_na = FALSE
){

  if(is.null(shapefile) & is.null(state)){
    stop("The state argument is required")
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
    if(general){
      if(vintage %in% c(1990, 2000)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr%sshp/tr%s_d%s_shp.zip",
                       sub_year, state, sub_year)
      }else if(vintage == 2010){
        a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_140_00_500k.zip",
                       state)
      }else{
        if(vintage > 2013) {
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_tract_500k.zip",
                         vintage_char, vintage_char, state)
        }else {
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_tract_500k.zip",
                         vintage_char, vintage_char, state)
        }
      }
    }else {
      if(vintage %in% c(2000, 2010)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/TRACT/%s/tl_2010_%s_tract%s.zip",
                         vintage_char, state, sub_year)
      }else {
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/TRACT/tl_%s_%s_tract.zip",
                         vintage_char, vintage_char, state)
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
      caller = "tiger_tracts_sf"
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
