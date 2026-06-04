#' @title tiger_water_sf
#'
#' @description
#'   This function performs three tasks:
#' \enumerate{
#'   \item Download to an output directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' @details
#' Returns simple feature (sf) water related geometric polygons provided
#'   by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional water related
#'   variables are provided.  See
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix J. Record Layouts: Hydrography (Area and Line)}
#'   for a description of water related area and line variables of the sf file. See
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix L-2. Record Layout: Coastlines Shapefile}
#'   for a description of coastline related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.10 Hydrography (Area and Linear}
#'   and 4.12.2 Coastline --
#'   "The area hydrography shapefile contains the geometry and attributes of both perennial and intermittent
#'   area hydrography features (e.g., ponds, lakes, oceans, swamps, glaciers, and the area covered by large
#'   streams represented as double-line drainage). The linear hydrography shapefile includes
#'   streams/rivers, braided streams, canals, ditches, artificial paths, and aqueducts. A linear hydrography
#'   feature may include edges with both perennial and intermittent persistence"
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::create_sf_plot()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' Some earlier vintages may have NA for the crs so you may need to specify the 'crs_transform' to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' @param state The two-digit FIPS code for the state of interest.
#'   See \href{https://cran.r-project.org/package=usmap}{usmap::fips function} for finding FIPS codes.
#' @param county The \emph{three-digit} FIPS code for the county of interest.
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'`.
#'   The default is \code{TRUE}.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#'   The value should be greater than 2010.
#' @param entity A character string that sets the specific water entity of interest. The
#'   acceptable values are "area", "linear", or "coastline". The default is "area".
#' @param set_crs A numeric or character string which if non-NULL calls sf::st_crs() to set the crs of the geometries and transforms them.
#' @param transform_crs A numeric or character string which if non-NULL calls sf::st_transform()
#'   to perform a crs transform of the geometries. Note that the crs of the shapefile must not be \code{NA}.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from 'datafile' dataframe used to key with the 'sf_key' column of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the 'datafile' dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe.
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Florida (which has a fips code of "12"),
#'   then you assign 'express' equal to: expression(STATEFP == "12"). The expression will be
#'   evaluated and only the geometries for Florida will be returned.
#' @param check_na A logical which if \code{TRUE} will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for \code{NA} values.
#'
#' \strong{Note: If entity equals "coastline" then the state and county arguments are not required.}
#'
#' @return A data frame object of class sf
#'
#' @examples
#' library(sf)
#' library(data.table)
#' library(downloader)
#' library(usmap)
#' library(withr)
#' library(RcensusPkg)
#'
#' # Get the water areas for a county in Ohio
#' state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
#' state_fips <- substr(state_county_fips,1,2)
#' county_fips <- substr(state_county_fips,3,5)
#' # Define a temporary output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#' geauga_area_water_sf <- RcensusPkg::tiger_water_sf(
#'   state = state_fips,
#'   county = county_fips,
#'   output_dir = output_dir,
#'   delete_files = FALSE
#' )
#'
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#'
#' @export
tiger_water_sf <- function(
  state = NULL,
  county = NULL,
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  entity = "area",
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
    a_url <- NULL
    if(entity == "area"){
      if(is.null(shapefile) & (is.null(state) | is.null(county))){
        stop("Both state and county arguments are required for entity 'area'")
      }
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/AREAWATER/tl_%s_%s%s_areawater.zip",
                       vintage_char, vintage_char, state, county)
    }else if(entity == "linear"){
      if(is.null(shapefile) & (is.null(state) | is.null(county))){
        stop("Both state and county arguments are required for entity 'linear'")
      }
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/LINEARWATER/tl_%s_%s%s_linearwater.zip",
                       vintage_char, vintage_char, state, county)
    }else if(entity == "coastline"){
      if(vintage > 2016){
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COASTLINE/tl_%s_us_coastline.zip",
                       vintage_char, vintage_char)
      }else{
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COAST/tl_%s_us_coastline.zip",
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
      caller = "tiger_water_sf"
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
