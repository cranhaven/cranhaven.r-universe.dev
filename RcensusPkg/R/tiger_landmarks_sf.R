#' @title tiger_landmarks_sf
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
#' Returns simple feature (sf) landmark related geometric polygons provided
#'   by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional landmark related
#'   variables are provided.  See
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix K-1.K-2, Record Layout: Area/Point Landmark Shapefile}
#'   for a description of landmark related area and point variables of the sf file.
#'   For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.11 Landmark (Area and Point} --
#'   "The Census Bureau includes landmarks in the MAF/TIGER System to locate special features and help
#'   enumerators during field operations. Some of the more common landmark types include area landmarks
#'   (e.g., airports, cemeteries, parks, and educational facilities) and point landmarks (e.g., schools and
#'   churches)"
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RplotterPkg}{RplotterPkg::create_sf_plot()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' Some earlier vintages may have NA for the crs so you may need to specify the 'transform_crs' to 3426.  Also
#'   you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' @param state The required two-digit FIPS code for the state of interest.
#'  See \href{https://cran.r-project.org/package=usmap}{usmap::fips function} for finding FIPS codes.
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is \code{TRUE}.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#'   The value should be greater than 2010.
#' @param entity A character string that sets the specific landmark entity of interest. The
#'   acceptable values are "area" or "point". The default is "point".
#' @param set_crs A numeric or character string which if non-NULL calls sf::st_crs() to set the crs of the geometries and transforms them.
#' @param transform_crs A numeric or character string which if non-NULL calls \code{sf::st_transform()}
#'   to perform a crs transform of the geometries. Note that the crs of the shapefile must not be NA.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param express A logical expression object used to filter the resultant simple feature dataframe.
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Nelson County, Kentucky (which has a fips code of "179"),
#'   then you assign 'express' equal to: expression(COUNTYFP == "179"). The expression will be
#'   evaluated and only the landmark geometries for Nelson County will be returned.
#' @param check_na A logical which if \code{TRUE} will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for \code{NA} values.
#'
#' \strong{Note: Vintage must be greater than 2010.}
#'
#' @return A data frame object of class sf
#'
#' @examples
#' library(downloader)
#' library(sf)
#' library(data.table)
#' library(withr)
#' library(usmap)
#' library(RcensusPkg)
#'
#' # Get the sf object for landmarks in the state of Kentucky
#' # Define a temporary, self deleting output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#'
#' # Get the fips code for the state
#' kentucky_fips <- usmap::fips(state = "kentucky")
#'
#' # Get the "point" landmarks for Kentucky
#' kentucky_landmarks_sf <- RcensusPkg::tiger_landmarks_sf(
#'   state = kentucky_fips,
#'   entity = "point",
#'   check_na = TRUE,
#'   output_dir = output_dir,
#'   delete_files = FALSE
#' )
#'
#' @importFrom sf st_read
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom data.table as.data.table
#'
#' @export
tiger_landmarks_sf <- function(
  state = NULL,
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  entity = "point",
  set_crs = NULL,
  transform_crs = NULL,
  sf_info = FALSE,
  do_progress = FALSE,
  shapefile = NULL,
  express = NULL,
  check_na = FALSE
){

  if(is.null(shapefile) & is.null(state)){
    stop("The state argument is required")
  }

  if(is.null(shapefile) & vintage < 2011){
    stop("Vintage is not currently available for years prior to 2011")
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
    a_url <- NULL
    if(entity == "point"){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/POINTLM/tl_%s_%s_pointlm.zip",
                       vintage_char, vintage_char, state)
    }else if(entity == "area"){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/AREALM/tl_%s_%s_arealm.zip",
                       vintage_char, vintage_char, state)
    }else {
      stop(paste0("Function does not recognize entity ", entity))
    }

    tiger_sf <- .send_tiger_url(
      a_url = a_url,
      output_dir = output_dir,
      delete_files = delete_files,
      set_crs = set_crs,
      transform_crs = transform_crs,
      sf_info = sf_info,
      do_progress = do_progress,
      caller = "tiger_landmarks_sf"
    )

    if(!is.null(tiger_sf)){
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

