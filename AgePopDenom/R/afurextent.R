#' Africa Urban/Rural Extent Raster
#'
#' This raster dataset provides the urban and rural extent across Africa,
#' useful for spatial modeling and demographic analysis. It is compressed
#' as a `.zip` file and included in the package's `inst/extdata/` directory.
#'
#' @format A `.zip` file containing a `.asc` raster file.
#' @details
#' The raster data represents urban and rural extents across Africa:
#' \itemize{
#'   \item Urban areas are represented by one class.
#'   \item Rural areas are represented by another class.
#' }
#'
#' To load and use this raster in your R session, unzip the file and load it using
#' the \code{raster} package.
#'
#' @examples
#' \donttest{
#' # Locate the raster file
#' raster_zip <- system.file("extdata", "afurextent.asc.zip",
#'                            package = "yourpackage")
#'
#' # Unzip and load the raster
#' temp_dir <- tempdir()
#' unzip(raster_zip, exdir = temp_dir)
#' raster_path <- file.path(temp_dir, "afurextent.asc")
#'
#' # Load using terra package
#' r <- terra::rast(raster_path)
#' print(r)
#' }
#' @source Derived from public urban and rural spatial data sources.

#' @keywords datasets

