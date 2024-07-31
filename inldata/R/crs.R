#' Coordinate Reference System
#'
#' @description Coordinate reference system (CRS) used by the
#'   U.S. Geological Survey Idaho National Laboratory Project Office.
#'   The CRS is based on the following attributes:
#'   Albers equal-area conic projection;
#'   latitude of first and second standard parallel is 42.83 and 44.16 decimal degrees, respectively;
#'   latitude and longitude of false origin is 41.5 and -113 decimal degrees, respectively;
#'   easting and northing of false origin is 200,000 and 0 meters, respectively;
#'   Clarke (1966) reference ellipsoid; North American Datum of 1983; and units of meters.
#'   The CRS is represented using an updated version of the well-known text
#'   ([WKT2](https://docs.ogc.org/is/18-010r7/18-010r7.html)) strings.
#'
#' @format A list with the following elements representing the CRS:
#'   `input` is the [PROJ.4](https://proj4.org/) string, and `wkt` is the WKT2 strings.
#'
#' @source Idaho National Laboratory Project Office
#'
#' @keywords datasets
#'
#' @examples
#' print(crs)
"crs"
