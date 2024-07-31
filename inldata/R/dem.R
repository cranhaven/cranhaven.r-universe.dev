#' Digital Elevation Model
#'
#' @description The digital elevation model (DEM) for the Idaho National Laboratory and its vicinity in eastern Idaho.
#'   A DEM is a representation of the land surface that uses a digital grid
#'   to describe the elevation values of the terrain.
#'
#' @format A compressed [`SpatRaster`][terra::SpatRaster] class object with layer:
#'   \describe{
#'     \item{`elevation`}{Land-surface elevations in feet above the North American Vertical Datum of 1988 (NAVD 88).}
#'   }
#'
#' @source The National Map
#'   ([TNM](https://www.usgs.gov/programs/national-geospatial-program/national-map))
#'   1/3-arc-second DEM (Gesch, 2007; Gesch and others, 2002), accessed on August 4, 2020.
#'   This dataset can be downloaded in a Esri ArcGRID TM format using
#'   [TNM Download](https://apps.nationalmap.gov/downloader).
#'   Elevation datasets are distributed in geographic coordinates in units of decimal degrees,
#'   and in conformance with the NAD 83.
#'   Elevation values are in feet above the NAVD 88.
#'
#' @references Gesch, D.B., 2007, The National Elevation Dataset, in Maune, D., ed.,
#'   Digital Elevation Model Technologies and Applications---The DEM User's Manual,
#'   2nd ed.: Bethesda, Maryland, American Society for Photogrammetry and Remote Sensing,
#'   p. 99--118.
#'
#'   Gesch, D., Oimoen, M., Greenlee, S., Nelson, C., Steuck, M., and Tyler, D., 2002,
#'   The National Elevation Dataset: Photogrammetric Engineering and Remote Sensing,
#'   v. 68, no. 1, p. 5--11.
#'
#' @seealso [`make_shade`] function for computing the hill shade from the DEM.
#'
#' @keywords datasets
#'
#' @examples
#' elevation <- terra::unwrap(dem)
#' print(elevation)
"dem"
