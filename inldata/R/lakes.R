#' Lakes and Ponds
#'
#' @description Perennial lakes and ponds in the vicinity of the Idaho National Laboratory (INL) in eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`gnis_nm`}{GNIS name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`reach_cd`}{Reach code, a unique 14-digit code that identifies a
#'       continuous piece of surface water with similar hydrologic characteristics.}
#'     \item{`gnis_id`}{Geographic Names Information System (GNIS) identifier.}
#'     \item{`feature_tp`}{USGS National Hydrography Dataset (NHD) feature type code.
#'       "LakePond" is a standing body of water with a predominantly natural shoreline surrounded by land;
#'       "Reservoir" is a constructed basin formed to contain water or other liquids; and
#'       "SwampMarsh" is a non-cultivated, vegetated area that is inundated or
#'         saturated for a significant part of the year.}
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring;
#'       the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source Spatial polygon extract files obtained from the
#'    U.S. Geological Survey (USGS) National Hydrography Dataset (NHD) Medium Resolution for Idaho,
#'    released August 4, 2014. Which is part of the National Geospatial Technical Operations Center.
#'    These extracts were cropped to eastern Idaho extent and unnecessary columns were removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(lakes)
"lakes"
