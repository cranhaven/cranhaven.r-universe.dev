#' Rivers and Streams
#'
#' @description Stream segments in the vicinity of Idaho National Laboratory (INL), eastern Idaho.
#'   Surface water infiltrated to the eastern Snake River Plain aquifer
#'   through river and streams.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`gnis_nm`}{GNIS name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`reach_cd`}{Reach code, a unique 14-digit code that identifies a
#'       continuous piece of surface water with similar hydrologic characteristics.}
#'     \item{`gnis_id`}{Geographic Names Information System (GNIS) identifier.}
#'     \item{`feature_tp`}{USGS National Hydrography Dataset (NHD) feature type code.
#'       The codes and their meanings are as follows:
#'         "ArtificialPath" is a surrogate for general flow direction;
#'         "CanalDitch" is an artificial open waterway constructed to transport water,
#'           to irrigate or drain land, to connect two or more bodies of water,
#'           or to serve as a waterway for watercraft;
#'         "Connector"is a linear water feature that connects two or more waterbodies or
#'           other linear water features; and
#'         "StreamRiver" is a linear water feature that is a natural or man-made flowing body of water.}
#'     \item{`geometry`}{Sequence of points connected by straight, non-self-intersecting line pieces,
#'       one-dimensional geometry.}
#'   }
#'
#' @source Spatial line extract files obtained from the
#'   U.S. Geological Survey (USGS) National Hydrography Dataset (NHD) Medium Resolution for Idaho,
#'   released August 4, 2014. Which is part of the National Geospatial Technical Operations Center.
#'   These extracts were cropped to eastern Idaho extent and unnecessary columns were removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(streams)
"streams"
