#' Idaho National Laboratory Boundary
#'
#' @description Geographic limits of the Idaho National Laboratory ([INL](https://inl.gov/)).
#'   The INL is located on the west-central part of the eastern Snake River Plain
#'   and covers an area of approximately 890 square miles.
#'   It was established in 1949 to develop atomic energy, nuclear safety, defense programs,
#'   environmental research, and advanced energy concepts.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring; the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source The INL boundary was digitized from 7.5' quads within and in the vicinity of the Idaho
#' National Laboratory by the U.S. Geological Survey
#' [Idaho National Laboratory Project Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)
#' The digitized polygons were made into shapefiles and converted into GeoJSON files.
#'
#' @keywords datasets
#'
#' @examples
#' print(inl)
"inl"
