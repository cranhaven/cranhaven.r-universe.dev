#' Eastern Snake River Plain Boundary
#'
#' @description Boundary of the eastern Snake River Plain (ESPR), Idaho.
#'   The ESPR is a structural basin that extends about 200 miles in a northeast direction and is 50-70 miles wide.
#'   The basin is bounded by faults on the northwest and by down warping and faulting on the southeast.
#'   It has been filled with basaltic lava flows interbedded with terrestrial sediments.
#'   The combination of basaltic rock and sedimentary deposits forms the ESRP aquifer,
#'   which is the primary source of groundwater in the basin
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring; the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source The ESPR boundary was digitized from 7.5' quads within and in the vicinity of the Idaho
#'   National Laboratory by the U.S. Geological Survey
#'   [Idaho National Laboratory Project Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)
#'   The digitized polygons were made into shapefiles and converted into GeoJSON files.
#'
#' @keywords datasets
#'
#' @examples
#' print(esrp)
"esrp"
