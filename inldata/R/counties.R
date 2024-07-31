#' County Boundaries
#'
#' @description County boundaries in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{County name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring; the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source Spatial polygon extracts were obtained from the
#'   Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   ([MAF/TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html))
#'   Database (MTDB), 2023 data collection, released November 22, 2023. Which is a part of the
#'   U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   These polygons were cropped to study area, and any non-essential data was removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(counties)
"counties"
