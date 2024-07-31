#' Cities and Towns
#'
#' @description Cities and towns (populated places) in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{City name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`geometry`}{Zero-dimensional geometry containing a single point.}
#'   }
#'
#' @source Spatial point extracts were obtained from the
#'   Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   ([MAF/TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html))
#'   Database (MTDB), 2023 data collection, released November 22, 2023. Which is a part of the
#'   U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   The centroids of these extracts were cropped to the study area, and any non-essential data was removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(cities)
"cities"
