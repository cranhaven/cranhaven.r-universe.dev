#' Road Network
#'
#' @description Road network in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{Street or road name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`prisec_fl`}{Whether a road is classified as primary or secondary.}
#'     \item{`geometry`}{Sequence of points connected by straight, non-self-intersecting line pieces,
#'       one-dimensional geometry.}
#'   }
#'
#' @source Spatial line extracts were obtained from the
#'   Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   ([MAF/TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html))
#'   Database (MTDB), 2023 data collection, released November 22, 2023. Which is a part of the
#'   U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   These lines were cropped to the study area, and any non-essential data was removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(roads)
"roads"
