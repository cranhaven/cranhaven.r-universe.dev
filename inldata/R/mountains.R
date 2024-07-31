#' Mountain Ranges and Buttes
#'
#' @description A simplified representation of mountain ranges and buttes in the
#'   vicinity of Idaho National Laboratory (INL) in eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{Feature name.}
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring;
#'       the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source Spatial polygons were created from areas in the digital elevation model (DEM)
#'   where terrain slope is greater than a specified threshold.
#'
#' @keywords datasets
#'
#' @examples
#' print(mountains)
"mountains"
