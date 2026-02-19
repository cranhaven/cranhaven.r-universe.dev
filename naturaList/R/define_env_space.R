#' Define environmental space for species occurrence
#'
#' Based on two continuous environmental variables, it defines a
#' bi-dimensional environmental space.
#'
#' @param env matrix or data frame with two columns containing two
#'  environmental variables. The variables must be numeric, even for data frames.
#' @param buffer.size numeric value indicating a buffer size around each point
#'  which will delimit the environmental geographical border for the occurrence
#'  point. See details.
#' @param plot logical. whether to plot the polygon. Default is TRUE.
#'
#' @details The environmental variables are standardized by range, which turns
#'  the range of each environmental variable from 0 to 1. Then, it is delimited
#'  a buffer of size equal to \code{buffer.size} around each point in this
#'  space and a polygon is draw to link these buffers. The function returns the
#'  polygon needed to link all points, and the area of the polygon indicates
#'  the environmental space based in the variables used.
#'
#' @return An object of sfc_POLYGON class
#'
#' @examples
#' \dontrun{
#' library("raster")
#'
#' # load climate data
#' data("r.temp.prec")
#' env.data <- raster::as.data.frame(r.temp.prec)
#'
#' define_env_space(env.data, 0.05)
#' }
#'
#'
#'
#'
#'
#'
#' @importFrom stats na.omit
#' @export
#'
define_env_space <- function(env,
                             buffer.size,
                             plot = TRUE){

  env.range <- vegan::decostand(na.omit(env), "range")
  env.range <- unique(env.range)
  env.point <-  sf::st_multipoint(as.matrix(env.range))

  box <- raster::extent(c(0,1,0,1))
  r <- raster::raster(box, resolution = 0.025)
  r.cell <- unique(raster::cellFromXY(r, env.range))
  xy.cell <- raster::xyFromCell(r, r.cell)
  ch.point <-  sf::st_multipoint(as.matrix(xy.cell))
  ch.buffer <- sf::st_buffer(ch.point, buffer.size)

  env.space <- sf::st_crop(sf::st_geometry(ch.buffer), box)

  if(plot) plot(env.space)
  return(env.space)
}
