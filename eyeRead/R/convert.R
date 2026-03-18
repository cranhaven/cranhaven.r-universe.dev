#'
#' @rdname convert
#' 
#' @name convert
#' 
#' @title convert between units in eye-tracking
#' 
#' @description The functions convert  between visual degrees and centimeters, inches or pixels. 
#'
NULL 

#' @describeIn convert converts from centimeters to visual degrees
#' 
#' @aliases size2deg
#' 
#' @param x The value to convert. This can be a single number or a numerical 
#'   vector.
#' 
#' @param dist The distance to the screen. See Details for the measurement unit.
#' 
#' @details When converting from size to degrees (\code{size2deg}), the measurement 
#'   unit of the distance to the screen (\code{dist}) should be consistent with 
#'   the measurement unit of the size you are converting from (\code{x}. 
#'   Namely if the size is in centimeters, then the distance should be to, and if 
#'   the size is in inches, then the distance should be in inches.
#' 
#' @return Returns a value or vector of values indicating the corresponding visual 
#'   degrees.
#' 
#' @examples ### these functions convert between units
#'   ## from size to degrees
#'   # for single values
#'   size2deg( x = 2, dist = 30 ) # 3.818304866
#'   
#'   # and multiple values
#'   size2deg( x = c( 2, 0.5, 7, 2, 20, 0.5 ),
#'             dist = c( 30, 30, 60, 15, 30, 15 ) )
#'    # 3.818304866, 0.954907555, 6.676941008, 7.628149669, 36.86989765, 
#'    # 1.909682508
#' 
#' @export size2deg
#' 

size2deg <- function( x, dist )
{
  rad <- 2 * atan( x / (2 * dist ) )
  rad * 180 / pi
}

#' @describeIn convert converts from pixels to visual degrees
#' 
#' @aliases px2deg
#' 
#' @param res A vector indicating the screen resolution in the horizontal 
#'   direction (in pixels).
#' 
#' @param screenW The width of the screen in the horizontal direction. See 
#'   Details for the measurement unit.
#'   
#' @details When converting from pixels to degrees (\code{px2deg}), the measurement 
#'   unit of the distance to the screen (\code{dist}) should be consistent with 
#'   the measurement unit of the screen width (\code{screen width}). Namely if 
#'   the size is in centimeters, then the screen width should be to, and if the 
#'   size is in inches, then the screen width should be in inches.
#' 
#' @examples ## from pixels to degrees
#'   # for single values
#'   px2deg( x = 2, dist = 30, res = 1024, screenW = 32 ) # 0.119366164
#'   
#'   # and multiple values
#'   px2deg( x = c( 2, 8, 100 ), dist = 30, res = 1024, screenW = 32 )
#'     # 0.119366164, 0.477462066, 5.96292244
#' 
#' @export px2deg
#' 

px2deg <- function( x, dist, res, screenW )
{
  x <- x / ( res / screenW )
  size2deg( x, dist )
}

#' @describeIn convert converts from visual degrees to centimeters
#' 
#' @aliases deg2size
#' 
#' @examples ## from degrees to size
#'   # for single values
#'   deg2size( x = 2, dist = 30 ) # 1.047303896
#'   
#'   # and multiple values
#'   deg2size( x = c( 2, 8, 100 ), dist = 30 )
#'     # 1.047303896, 4.195608717, 71.50521556
#' 
#' @export deg2size
#' 

deg2size <- function( x, dist )
{
  rad <- x * pi / 180
  tan( rad/2 ) * 2 * dist
}

#' @describeIn convert converts from visual degrees to pixels
#' 
#' @aliases deg2px
#' 
#' @examples ## from degrees to pixels
#'   # for single values
#'   deg2px( x = 0.119366164, dist = 30, res = 1024, screenW = 32 ) # 2
#'   
#'   # and multiple values
#'    deg2px( x = c( 0.119366164, 0.477462066, 5.96292244 ), dist = 30,
#'            res = 1024, screenW = 32 )
#'      # 2, 8, 100
#' 
#' @export deg2px
#' 

deg2px <- function( x, dist, res, screenW )
{
  x <- deg2size( x, dist )
  x * ( res / screenW )
}