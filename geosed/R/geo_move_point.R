#' New Latitude and Longitude Points from Point, Bearing and Distance
#'
#' Creates a new latitude, longitude point based on an origin point, bearing and distance
#'
#' @param coordinates A vector contaning one latitude and longitude point
#' @param bearing The angle relative to north to move towards
#' @param distance The distance in kilometers to move away from the origin point
#'
#' @return Returns a vector of length 2 containing a latitude and longitude point.
#'
#' @author Shant Sukljian
#' @seealso \code{\link{geo_sed}} \code{\link{geo_point_dist}}
#' @keywords sed smallest enclosing disk circle geo latitude longitude
#'
#' @examples
#'
#' # Load required packages
#' require(mapview)
#' require(sp)
#'
#' # Create sample geo dataset
#' sample_coord <-
#'    matrix(
#'         c(
#'             sample(327131680:419648450, 1) / 10000000,
#'             sample(-1147301410:-1241938690, 1) / 10000000
#'         ),
#'         ncol = 2
#'     )
#'
#' # Create new point
#' (gmp <- geo_move_point(sample_coord, sample(0:359, 1), 500))
#'
#' # Join all the points into a single matrix
#' bound_poly <- rbind(sample_coord, gmp)
#'
#' # Create SpacialPoints object and pass to mapview for visualization
#' mapview(
#'     SpatialPoints(
#'         bound_poly[,c(2, 1)],
#'         proj4string = CRS("+proj=longlat +datum=WGS84")
#'     )
#' )
#'
#'
#' @export

geo_move_point <- function(coordinates, bearing, distance) {

    if (length(as.vector(coordinates)) > 2) {

        stop("geo_move_point: Argument \"coordinates\" contains more than two values")

    } else if (length(as.vector(coordinates)) < 2) {

        stop("geo_move_point: Argument \"coordinates\" contains less than two values")

    }

    if (bearing > 360 | bearing < 0) {

        stop("geo_move_point: Argument \"bearing\" must be between 0 and 360")

    }

    dist_offset     <- distance / 6371
    rad_multiplier  <- pi / 180
    lat1_rad        <- as.vector(coordinates)[1] * rad_multiplier
    lon1_rad        <- as.vector(coordinates)[2] * rad_multiplier
    sin_bearing     <- sin(dist_offset)
    cos_dist_offset <- cos(dist_offset)
    cos_lat1_rad    <- cos(lat1_rad)
    sin_lat1_rad    <- sin(lat1_rad)
    bearing_rad     <- bearing * rad_multiplier
    lat3_rad        <- 
        asin(
            (sin_lat1_rad * cos_dist_offset) + 
            (cos_lat1_rad * sin_bearing * cos(bearing_rad))
        )
    lon3_rad        <- 
        lon1_rad + 
        atan2(
            sin(bearing_rad) * sin_bearing * cos_lat1_rad, 
            cos_dist_offset - sin_lat1_rad * sin(lat3_rad)
        ) 

    return(c(lat3_rad / rad_multiplier, lon3_rad / rad_multiplier))
    
}