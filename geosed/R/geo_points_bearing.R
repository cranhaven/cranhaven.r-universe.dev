#' Bearing Between Two Latitude and Longitude Points
#'
#' Calculates the bearing angle in degrees between two latitude, longitude points
#'
#' @param coordinate_pair A matrix of latitude and longitude columns and two rows of points
#'
#' @return A vector of length 1 containing a bearing
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
#'             sample(327131680:419648450, 2) / 10000000,
#'             sample(-1147301410:-1241938690, 2) / 10000000
#'         ),
#'         ncol = 2
#'     )
#'
#' # Calculate bearing
#' (gpb <- geo_points_bearing(sample_coord))
#'
#' # Create SpacialPoints object and pass to mapview for visualization
#' mapview(
#'     SpatialPoints(
#'         sample_coord[,c(2, 1)],
#'         proj4string = CRS("+proj=longlat +datum=WGS84")
#'     )
#' )
#'
#'
#' @export

geo_points_bearing <- function(coordinate_pair) {

    if (nrow(coordinate_pair) > 2) {

        stop("geo_points_bearing: Argument \"coordinate_pair\" contains more than two rows")

    } else if (nrow(coordinate_pair) < 2) {

        stop("geo_points_bearing: Argument \"coordinate_pair\" contains less than two rows")

    }

    if(!ncol(coordinate_pair) == 2) {

        stop("geo_points_bearing: Argument \"coordinate_pair\" must contain two columns, latitude and longitude")
        
    }

    if(!is.matrix(coordinate_pair)) {

        stop("geo_points_bearing: Argument \"coordinate_pair\" must be a matrix")
        
    }

    rad_multiplier  <- pi / 180
    lat1_rad        <- coordinate_pair[1, 1] * rad_multiplier
    lon1_rad        <- coordinate_pair[1, 2] * rad_multiplier
    lat2_rad        <- coordinate_pair[2, 1] * rad_multiplier
    lon2_rad        <- coordinate_pair[2, 2] * rad_multiplier
    interm_1        <- cos(lat2_rad) * sin(lon2_rad - lon1_rad)
    interm_2        <- cos(lat1_rad) * sin(lat2_rad) - sin(lat1_rad) * cos(lat2_rad) * cos(lon2_rad - lon1_rad)

    return(atan2(interm_1, interm_2) / rad_multiplier)

}