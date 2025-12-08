#' Geo Polygon
#'
#' Generates a collection of points that are equidistant to the center coordinates given and are distributed equally around the center
#'
#' @param coordinates A vector of the center latitude and longitude point
#' @param distance Distance to move away from center for each bearing
#' @param sides Number of polygon sides
#'
#' @return Returns a matrix of latitude and longitude points.
#'
#' @author Shant Sukljian
#' @seealso \code{\link{geo_sed}} \code{\link{geo_point_dist}}
#' @keywords sed smallest enclosing disk circle geo latitude longitude
#' @examples
#'
#' # Load required packages
#' library(mapview)
#' library(sp)
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
#' # Create 80 sided polygon based on a random center and radius
#' geo_poly <- geo_surround_poly(sample_coord, sample(50:500, 1), 80)
#'
#' # Join all the points into a single matrix
#' bound_poly <- rbind(sample_coord, geo_poly)
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

geo_surround_poly <- function(coordinates, distance, sides) {

    if (length(as.vector(coordinates)) > 2) {

        stop("geo_surround_poly: Argument \"coordinates\" contains more than two values")

    } else if (length(as.vector(coordinates)) < 2) {

        stop("geo_surround_poly: Argument \"coordinates\" contains less than two values")

    }

    hexagon_angles  <- seq(0, 359, 360 / sides)
    dist_offset     <- distance / 6371
    rad_multiplier  <- pi / 180
    lat1_rad        <- as.vector(coordinates)[1] * rad_multiplier
    lon1_rad        <- as.vector(coordinates)[2] * rad_multiplier
    sin_bearing     <- sin(dist_offset)
    cos_dist_offset <- cos(dist_offset)
    cos_lat1_rad    <- cos(lat1_rad)
    sin_lat1_rad    <- sin(lat1_rad)
    new_coordinates <- rep(list(vector(mode = "double", length = 2)), length(hexagon_angles))

    for(i in 1:length(hexagon_angles)) {

        bearing_rad <- hexagon_angles[i] * rad_multiplier
        lat3_rad    <-
            asin((sin_lat1_rad * cos_dist_offset) +
            (cos_lat1_rad * sin_bearing * cos(bearing_rad)))
        lon3_rad    <-
            lon1_rad +
            atan2(
                sin(bearing_rad) * sin_bearing * cos_lat1_rad,
                cos_dist_offset - sin_lat1_rad * sin(lat3_rad))
        new_coordinates[[i]] <-
            c(lat3_rad / rad_multiplier, lon3_rad / rad_multiplier)

    }

    return(matrix(unlist(new_coordinates), ncol = 2, byrow = TRUE))

}
