#' Distance Between Two Latitude and Longitude Points
#'
#' Calculates the distance in kilometers between up to a combination of three latitude, longitude points
#'
#' @param coordinate_matrix A matrix of latitude and longitude columns and up to three rows of points
#' @param matrix Generates a matrix that shows/preseves the relationship between point combinations and the respective distance between them
#'
#' @return An input matrix with two rows returns a vector of length 1 containing the calculated distance. If the matrix argument is set to FALSE and an input matrix with three rows is given as the coordinate_matrix argument a vector of length 3 containing the calculated distances is returned. If the matrix argument is set to TRUE and an input matrix with three rows is given as the coordinate_matrix argument a 3 by 3 matrix of distances is returned.
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
#'             sample(327131680:419648450, 3) / 10000000,
#'             sample(-1147301410:-1241938690, 3) / 10000000
#'         ),
#'         ncol = 2
#'     )
#'
#' # Calculate distances
#' (gpd <- geo_point_dist(sample_coord))
#'
#' # Calculate distances and preserve relationship (Useful for three input points)
#' (gpd <- geo_point_dist(sample_coord, matrix = TRUE))
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

geo_point_dist <- function(coordinate_matrix, matrix = FALSE) {

    if (nrow(coordinate_matrix) > 3) {

        stop("geo_point_dist: Argument \"coordinate_matrix\" contains more than three rows")

    } else if (nrow(coordinate_matrix) < 2) {

        stop("geo_point_dist: Argument \"coordinate_matrix\" contains less than two rows")

    }

    if(!ncol(coordinate_matrix) == 2) {

        stop("geo_point_dist: Argument \"coordinate_matrix\" must contain two columns, latitude and longitude")
        
    }

    if(!is.matrix(coordinate_matrix)) {

        stop("geo_point_dist: Argument \"coordinate_matrix\" must be a matrix")
        
    }

    rad_multiplier  <- pi / 180
    lat1_rad        <- coordinate_matrix[1, 1] * rad_multiplier
    lon1_rad        <- coordinate_matrix[1, 2] * rad_multiplier
    lat2_rad        <- coordinate_matrix[2, 1] * rad_multiplier
    lon2_rad        <- coordinate_matrix[2, 2] * rad_multiplier

    if(nrow(coordinate_matrix) == 2) {

        return(
            acos(
                sin(lat1_rad) * 
                sin(lat2_rad) + 
                cos(lat1_rad) * 
                cos(lat2_rad) * 
                cos(lon2_rad - lon1_rad)
            ) * 6371
        )

    } else if (nrow(coordinate_matrix) == 3) {

        lat3_rad <- coordinate_matrix[3, 1] * rad_multiplier
        lon3_rad <- coordinate_matrix[3, 2] * rad_multiplier
        dist_ab <-
            acos(
                sin(lat1_rad) * 
                sin(lat2_rad) + 
                cos(lat1_rad) * 
                cos(lat2_rad) * 
                cos(lon2_rad - lon1_rad)
            ) * 6371
        dist_ac <-
            acos(
                sin(lat1_rad) * 
                sin(lat3_rad) + 
                cos(lat1_rad) * 
                cos(lat3_rad) * 
                cos(lon3_rad - lon1_rad)
            ) * 6371
        dist_bc <-
            acos(
                sin(lat2_rad) * 
                sin(lat3_rad) + 
                cos(lat2_rad) * 
                cos(lat3_rad) * 
                cos(lon3_rad - lon2_rad)
            ) * 6371

        if (matrix) {

        return(
            matrix(
                c(
                    0, 
                    dist_ab, 
                    dist_ac, 
                    dist_ab, 
                    0, 
                    dist_bc, 
                    dist_ac, 
                    dist_bc, 
                    0
                ), 
                ncol = 3
            )
        )

        } else {

            return(c(dist_ab, dist_ac, dist_bc))

        }
    }
}