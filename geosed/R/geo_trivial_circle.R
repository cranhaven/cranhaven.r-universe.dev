#' Circle encompassing up to three points
#'
#' Generates a center point and radius that represent the smallest circle that contains up to three input points
#'
#' @param coordinate_matrix A matrix of latitude and longitude columns and up to three rows
#' @param ... `alternative` argument to be used when calling \code{\link{geo_midpoint}}
#'
#' @return Returns a list of three elements named radius, center and making. Radius contains a single value representing the circle radius. Center contains a vector of length 2 representing the circle center latitude and longitude. Making contains a matrix of the latitude and longitude points were used as the coordinate_matrix argument.
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
#' # Generate sed center and radius
#' gtc <- geo_trivial_circle(sample_coord)
#'
#' # Create 80 sided polygon based on gtc's center and radius
#' gtc_poly <- geo_surround_poly(gtc$center, gtc$radius, 80)
#'
#' # Join all the points into a single matrix
#' bound_poly <- rbind(sample_coord, gtc$center, gtc_poly)
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

geo_trivial_circle <- function(coordinate_matrix, ...) {

    if (!is.matrix(coordinate_matrix)) {

        stop("Argument \"coordinate_matrix\" must be a matrix")

    }

    if (nrow(coordinate_matrix) > 3) {

        stop("Argument \"coordinate_matrix\" has to contain no more than 3 rows")

    }

    if (!ncol(coordinate_matrix) == 2) {

        stop("Argument \"coordinate_matrix\" has to contain 2 columns, latitiude & longitude")

    }

    e <- 1

    if (nrow(coordinate_matrix) == 0) {

        return(
            list(
                radius = 0,
                center = c(0, 0),
                making = coordinate_matrix
            )
        )

    } else if (nrow(coordinate_matrix) == 1) {

        return(
            list(
                radius = 0,
                center = as.numeric(coordinate_matrix),
                making = coordinate_matrix
            )
        )

    } else if (nrow(coordinate_matrix) == 2) {

        return(
            list(
                radius = geo_point_dist(coordinate_matrix) / 2 * e,
                center = as.numeric(geo_midpoint(coordinate_matrix)),
                making = coordinate_matrix
            )
        )

    } else if (nrow(coordinate_matrix) == 3) {

        point_distances_matrix <- geo_point_dist(coordinate_matrix, matrix = TRUE)

        point_distances <- unique(as.vector(point_distances_matrix))[unique(as.vector(point_distances_matrix)) != 0]

        longest_side <- which(point_distances == max(point_distances))

        if (
            (point_distances[-longest_side][1]^2 +
             point_distances[-longest_side][2]^2 -
             point_distances[longest_side]^2) < 0
        ) {

            furthest_points <-
                `[`(
                    coordinate_matrix,
                    which(
                        point_distances_matrix == max(point_distances_matrix),
                        arr.ind = TRUE
                    )[1,],
                )

            list(
                radius = geo_point_dist(furthest_points) / 2 * e,
                center = as.numeric(geo_midpoint(furthest_points)),
                making = coordinate_matrix
            )

        } else {

            furthest_points <-
                `[`(
                    coordinate_matrix,
                    which(
                        point_distances_matrix == max(point_distances_matrix),
                        arr.ind = TRUE
                    )[1,],
                )

            midpoint <- geo_midpoint(coordinate_matrix, ...)

            return(
                list(
                    radius = as.numeric(geo_point_dist(rbind(furthest_points[1, ], midpoint))) * e,
                    center = as.numeric(midpoint),
                    making = coordinate_matrix
                )
            )
        }
    }
}
