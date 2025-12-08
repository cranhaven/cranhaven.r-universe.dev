#' Point of Equidistance to Up to Three Longitude, Latitude Points
#'
#' Generates a latitude and longitude point that is equidistant to up to three latitude and longitude points
#'
#' @param coordinate_matrix A matrix of latitude and longitude columns and up to three rows
#' @param alternative Whether to use alternative line creation method. Could be needed when nearly inverse angles cause intersections to be ambiguous.
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
#'             sample(327131680:419648450, 3) / 10000000,
#'             sample(-1147301410:-1241938690, 3) / 10000000
#'         ),
#'         ncol = 2
#'     )
#'
#'# Generate circumcenter and radius
#' gmp <- geo_midpoint(sample_coord)
#'
#' # Find distance to circumcenter
#' radius <- geo_point_dist(rbind(sample_coord[1, ], gmp))
#'
#' # Create 80 sided polygon based on gmp's center and radius
#' gmp_poly <- geo_surround_poly(gmp, radius, 80)
#'
#' # Join all the points into a single matrix
#' bound_poly <- rbind(sample_coord, as.vector(gmp), gmp_poly)
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

geo_midpoint <- function(coordinate_matrix, alternative = FALSE) {

    if (!is.matrix(coordinate_matrix)) {

        stop("Argument \"coordinate_matrix\" must be a matrix")

    }

    if (!ncol(coordinate_matrix) == 2) {

        stop("Argument \"coordinate_matrix\" has to contain 2 columns, latitiude & longitude")

    }

    if(nrow(coordinate_matrix) == 2) {

        rad_multiplier  <- pi / 180
        lat1_rad        <- coordinate_matrix[1, 1] * rad_multiplier
        lon1_rad        <- coordinate_matrix[1, 2] * rad_multiplier
        lat2_rad        <- coordinate_matrix[2, 1] * rad_multiplier
        lon2_rad        <- coordinate_matrix[2, 2] * rad_multiplier
        cart1_x         <- cos(lat1_rad) * cos(lon1_rad)
        cart1_y         <- cos(lat1_rad) * sin(lon1_rad)
        cart1_z         <- sin(lat1_rad)
        cart2_x         <- cos(lat2_rad) * cos(lon2_rad)
        cart2_y         <- cos(lat2_rad) * sin(lon2_rad)
        cart2_z         <- sin(lat2_rad)
        cart3_x         <- (cart1_x * 0.5) + (cart2_x * 0.5)
        cart3_y         <- (cart1_y * 0.5) + (cart2_y * 0.5)
        cart3_z         <- (cart1_z * 0.5) + (cart2_z * 0.5)
        hyp             <- sqrt(cart3_x * cart3_x + cart3_y * cart3_y)
        lon3_rad        <- atan2(cart3_y, cart3_x)
        lat3_rad        <- atan2(cart3_z, hyp)
        return(`names<-`(c(lat3_rad / rad_multiplier, lon3_rad / rad_multiplier), c('lat', 'lon')))

    } else if (nrow(coordinate_matrix) == 3) {

        max_deviation      <- max(geo_point_dist(coordinate_matrix))

        points_ab           <- coordinate_matrix[c(1, 2), ]
        midpoint_ab         <- geo_midpoint(points_ab)
        general_bearing_ab  <- geo_points_bearing(rbind(midpoint_ab, coordinate_matrix[3, ])) %% 360
        ceiling_bearing_ab  <- (geo_points_bearing(points_ab) + 90) %% 360
        floor_bearing_ab    <- (geo_points_bearing(points_ab) - 90) %% 360

        if (alternative) {

        ceiling_outpoint_ab <- geo_move_point(midpoint_ab, ceiling_bearing_ab, max_deviation)
        floor_outpoint_ab   <- geo_move_point(midpoint_ab, floor_bearing_ab, max_deviation)

        } else {
            if (
                abs(ceiling_bearing_ab - general_bearing_ab) >
                abs(floor_bearing_ab - general_bearing_ab)
            ) {
                bearing_ab <- floor_bearing_ab
            } else {
                bearing_ab <- ceiling_bearing_ab
            }
        }

        points_ac           <- coordinate_matrix[c(1, 3), ]
        midpoint_ac         <- geo_midpoint(points_ac)
        general_bearing_ac  <- geo_points_bearing(rbind(midpoint_ac, coordinate_matrix[2, ])) %% 360
        ceiling_bearing_ac  <- (geo_points_bearing(points_ac) + 90) %% 360
        floor_bearing_ac    <- (geo_points_bearing(points_ac) - 90) %% 360
        if (
            abs(ceiling_bearing_ac - general_bearing_ac) >
            abs(floor_bearing_ac - general_bearing_ac)
        ) {
            bearing_ac <- floor_bearing_ac
        } else {
            bearing_ac <- ceiling_bearing_ac
        }

        if (alternative) {

            ceiling_outpoint_ac <- geo_move_point(midpoint_ac, ceiling_bearing_ac, max_deviation)
            floor_outpoint_ac   <- geo_move_point(midpoint_ac, floor_bearing_ac, max_deviation)

        } else {
            if (
                abs(ceiling_bearing_ac - general_bearing_ac) >
                abs(floor_bearing_ac - general_bearing_ac)
            ) {
                bearing_ac <- floor_bearing_ac
            } else {
                bearing_ac <- ceiling_bearing_ac
            }
        }

        if (alternative) {

            rad_multiplier  <- pi / 180
            lat1_rad        <- floor_outpoint_ab[1] * rad_multiplier
            lon1_rad        <- floor_outpoint_ab[2] * rad_multiplier
            lat2_rad        <- ceiling_outpoint_ac[1] * rad_multiplier
            lon2_rad        <- ceiling_outpoint_ac[2] * rad_multiplier
            bearing1_rad    <- ceiling_bearing_ab[1] * rad_multiplier
            bearing2_rad    <- floor_bearing_ac[1] * rad_multiplier

        } else {

            rad_multiplier <- pi / 180
            lat1_rad       <- midpoint_ab[1] * rad_multiplier
            lon1_rad       <- midpoint_ab[2] * rad_multiplier
            lat2_rad       <- midpoint_ac[1] * rad_multiplier
            lon2_rad       <- midpoint_ac[2] * rad_multiplier
            bearing1_rad   <- bearing_ab * rad_multiplier
            bearing2_rad   <- bearing_ac * rad_multiplier

        }


        dLon <- lon2_rad - lon1_rad
        dLat <- lat2_rad - lat1_rad

        dist12 <-
            asin(
                sqrt(
                    sin(dLat / 2) *
                    sin(dLat / 2) +
                    cos(lat1_rad) *
                    cos(lat2_rad) *
                    sin(dLon/2) *
                    sin(dLon/2)
                )
            ) * 2

        if (dist12 == 0) {

            return(NULL)

        }

        brngA <-
            acos(
                (sin(lat2_rad) - sin(lat1_rad)*cos(dist12)) /
                (sin(dist12) * cos(lat1_rad))
            )

        if (is.na(brngA)) {

            brngA <- 0

        }

        brngB <-
            acos(
                (sin(lat1_rad) - sin(lat2_rad) * cos(dist12)) /
                (sin(dist12)*cos(lat2_rad))
            )

        if (sin(lon2_rad - lon1_rad) > 0) {

            brng12 <- brngA
            brng21 <- 2 * pi - brngB

        } else {

            brng12 <- 2 * pi - brngA
            brng21 <- brngB

        }

        alpha1 <- (bearing1_rad - brng12 + pi) %% (2 * pi) - pi
        alpha2 <- (brng21 - bearing2_rad + pi) %% (2 * pi) - pi

        if (sin(alpha1) == 0 & sin(alpha2) == 0) {

            return(NULL)

        }

        # if (sin(alpha1) * sin(alpha2) < 0) {
        #
        #     return(NULL)
        #
        # }

        alpha3 <-
            acos(
                -cos(alpha1) *
                cos(alpha2) +
                sin(alpha1) *
                sin(alpha2) *
                cos(dist12)
            )

        dist13 <-
            atan2(
                sin(dist12) *
                sin(alpha1) *
                sin(alpha2),
                cos(alpha2) +
                cos(alpha1) *
                cos(alpha3)
            )

        lat3 <-
            asin(
                sin(lat1_rad) *
                cos(dist13) +
                cos(lat1_rad) *
                sin(dist13) *
                cos(bearing1_rad)
            )

        dLon13 <-
            atan2(
                sin(bearing1_rad) *
                sin(dist13) *
                cos(lat1_rad),
                cos(dist13) -
                sin(lat1_rad) *
                sin(lat3)
            )

        lon3 <- lon1_rad + dLon13
        lon3 <- (lon3 + pi) %% (2 * pi) - pi

        return(c(lat3 / rad_multiplier, lon3 / rad_multiplier))

    } else {

        stop("Argument \"coordinate_matrix\" has to contain 2 or 3 rows")

    }
}
