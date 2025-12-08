#' Smallest circle encompassing all latitude and longitude points
#'
#' Generates a center point and radius that represent the smallest circle that contains all input points
#'
#' @param coordinate_matrix A matrix of latitude and longitude columns and any chosen number of rows to generate a smallest circle arround
#'
#' @return Returns a list of three elements named radius, center and making. Radius contains a single value representing the circle radius. Center contains a vector of length 2 representing the circle center latitude and longitude. Making contains a matrix of the latitude and longitude points that lie on the final smallest circle circumference.
#'
#' @author Shant Sukljian
#' @seealso \code{\link{geo_trivial_circle}} \code{\link{geo_point_dist}}
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
#'             sample(327131680:419648450, 10) / 10000000,
#'             sample(-1147301410:-1241938690, 10) / 10000000
#'         ),
#'         ncol = 2
#'     )
#'
#' # Generate sed center and radius
#' gsc <- geo_sed(sample_coord)
#'
#' # Create 80 sided polygon based on gsc's center and radius
#' gsc_poly <- geo_surround_poly(gsc$center, gsc$radius, 80)
#'
#' # Join all the points into a single matrix
#' bound_poly <- rbind(sample_coord, gsc$center, gsc_poly)
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
#' @importFrom grDevices chull
#' @importFrom utils combn

geo_sed <- function(coordinate_matrix) {

    c_hull_points <- coordinate_matrix[chull(coordinate_matrix), , drop = FALSE]

    if(nrow(c_hull_points) <= 2) {
        return(geo_trivial_circle(c_hull_points))
    }

    possible_combinations <-
        lapply(
            combn(
                paste0(
                    1:nrow(c_hull_points),
                    ',',
                    c_hull_points[, 1],
                    ',',
                    c_hull_points[, 2]
                ),
                3,
                simplify = FALSE
            ),
            function(x) {
                matrix(
                    as.numeric(
                        unlist(
                            strsplit(x, ',')
                        )
                    ),
                    ncol = 3,
                    byrow = TRUE
                )
            }
        )

    for(i in 1:length(possible_combinations)) {

        cross_check_combinations <- c_hull_points[-possible_combinations[[i]][, 1], , drop = FALSE]

        circle_data <- geo_trivial_circle(possible_combinations[[i]][, c(2, 3)])

        if(nrow(cross_check_combinations) > 0) {

            for(x in 1:nrow(cross_check_combinations)) {

                ccc_status <- geo_point_dist(rbind(cross_check_combinations[x, ], circle_data$center)) < circle_data$radius

                if (!ccc_status) {
                break()
                }

            }

            if (ccc_status) {

            break()

            }
        } else {

            ccc_status <- TRUE

            break()

        }

    }



    if (ccc_status) {

       return(circle_data)

    } else {

        for(i in 1:length(possible_combinations)) {

            cross_check_combinations <- c_hull_points[-possible_combinations[[i]][, 1], , drop = FALSE]

            circle_data_alt <- geo_trivial_circle(possible_combinations[[i]][, c(2, 3)])

            circle_data_alt$radius <- circle_data$radius * 1.2

            if(nrow(cross_check_combinations) > 0) {

                for(x in 1:nrow(cross_check_combinations)) {

                    ccc_status_alt <- geo_point_dist(rbind(cross_check_combinations[x, ], circle_data_alt$center)) < circle_data_alt$radius

                    if (!ccc_status_alt) {
                        break()
                    }

                }

                if (ccc_status_alt) {

                    break()

                }
            } else {

                ccc_status_alt <- TRUE

                break()

            }

        }



        if (ccc_status_alt) {

            return(circle_data_alt)

        } else {

            for(i in 1:length(possible_combinations)) {

                cross_check_combinations <- c_hull_points[-possible_combinations[[i]][, 1], , drop = FALSE]

                circle_data <-
                    geo_trivial_circle(
                        possible_combinations[[i]][, c(2, 3)],
                        alternative = TRUE)

                if(nrow(cross_check_combinations) > 0) {

                    for(x in 1:nrow(cross_check_combinations)) {

                        ccc_status <- geo_point_dist(rbind(cross_check_combinations[x, ], circle_data$center)) < circle_data$radius

                        if (!ccc_status) {
                            break()
                        }

                    }

                    if (ccc_status) {

                        break()

                    }
                } else {

                    ccc_status <- TRUE

                    break()

                }

            }



            if (ccc_status) {

                return(circle_data)

            } else {

                for(i in 1:length(possible_combinations)) {

                    cross_check_combinations <- c_hull_points[-possible_combinations[[i]][, 1], , drop = FALSE]

                    circle_data_alt <- circle_data <-
                        geo_trivial_circle(
                            possible_combinations[[i]][, c(2, 3)],
                            alternative = TRUE)

                    circle_data_alt$radius <- circle_data$radius * 1.2

                    if(nrow(cross_check_combinations) > 0) {

                        for(x in 1:nrow(cross_check_combinations)) {

                            ccc_status_alt <- geo_point_dist(rbind(cross_check_combinations[x, ], circle_data_alt$center)) < circle_data_alt$radius

                            if (!ccc_status_alt) {
                                break()
                            }

                        }

                        if (ccc_status_alt) {

                            break()

                        }
                    } else {

                        ccc_status_alt <- TRUE

                        break()

                    }

                }



                if (ccc_status_alt) {

                    return(circle_data_alt)

                } else {

                    return("Failed")

                }


            }


        }


    }


}
