#' Calculate (x, y) coordinates
#'
#' Calculates (x,y) coordinates from dx and dy values
#'
#' Use the dx and dy columns in the servosphere data frames to calculate the (x,
#' y) coordinate for each position recording. If the data will be aggregated, it is
#' recommended to aggregate the data before running this function.
#'
#' @param list A list of data frame objects with columns dx and dy.
#' @return Converts dx and dy values to (x, y) coordinates.
#' @examples
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcXY(servosphere)
#' @export


calcXY <- function(list) {
   purrr::map_if(list, is.data.frame, function(.x) {
      dplyr::mutate(.x,
                    x = cumsum(.x$dx),
                    y = cumsum(.x$dy))
   })
}

#' Calculate distance
#'
#' Calculate distance moved between time steps
#'
#' Use the dx and dy columns in the servosphere data frames to calculate the
#' distance moved between each time recording. If the data will be aggregated, it
#' is recommended to aggregate the data before running this function.
#'
#' @param list A list of data frames, each of which has a column for dx and dy.
#' @return A list of data frames, each of which has a variable for the distance
#'   moved between each data recording.
#' @examples
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcDistance(servosphere)
#' @export

calcDistance <- function(list) {
   purrr::map_if(list, is.data.frame, function(.x) {
      dplyr::mutate(.x, distance = sqrt((.x$dx ^ 2) + (.x$dy ^ 2)))
   })
}

#' Calculate angle between a vector and y-axis
#'
#' Calculate the angle between a vector and y-axis, not the x-axis as in atan2
#'
#' This is a helper function. It is called in another function but is otherwise
#' not "outward facing".
#'
#' This function calculates the angle between a vector and the y-axis. The
#' base function atan2 calculates the angle between a vector and the x-axis,
#' which is not desirable in this case.
#'
#' If the data will be aggregated, it is recommended to aggregate the data before
#' running this function.
#'
#' @param x The x coordinate from the (x, y) locations.
#' @param y The y coordinate from the (x, y) location for an organism.

atan3 <- function(x, y) {
   atan2(x, y)
}

#' Calculate bearing
#'
#' Calculates the bearing (direction of movement) for each time step
#'
#' This function requires that the data has been previously processed with the
#' \code{calcXY()} function, providing (x, y) coordinates. Calculate the
#' direction moved by the organism relative to the y axis between each time step
#' in your data frames.
#'
#' If the data will be aggregated, it is recommended to aggregate the data before
#' running this function.
#'
#' @param list A list of data frames with separate columns for x and y
#'   coordinate values.
#' @return A list of data frames with a column for the bearing of the organism
#'   at each time step.
#' @examples
#' # Provide a list of data frames with two columns for the (x, y) coordinates
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcXY(servosphere)
#'
#' servosphere <- calcBearing(servosphere)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

calcBearing <- function(list) {
   list %>% purrr::map_if(is.data.frame, function(.x) {
      dplyr::mutate(.x,
                    ang = atan3(
                       lead(.x$x, default = NA) - lag(.x$x, 0, default = NA),
                       lead(.x$y, default = NA) - lag(.x$y, 0, default = NA)
                    ))
   }) %>%
      purrr::map_if(is.data.frame, function(.x) {
         mutate(.x, bearing = (.x$ang * (180 / pi)) %% 360)
      }) %>%
      purrr::map_if(is.data.frame, function(.x) {
         .x[!names(.x) %in% "ang"]
      })
}

#' Calculate turn angle
#'
#' Calculate the turn angle between two successive moves
#'
#' For this function to work, the data must have previously been processed with
#' the \code{calcBearing} function.
#'
#' This function calculates the turn angle between two successive movement
#' vectors. If the organism has not moved for a period of time but begins moving
#' again, the function calculates the turn angle between the last movement the
#' organism made and its current move.
#'
#' If the data will be aggregated, it is recommended to aggregate the data before
#' running this function.
#'
#' @param list A list of data frames, where each data frame has a column for
#'   bearing.
#' @return A list of data frames that each contain a column for turn angle.
#' @examples
#' # Provide a data frame that includes a column with bearing data
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcXY(servosphere)
#' servosphere <- calcBearing(servosphere)
#' servosphere <- calcTurnAngle(servosphere)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

calcTurnAngle <- function(list) {
   purrr::map_if(list, is.data.frame, function(.x) {
      .x[!is.na(.x$bearing), "turn_angle"] <-
         c(NA, diff(.x[!is.na(.x$bearing), "bearing"]))
      return(.x)
   })
}

#' Calculate turn velocity
#'
#' Calculate the turning velocity in degrees per second between two moves
#'
#' For this function to work, the data must have previously been processed with
#' the \code{calcTurnAngle} function.
#'
#' This function calculates the turning velocity between two consecutive moves.
#' The units for turn velocity will be degrees per second.
#'
#' If the data will be aggregated, it is recommended to aggregate the data before
#' running this function.
#'
#' @param list A list of data frames, where each data frame has a column for
#'   turn angle.
#' @return A list of data frames that each contain a column for turn velocity.
#' @examples
#' # Provide data previously processed by the calcTurnAngle function
#'
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#' servosphere <- calcXY(servosphere)
#' servosphere <- calcBearing(servosphere)
#' servosphere <- calcTurnAngle(servosphere)
#' servosphere <- calcTurnVelocity(servosphere)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

calcTurnVelocity <- function(list) {
   purrr::map_if(list, is.data.frame, function(.x) {
      dplyr::mutate(.x, turn_velocity = abs(.x$turn_angle / (.x$dT / 1000)))
   })
}

#' Calculate velocity
#'
#' Calculate the average velocity between two location recordings
#'
#' This function calculates the velocity of the organism on the servosphere
#' between two location recordings. The units for velocity will be distance per
#' second, where distance is the units of distance used by the software in
#' recording the movement of the organism. For example, if the software recorded
#' distance in centimeters, the units for velocity will be centimeters per
#' second.
#'
#' If the data will be aggregated, it is recommended to aggregate the data before
#' running this function.
#'
#' @param list A list of data frames, where each data frame has a column for dT,
#'   dx, and dy.
#' @return A list of data frames that each contain a column for velocity.
#' @examples
#'  servosphere <- list(data.frame(id = rep(1, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("a", 200),
#'                                 date = rep("2032018", 200)),
#'                      data.frame(id = rep(2, 200),
#'                                 stimulus = rep(c(0, 1), each = 100),
#'                                 dT = sample(8:12, 200, replace = TRUE),
#'                                 dx = runif(200, 0, 5),
#'                                 dy = runif(200, 0, 5),
#'                                 treatment = rep("b", 200),
#'                                 date = rep("2032018", 200)))
#'
#' servosphere <- calcVelocity(servosphere)
#' @export
#' @import dplyr
#' @importFrom magrittr %>%

calcVelocity <- function(list) {
   purrr::map_if(list, is.data.frame, function(.x) {
      dplyr::mutate(.x,
                    velocity =
                       case_when(
                          .x$dx == 0 & .x$dy == 0 ~ 0,
                          .x$dx != 0 |
                             .x$dy != 0 ~ (sqrt((.x$dx ^ 2) + (.x$dy ^ 2))) /
                             (.x$dT / 1000)
                       ))
   })
}


