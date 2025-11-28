#' @noRd
# @title
#  Check position of geographical location
#
# @family utils
#
# @description
#   Check if a geographical location is inside the triangle formed by three another objects on a map
#
#
# @param lat
#   latitude of geographical location to check, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lon
#   longitude of geographical location to check, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lat1
#   latitude of the first vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lon1
#   longitude of the first vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lat2
#   latitude of the second vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lon2
#   longitude of the second vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lat3
#   latitude of the third vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param lon3
#   longitude of the third  vertex of geographical triangle, [\emph{DD}].
#   Type: \code{\link{assert_number}}.
#
# @param earth
#   \emph{Earth} radius, [\emph{m}]. Type: \code{\link{assert_numeric}}.
#
# @return
#   Is object in triangle? Type: \code{\link{assert_flag}}.
#
# @export
#
# @examples
# library(pipenostics)

geointri <- function(lat, lon, lat1, lon1, lat2, lon2, lat3, lon3, earth = 6371008.7714){
  checkmate::assert_number(  lat, lower =     -90     , upper =      90     )
  checkmate::assert_number(  lon, lower =    -180     , upper =     180     )
  checkmate::assert_number( lat1, lower =     -90     , upper =      90     )
  checkmate::assert_number( lon1, lower =    -180     , upper =     180     )
  checkmate::assert_number( lat2, lower =     -90     , upper =      90     )
  checkmate::assert_number( lon2, lower =   - 180     , upper =     180     )
  checkmate::assert_number( lat3, lower =     -90     , upper =      90     )
  checkmate::assert_number( lon3, lower =    -180     , upper =     180     )
  checkmate::assert_number(earth, lower = 6335439.0000, upper = 6399593.6259)

  L_TRIANGLE <- 3L    # number of angles in a triangle
  TOLERANCE  <- 1e-6  # precision up to [m^2]

  spot_set <- cbind(
    c(lat1, lat2, lat3, lat),
    c(lon1, lon2, lon3, lon)
  )
  tri      <- utils::combn(seq_len(nrow(spot_set)), L_TRIANGLE)
  area     <- geoarea(
    lat1 = spot_set[tri[1,],1],  lon1 = spot_set[tri[1,],2],
    lat2 = spot_set[tri[2,],1],  lon2 = spot_set[tri[2,],2],
    lat3 = spot_set[tri[3,],1],  lon3 = spot_set[tri[3,],2],
    earth = earth
  )
  abs(area[[1]] - sum(area[utils::tail(seq_along(area), -1)])) < TOLERANCE
}
