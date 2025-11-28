# Calculate geographical area
#' @rdname geodist
#' @export
geoarea <- function(lat1, lon1, lat2, lon2, lat3, lon3, earth = 6371008.7714) {
  checkmate::assert_double(
    lat1, lower = -90, upper = 90, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon1, lower = -180, upper = 180, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lat2, lower = -90, upper = 90, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon2, lower = -180, upper = 180, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lat3, lower = -90, upper = 90, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon3, lower = -180, upper = 180, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_true(commensurable(c(
    length(lat1), length(lon1),
    length(lat2), length(lon2),
    length(lat3), length(lon3)
  )))
  checkmate::assert_number(earth, lower = 6335439.0000, upper = 6399593.6259)

  PI <- base::pi
  radius  <- 1e-3 * earth  # [km]

  d2r1 <- function(x) PI * (90 - unname(x)) / 180
  d2r2 <- function(x) PI * unname(x) / 180

  x <- function(lat, lon) sin(d2r1(lat)) * cos(d2r2(lon))
  y <- function(lat, lon) sin(d2r1(lat)) * sin(d2r2(lon))
  z <- function(lat)      cos(d2r1(lat))

  p1 <- cbind(x(lat1, lon1), y(lat1,lon1), z(lat1))
  p2 <- cbind(x(lat2, lon2), y(lat2,lon2), z(lat2))
  p3 <- cbind(x(lat3, lon3), y(lat3,lon3), z(lat3))

  2 * atan2(
    abs(
      rowSums(p1 * cbind(
        p2[, 2] * p3[, 3] - p2[, 3] * p3[, 2],
        p2[, 3] * p3[, 1] - p2[, 1] * p3[, 3],
        p2[, 1] * p3[, 2] - p2[, 2] * p3[, 1]
      ))
    ),
    1 + rowSums(p1 * p2) + rowSums(p2 * p3) + rowSums(p1 * p3)
  ) * radius^2
}



