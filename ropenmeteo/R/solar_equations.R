cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
  et <- equation_of_time(doy)
  merid  <- floor(lon / 15) * 15
  merid[merid < 0] <- merid[merid < 0] + 15
  lc     <- (lon - merid) * -4/60  ## longitude correction
  tz     <- merid / 360 * 24  ## time zone
  midbin <- 0.5 * dt / 86400 * 24  ## shift calc to middle of bin
  t0   <- 12 + lc - et - tz - midbin  ## solar time
  h    <- pi/12 * (hr - t0)  ## solar hour
  dec  <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)  ## declination
  cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
  cosz[cosz < 0] <- 0
  return(cosz)
}

equation_of_time <- function(doy) {
  stopifnot(doy <= 367)
  f      <- pi / 180 * (279.5 + 0.9856 * doy)
  et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
               sin(4 * f) - 429.3 * cos(f) - 2 *
               cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
  return(et)
}

downscale_solar_geom <- function(doy, lon, lat) {

  dt <- stats::median(diff(doy)) * 86400 # average number of seconds in time interval
  hr <- (doy - floor(doy)) * 24 # hour of day for each element of doy

  ## calculate potential radiation
  cosz <- cos_solar_zenith_angle(doy, lat, lon, dt, hr)
  rpot <- 1366 * cosz
  return(rpot)
}
