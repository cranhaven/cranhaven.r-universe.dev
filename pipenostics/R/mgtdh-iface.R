#' @title
#'  Get ground temperature
#'
#' @family utils
#'
#' @description
#'  Get the undisturbed (median) value of ground temperature at different depths
#'  at a specified time leveraging \emph{Modified Ground Temperature Double Harmonic Model}
#' (\emph{MGTDH}-model).
#'
#' @details
#' The \emph{MGTDH}-model is a modified solution of the thermal conductivity equation for soil
#' and can be expressed by the formula
#' \deqn{t\left( \tau, d \right) = \beta e^{r_1} \cdot A_1 \cos(2 \pi \omega \tau + r_1 - P_1) + \beta r_2^{A_2 \cdot d} \cos(4 \pi \omega \tau + r_2 - P_2)}
#' where 
#'  \describe{
#'   \item{\eqn{t\left( \tau, d \right)}}{undisturbed (median) ground temperature [\emph{°C}] at specified depth \eqn{d} [\emph{m}], and time \eqn{\tau} [\emph{hour}].}
#'   \item{\eqn{\tau}}{time point (\code{tau}) calculated in hours since the beginning of the year, [\emph{hour}].}
#'   \item{\eqn{d}}{depth (\code{depth}) at which the ground temperature should be calculated, [\emph{m}].}
#'   \item{\eqn{\beta = -1}}{shift constant, [].}
#'   \item{\eqn{r_1(d) = -1000 d \sqrt{\frac{\pi \omega}{\alpha_s D}}}, \eqn{r_2(d) = r_1(d) \sqrt{2}}}{temperature diffusivity factors, [].}
#'   \item{\eqn{\omega = \frac{1}{8760}}}{rate of rotation of the \emph{Earth}, expressed with an accuracy equal to the inverse of an hour, [\emph{1/hour}].}
#'   \item{\eqn{\alpha_s}}{soil diffusivity, [\emph{mm^2/s}].}
#'   \item{\eqn{D = 86400}}{constant that represents the number of seconds in one day, [\emph{s/day}].}
#'   \item{\eqn{A_1}, \eqn{A_2}}{harmonic temperature amplitudes, [\emph{°C}].}
#'   \item{\eqn{P_1}, \eqn{P_2}}{phase shifts, depending on the geographical location, [].}
#'  }
#'
#'  Soil diffusivity, \eqn{\alpha_s}, harmonic temperature amplitudes, \eqn{A_1}, \eqn{A_2}, 
#'  and phase shifts \eqn{P_1}, \eqn{P_2} are geographically dependant parameters which values 
#'  were established for each weather station listed in \code{\link{meteos}}-dataset.
#'
#'  For the convenience of using the \emph{MGTDH}-model, several interface 
#'  functions have been provided. Each function generates a vector of type
#'  \code{\link{assert_double}} as an output. 
#'
#'  The \code{mgtdhid} and \code{mgtdhidt} functions are used to obtain 
#'  ground temperature data from specific meteorological stations. 
#'
#'  The functions \code{mgtdhgeo} and \code{mgtdhgeot} provide ground temperatures
#'  at any geographical location, but note that their usage is primarily limited
#'  to the Northern Asian part of \emph{Eurasia}, as most meteorological stations
#'  and parameters for the \emph{MGTDH}-model are established there. Ground temperature
#'  at the specified location is obtained by linear interpolation using barycentric
#'  coordinates formed in the system of the three nearest meteorological stations.
#'
#' @param id
#'  weather station unique identifier. Only identifiers from \code{\link{meteos}} dataset are accepted.
#'  Type: \code{\link{assert_integer}}. For \code{mgtdhidt} acceptable length is \code{1}.
#'
#' @param tau
#'  time point for which it is necessary to obtain the value of the soil
#'  temperature; it can be specified as an integer, representing the 
#'  number of hours that have passed since the beginning of the year, or 
#'  as a value of \code{\link{POSIXct}} type. 
#'  Type: \code{\link{assert_count}}, or \code{\link{assert_posixct}}. 
#'  For \code{mgtdhid} acceptable length is \code{1}.
#'
#' @param lat
#'  latitude of the geographical location where the value of soil temperature needs to be determined, [\emph{DD}].
#'  Type: \code{\link{assert_double}}. For \code{mgtdhgeot} acceptable length is \code{1}.
#'
#' @param lon
#'  longitude of the geographical location where the value of soil temperature needs to be determined, [\emph{DD}].
#'  Type: \code{\link{assert_double}}. For \code{mgtdhgeot} acceptable length is \code{1}.
#'
#' @param depth
#'  depth at which the ground temperature is calculated, [\emph{m}]. Type: \code{\link{assert_number}}.
#'
#' @param use_cluster
#'    utilize functionality of parallel processing on multi-core CPU.
#'    Type: \code{\link{assert_flag}}.
#'
#' @return
#'  Undisturbed (median) ground temperature value calculated with 
#'  the \emph{MGTDH}-model, specifically for the location of the 
#'  user-specified meteorological station, at specified depth, and time, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @references
#'  Lu Xing & Jeffrey D. Spitler (2017) \emph{Prediction of undisturbed ground temperature using analytical and numerical modeling. Part I: Model development and experimental validation}.
#'  Science and Technology for the Built Environment, 23:5, 787-808, \doi{10.1080/23744731.2016.1258371}.
#' 
#' @seealso
#'  \code{\link{geodist}} and \code{\link{geoarea}} for calculating geographical metrics.
#' 
#' @examples
#'  # Let consider the next geographical positions:
#'  lat <- c(s28434 = 56.65, s28418 = 56.47, s23711 = 62.70, ControlPoint = 57)
#'  lon <- c(s28434 = 57.78, s28418 = 53.73, s23711 = 56.20, ControlPoint = 57)
#'  
#'  # * ground temperatures at first three locations on 02 March 2023 at depth 3 m:
#'  mgtdhgeo(head(lat, 3), head(lon, 3), tau = as.POSIXct("2023-03-02"), depth = 3)
#' 
#'  # * it is the same as obtaining ground temperatures from weather stations:
#'  mgtdhid(id = c(28434L, 28418L, 23711L), tau = as.POSIXct("2023-03-02"), depth = 3)
#'
#'  # * undisturbed ground temperature plot at Control Point:
#'  days <- as.POSIXct("2023-01-01") + 3600*24*(seq.int(1, 365) - 1)
#'  plot(
#'    days, 
#'    mgtdhgeot(days, lat[["ControlPoint"]], lon[["ControlPoint"]]), 
#'    type = "l", 
#'    ylab = "Temperature, °C"
#'  )
#'
#' @rdname mgtdh-iface
#' @export
mgtdhid <- function(id, tau = 1440L, depth = 2.4){
  L_STATION_ID <- "station_id"
  L_YEAR_START <- "%Y-01-01 00:00:00"
  L_HOUR       <- 3600L  # [s]
  L_TIMEZONE   <- "UTC"
 
  checkmate::assert_integer(
    id, lower = 0, upper = 1e5, any.missing = FALSE, min.len = 1
  )
  if (checkmate::test_posixct(tau, any.missing = FALSE, len = 1)) {
    tau <- as.POSIXct(tau, tz = L_TIMEZONE)
    tau <- (
      as.integer(tau) - as.integer(as.POSIXct(format(tau, L_YEAR_START), tz = L_TIMEZONE))
    ) %/% L_HOUR
  }
  checkmate::assert_integer(
    tau, lower = 0, upper = 8784, any.missing = FALSE, len = 1
  )
  checkmate::assert_double(
    depth, lower = 0.3, upper = 10.0, any.missing = FALSE, len = 1
  )
  checkmate::assert_subset(id, meteosd[[L_STATION_ID]])

  s <- meteosd[as.character(id), ]
  mgtdh(
     x           = tau
    ,depth       = depth
    ,avg         = s[["avg"]]
    ,ampl1       = s[["ampl1"]]
    ,ampl2       = s[["ampl2"]]
    ,pl1         = s[["pl1"]]
    ,pl2         = s[["pl2"]]
    ,diffusivity = s[["diffusivity"]]
  )
}

#' @rdname mgtdh-iface
#' @export
mgtdhidt <- function(tau, id = 28434L, depth = 2.4){
  L_STATION_ID <- "station_id"
  L_YEAR_START <- "%Y-01-01 00:00:00"
  L_HOUR       <- 3600L  # [s]
  L_TIMEZONE   <- "UTC"
 
  if (checkmate::test_posixct(tau, any.missing = FALSE, min.len = 1)) {
    tau <- as.POSIXct(tau, tz = L_TIMEZONE)
    tau <- (
      as.integer(tau) - as.integer(as.POSIXct(format(tau, L_YEAR_START), tz = L_TIMEZONE))
    ) %/% L_HOUR
  }
  checkmate::assert_integer(
    tau, lower = 0, upper = 8784, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_integer(
    id, lower = 0, upper = 1e5, any.missing = FALSE, len = 1
  )
  checkmate::assert_double(
    depth, lower = 0.3, upper = 10.0, any.missing = FALSE, len = 1
  )
  checkmate::assert_choice(id, meteosd[[L_STATION_ID]])

  s <- meteosd[as.character(id),]
  mgtdh(
     x           = tau
    ,depth       = depth
    ,avg         = s[["avg"]]
    ,ampl1       = s[["ampl1"]]
    ,ampl2       = s[["ampl2"]]
    ,pl1         = s[["pl1"]]
    ,pl2         = s[["pl2"]]
    ,diffusivity = s[["diffusivity"]]
  )
}


#' @rdname mgtdh-iface
#' @export
mgtdhgeo <- function(lat, lon, tau = 1440L, depth = 2.4, use_cluster = FALSE){
  L_YEAR_START <- "%Y-01-01 00:00:00"
  L_HOUR       <- 3600L  # [s]
  L_TIMEZONE   <- "UTC"

  checkmate::assert_double(
    lat, lower = 42, upper = 68, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon, lower = 30, upper = 166, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_true(commensurable(c(length(lat), length(lon))))
  if (checkmate::test_posixct(tau, any.missing = FALSE, len = 1)) {
    tau <- as.POSIXct(tau, tz = L_TIMEZONE)
    tau <- (
      as.integer(tau) - as.integer(as.POSIXct(format(tau, L_YEAR_START), tz = L_TIMEZONE))
    ) %/% L_HOUR
  }
  checkmate::assert_integer(
    tau, lower = 0, upper = 8784, any.missing = FALSE, len = 1
  )
  checkmate::assert_double(
    depth, lower = 0.3, upper = 10.0, any.missing = FALSE, len = 1
  )
  if (use_cluster){
    cluster <- parallel::makeCluster(parallel::detectCores() - 1)
    stream <- parallel::clusterMap(
      cluster,
      function(x, y) pipenostics::mgtdhgeot(tau = tau, lat = x, lon = y, depth = depth),
      lat, lon,
      USE.NAMES = FALSE, SIMPLIFY = TRUE
    )
    parallel::stopCluster(cluster)
    return(stream)
  } else {
    unlist(
      Map(
        function(x, y) pipenostics::mgtdhgeot(tau = tau, lat = x, lon = y, depth = depth),
        lat, lon,
        USE.NAMES = FALSE
      )
    )
  }
}


#' @rdname mgtdh-iface
#' @export
mgtdhgeot <- function(tau, lat = 57, lon = 57,  depth = 2.4){
  L_YEAR_START <- "%Y-01-01 00:00:00"
  L_HOUR       <- 3600L  # [s]
  L_TIMEZONE   <- "UTC"

  if (checkmate::test_posixct(tau, any.missing = FALSE, min.len = 1)) {
    tau <- as.POSIXct(tau, tz = L_TIMEZONE)
    tau <- (
      as.integer(tau) - as.integer(as.POSIXct(format(tau, L_YEAR_START), tz = L_TIMEZONE))
    ) %/% L_HOUR
  }
  checkmate::assert_integer(
    tau, lower = 0, upper = 8784, any.missing = FALSE, min.len = 1
  )

  checkmate::assert_double(
    lat, lower = 42, upper = 68, any.missing = FALSE, len = 1
  )
  checkmate::assert_double(
    lon, lower = 30, upper = 166, any.missing = FALSE, len = 1
  )

  checkmate::assert_double(
    depth, lower = 0.3, upper = 10.0, any.missing = FALSE, len = 1
  )

  L_STATION_ID <- "station_id"
  L_NEAREST_N  <- 3L

  COMMON_STATION_DISTANCE <- 1000000L  # [m]
  STATION_RADIUS          <- 5000L     # [m]

  POW <- .Primitive("^")

  # Barycentric coordinate system converter
  f <- function(x, y) POW(1 + POW(x, 2) + POW(y, 2), -1)

  r <- sort(
    structure(
      pipenostics::geodist(lat, lon, meteosd[["lat"]], meteosd[["lon"]]),
      names = meteosd[[L_STATION_ID]]
    )
  )[seq_len(L_NEAREST_N)]
  checkmate::assert_true(any(r < COMMON_STATION_DISTANCE))
  
  station_id <- names(r)
  is.outside <- !geointri(
    lat, lon,
    lat1 = meteosd[meteosd[[L_STATION_ID]] == station_id[[1]], "lat"],
    lon1 = meteosd[meteosd[[L_STATION_ID]] == station_id[[1]], "lon"],
    lat2 = meteosd[meteosd[[L_STATION_ID]] == station_id[[2]], "lat"],
    lon2 = meteosd[meteosd[[L_STATION_ID]] == station_id[[2]], "lon"],
    lat3 = meteosd[meteosd[[L_STATION_ID]] == station_id[[3]], "lat"],
    lon3 = meteosd[meteosd[[L_STATION_ID]] == station_id[[3]], "lon"]
  )
  is.barycentricable <- all(r > STATION_RADIUS) && !is.outside
  lambda <- is.barycentricable * c(
    f(r[[1]]/r[[2]], r[[1]]/r[[3]]),
    f(r[[2]]/r[[1]], r[[2]]/r[[3]]),
    f(r[[3]]/r[[1]], r[[3]]/r[[2]])
  ) + (!is.barycentricable) * c(1, 0, 0)

  drop(
    cbind(
       mgtdhidt(id = as.integer(station_id[[1]]), tau, depth)
      ,mgtdhidt(id = as.integer(station_id[[2]]), tau, depth)
      ,mgtdhidt(id = as.integer(station_id[[3]]), tau, depth)
    ) %*% lambda
  )
}

