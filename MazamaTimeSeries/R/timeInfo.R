#' @title Get time related information
#'
#' @description Calculate the local time at the target location, as well as
#' sunrise, sunset and solar noon times, and create several temporal masks.
#'
#' The returned dataframe will have as many rows as the length of the incoming
#' UTC \code{time} vector and will contain the following columns:
#'
#' \itemize{
#' \item{\code{localStdTime_UTC} -- UTC representation of local \strong{standard} time}
#' \item{\code{daylightSavings} -- logical mask = TRUE if daylight savings is in effect}
#' \item{\code{localTime} -- local clock time}
#' \item{\code{sunrise} -- time of sunrise on each localTime day}
#' \item{\code{sunset} -- time of sunset on each localTime day}
#' \item{\code{solarnoon} -- time of solar noon on each localTime day}
#' \item{\code{day} -- logical mask = TRUE between sunrise and sunset}
#' \item{\code{morning} -- logical mask = TRUE between sunrise and solarnoon}
#' \item{\code{afternoon} -- logical mask = TRUE between solarnoon and sunset}
#' \item{\code{night} -- logical mask = opposite of day}
#' }
#'
#' @details
#' NOAA used the reference below to develop their Sunrise/Sunset
#'
#' \url{https://gml.noaa.gov/grad/solcalc/sunrise.html} and Solar
#' Position
#'
#' \url{https://gml.noaa.gov/grad/solcalc/azel.html}
#' Calculators.  The algorithms include corrections for atmospheric
#' refraction effects.
#'
#' Input can consist of one location and at least one POSIXct times, or one
#' POSIXct time and at least one location.  \var{solarDep} is recycled as
#' needed.
#'
#' Do not use the daylight savings time zone string for supplying
#' \var{dateTime}, as many OS will not be able to properly set it to
#' standard time when needed.
#'
#' The \code{localStdTime_UTC} column in the returned dataframe is primarily for
#' internal use and provides an important tool for creating LST daily averages
#' and LST axis labeling.
#'
#' @note
#' NOAA notes that “for latitudes greater than 72 degrees N and S,
#' calculations are accurate to within 10 minutes. For latitudes less than +/-
#' 72 degrees accuracy is approximately one minute.”
#'
#' @section Attribution:
#' Internal functions used for ephemerides calculations were copied verbatim
#' from the now deprecated \strong{maptools} package source
#' code in an effort to reduce the number of package dependencies.
#'
#' @section Warning:
#' Compared to NOAA's original Javascript code, the sunrise and sunset estimates
#' from this translation may differ by +/- 1 minute, based on tests using
#' selected locations spanning the globe. This translation does not include
#' calculation of prior or next sunrises/sunsets for locations above the Arctic
#' Circle or below the Antarctic Circle.
#'
#' @section Local Standard Time:
#' US EPA regulations mandate that daily averages be calculated
#' based on "Local Standard Time" (LST) (\emph{i.e. never shifting to daylight
#' savings}). To ease work in a regulatory context, LST times are included in the
#' returned dataframe.
#'
#' @author
#' Sebastian P. Luque \email{spluque@gmail.com}, translated from
#' Greg Pelletier's \email{gpel461@ecy.wa.gov} VBA code (available from
#' \url{https://ecology.wa.gov/Research-Data/Data-resources/Models-spreadsheets/Modeling-the-environment/Models-tools-for-TMDLs}), who in turn
#' translated it from original Javascript code by NOAA (see Details).
#' Roger Bivand \email{roger.bivand@nhh.no} adapted the code to work with
#' \strong{sp} classes. Jonathan Callahan \email{jonathan.callahan@gmail.com}
#' adapted the source code from the \strong{maptools} package to work with
#' \href{https://mazamascience.github.io/MazamaTimeSeries/}{MazmaTimeSeries} classes.
#'
#' @section References:
#' Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
#'
#' @param time POSIXct vector with specified timezone,
#' @param longitude Longitude of the location of interest.
#' @param latitude Latitude of the location of interest.
#' @param timezone Olson timezone at the location of interest.
#'
#' @return A dataframe with times and masks.
#'
#' @importFrom rlang .data
#' @importFrom lubridate is.POSIXct
#' @export
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' Carmel <-
#'   Carmel_Valley %>%
#'   mts_filterDate(20160801, 20160810)
#'
#' # Create timeInfo object for this monitor
#' ti <- timeInfo(
#'   Carmel$data$datetime,
#'   Carmel$meta$longitude,
#'   Carmel$meta$latitude,
#'   Carmel$meta$timezone
#' )
#'
#' t(ti[6:9,])
#'
#' # Subset the data based on day/night masks
#' data_day <- Carmel$data[ti$day,]
#' data_night <- Carmel$data[ti$night,]
#'
#' # Build two monitor objects
#' Carmel_day <- list(meta = Carmel$meta, data = data_day)
#' Carmel_night <- list(meta = Carmel$meta, data = data_night)
#'
#' # Plot them
#' plot(Carmel_day$data, pch = 8, col = 'goldenrod')
#' points(Carmel_night$data, pch = 16, col = 'darkblue')

timeInfo <- function(
  time = NULL,
  longitude = NULL,
  latitude = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(time)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(timezone)

  if ( !lubridate::is.POSIXct(time) )
    stop(sprintf("'time' must be of class POSIXct"))

  if ( !is.numeric(longitude) )
    stop(sprintf("'longitude' must be of class numeric"))

  if ( !is.numeric(latitude) )
    stop(sprintf("'latitude' must be of class numeric"))

  if ( !(timezone %in% base::OlsonNames()) )
    stop(sprintf("timezone '%s' is not a valid Oson timezone", timezone))

  # ----- Solar times ----------------------------------------------------------

  # convert to local time
  localTime <- lubridate::with_tz(time, tzone = timezone)

  # sunriset reqires matrix or spatial object for input
  coords <- matrix(c(longitude, latitude), nrow = 1)

  # calculate sunrise, sunset, and solar noon times using fancy algorithm
  sunrise <- .maptools_sunriset(coords, localTime, direction = "sunrise", POSIXct.out = TRUE)
  sunset <- .maptools_sunriset(coords, localTime, direction = "sunset", POSIXct.out = TRUE)
  solarnoon <- .maptools_solarnoon(coords, localTime, POSIXct.out = TRUE)

  sunrise <- sunrise[,2] ; sunset <- sunset[,2] ; solarnoon <- solarnoon[,2]

  # create masks
  dayMask <- (localTime >= sunrise) & (localTime < sunset)
  nightMask <- !dayMask
  morningMask <- (localTime > sunrise) & (localTime <= solarnoon)
  afternoonMask <- (localTime > solarnoon) & (localTime <= sunset)

  # ----- localStandardTime_UTC ------------------------------------------------

  # NOTE:  The EPA defines regulatory daily averages as midnight-to-midnight
  # NOTE:  in local-standard-time-all-year. We add a column of data that
  # NOTE:  displays the proper clock time for LSTAY. This can then be used to
  # NOTE:  calculate, plot and label the EPA regulatory midnight-to-midnight
  # NOTE:  daily averages

  # Calculate the Local Standard Time offset
  Christmas_UTC <- lubridate::ymd_h("2019-12-25 00", tz = "UTC")
  Christmas_localTime <- lubridate::with_tz(Christmas_UTC, tzone = timezone)
  Christmas_localTime_UTC <- lubridate::force_tz(Christmas_localTime, tzone = "UTC")
  lst_offset <- as.numeric(difftime(Christmas_localTime_UTC, Christmas_UTC, units = "hours"))

  localStandardTime_UTC <- lubridate::with_tz(localTime, tzone = "UTC") +
    lst_offset * lubridate::dhours(1)

  # ----- Return ---------------------------------------------------------------

  # Assemble dataframe
  timeInfo <- data.frame(
    localStandardTime_UTC = localStandardTime_UTC,
    daylightSavings = lubridate::dst(localTime),
    localTime = localTime,
    sunrise = sunrise,
    sunset = sunset,
    solarnoon = solarnoon,
    day = dayMask,
    morning = morningMask,
    afternoon = afternoonMask,
    night = nightMask
  )

  return(timeInfo)

}
