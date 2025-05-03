#' @export
#' @importFrom rlang .data
#'
#' @title Extend/contract \emph{mts} time series to new start and end times
#'
#' @param mts \emph{mts} object.
#' @param startdate Desired start date (ISO 8601).
#' @param enddate Desired end date (ISO 8601).
#' @param timezone Olson timezone used to interpret \code{startdate} and \code{enddate}.
#'
#' @description Extends or contracts the time range of an \emph{mts} object by
#' adding/removing time steps at the start and end and filling any new time
#' steps with missing values. The resulting time axis is guaranteed to be
#' a regular, hourly axis with no gaps using the same timezone as the incoming
#' \emph{mts} object. This is useful when you want to place separate \emph{mts}
#' objects on the same time axis for plotting.
#'
#' Dates can be anything that is understood by \code{MazamaCoreUtils::parseDatetime()}
#' including either of the following recommended formats:
#'
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#'
#' Timezone determination precedence assumes that if you are passing in
#' \code{POSIXct} values then you know what you are doing:
#'
#' \enumerate{
#' \item{get timezone from \code{startdate} if it is \code{POSIXct}}
#' \item{use passed in \code{timezone}}
#' \item{get timezone from \code{mts}}
#' }
#'
#' If either \code{startdate} or \code{enddate} is missing, the start or end of
#' the timeseries in \code{mts} will be used.
#'
#' If neither \code{startdate} nor \code{enddate} is a \code{POSIXct} value
#' AND no \code{timezone} is supplied, the timezone will be inferred from
#' the most common timezone found in \code{mts}.
#'
#' @return The incoming \emph{mts} time series object defined on a new time axis.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Default range
#' range(example_mts$data$datetime)
#'
#' # One-sided extend with user specified timezone
#' example_mts %>%
#'   mts_setTimeAxis(enddate = 20190815, timezone = "UTC") %>%
#'   mts_extractData() %>%
#'   dplyr::pull(datetime) %>%
#'   range()
#'
#' # Two-sided extend with user specified timezone
#' example_mts %>%
#'   mts_setTimeAxis(20190615, 20190815, timezone = "UTC") %>%
#'   mts_extractData() %>%
#'   dplyr::pull(datetime) %>%
#'   range()
#'
#' # Two-sided extend without timezone (uses timezone from mts$meta$timezone)
#' example_mts %>%
#'   mts_setTimeAxis(20190615, 20190815) %>%
#'   mts_extractData() %>%
#'   dplyr::pull(datetime) %>%
#'   range()
#'

mts_setTimeAxis <- function(
  mts = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'mts'

  result <- try({
    if ( !mts_isValid(mts) )
      stop("first argument is not a valid 'mts' object")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'mts' object?)"))
    }
  }

  if ( mts_isEmpty(mts) )
    stop("'mts' has no data")

  # Remove any duplicate data records
  mts <- mts_distinct(mts)

  if ( is.null(startdate) && is.null(enddate) )
    stop("at least one of 'startdate' or 'enddate' must be specified")

  # Use internal function to determine the timezone to use
  if ( !is.null(startdate) ) {
    timezone <- .determineTimezone(mts, startdate, timezone, verbose = TRUE)
  } else {
    timezone <- .determineTimezone(mts, enddate, timezone, verbose = TRUE)
  }

  # ----- Alter the time axis --------------------------------------------------

  time_axis_timezone <- lubridate::tz(mts$data$datetime[1])

  if ( is.null(startdate) ) {
    startdate <- min(mts$data$datetime)
  } else {
    startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
  }

  if ( is.null(enddate) ) {
    enddate <- max(mts$data$datetime)
  } else {
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
  }

  # Create a dataframe for hours
  hourlyDF <- data.frame(seq(startdate, enddate, by = "hours"))
  names(hourlyDF) <- "datetime"

  # Merge the data onto the new time axis with a left join
  mts$data <- dplyr::left_join(hourlyDF, mts$data, by = "datetime")

  # Make sure we retain the original timezone from mts$data$datetime
  mts$data$datetime <- lubridate::with_tz(mts$data$datetime, tzone = time_axis_timezone)

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
