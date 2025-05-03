#' @export
#' @importFrom rlang .data
#'
#' @title Datetime filtering for \emph{mts} time series objects
#'
#' @param mts \emph{mts} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Datetimes will be rounded to the nearest \code{unit}.
#' @param ceilingStart Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}} when rounding.
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}} when rounding.
#' @param includeEnd Logical specifying that records associated with \code{enddate}
#' should be included.
#'
#' @description \strong{\code{DEPRECATED -- use \link{mts_setTimeAxis}}}.
#'
#' Subsets an \code{mts} object by datetime. This function
#' allows for sub-day filtering as opposed to \code{mts_filterDate()} which
#' always filters to day-boundaries. Both the \code{startdate} and the
#' \code{enddate} will be included in the subset.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
#' the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.
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
#' @note This function is deprecated as of \strong{MazamaTimeSeries 0.2.15}.
#' Please use \link{mts_setTimeAxis} to shorten or lengthen the time axis
#' of an \emph{mts} object.
#'
# @note The returned \code{mts} object will contain data running from the
# beginning of \code{startdate} until
# the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
# with \code{enddate} will be returned. To include \code{enddate} you can
# specify \code{includeEnd = TRUE}.
#'
#' @return A subset of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{mts_filterData}
#' @seealso \link{mts_filterDate}
#' @seealso \link{mts_filterMeta}
#
# @examples
# library(MazamaTimeSeries)
#
# example_mts %>%
#   mts_filterDatetime(
#     startdate = "2019-07-03 06:00:00",
#     enddate = "2019-07-06 18:00:00"
#   ) %>%
#   mts_extractData() %>%
#   dplyr::pull(datetime) %>%
#   range()
#

mts_filterDatetime <- function(
  mts = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE,
  includeEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)

  if ( !mts_isValid(mts) )
    stop("'mts' is not a valid 'mts' object")

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  # Remove any duplicate data records
  mts <- mts_distinct(mts)

  # Use internal function to determine the timezone to use
  timezone <- .determineTimezone(mts, startdate, timezone, verbose = TRUE)

  # ----- Get the start and end times ------------------------------------------

  timeRange <- MazamaCoreUtils::timeRange(
    starttime = startdate,
    endtime = enddate,
    timezone = timezone,
    unit = unit,
    ceilingStart = ceilingStart,
    ceilingEnd = ceilingEnd
  )

  # ----- Subset the 'mts' object ----------------------------------------------

  # NOTE:  When processing lots of data automatically, it is best not to stop()
  # NOTE:  when no data exist for a requested date range. Instead, return a
  # NOTE:  valid 'mts' object with zero rows of data.

  if (timeRange[1] > mts$data$datetime[length(mts$data$datetime)] |
      timeRange[2] < mts$data$datetime[1]) {

    message(sprintf("mts does not contain the requested time range"))

    data <- mts$data[0,]

  } else {

    if ( includeEnd ) {
      data <-
        mts$data %>%
        dplyr::filter(.data$datetime >= timeRange[1]) %>%
        dplyr::filter(.data$datetime <= timeRange[2])
    } else {
      data <-
        mts$data %>%
        dplyr::filter(.data$datetime >= timeRange[1]) %>%
        dplyr::filter(.data$datetime < timeRange[2])
    }

  }

  mts$data <- data

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
