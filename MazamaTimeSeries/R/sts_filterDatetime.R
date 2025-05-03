#' @export
#' @importFrom rlang .data
#'
#' @title Datetime filtering for \emph{sts} time series objects
#'
#' @param sts MazamaSingleTimeseries \emph{sts} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param includeEnd Logical specifying that records associated with \code{enddate}
#' should be included.
#'
#' @description Subsets a MazamaSingleTimeseries object by datetime. This function
#' allows for sub-day filtering as opposed to \code{sts_filterDate()} which
#' always filters to day-boundaries.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
#' the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.
#'
#' Timezone determination precedence assumes that if you are passing in
#' \code{POSIXct} values then you know what you are doing.
#'
#' \enumerate{
#' \item{get timezone from \code{startdate} if it is \code{POSIXct}}
#' \item{use passed in \code{timezone}}
#' \item{get timezone from \code{sts}}
#' }
#'
#' @note The returned \code{sts} object will contain data running from the
#' beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. To include \code{enddate} you can
#' specify \code{includeEnd = TRUE}.
#'
#' @return A subset of the incoming \emph{sts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{sts_filter}
#' @seealso \link{sts_filterDate}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' example_sts %>%
#'   sts_filterDatetime(
#'     startdate = "2018-08-08 06:00:00",
#'     enddate = "2018-08-14 18:00:00"
#'   ) %>%
#'   sts_extractData() %>%
#'   head()
#'

sts_filterDatetime <- function(
  sts = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE,
  includeEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sts)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)

  if ( !sts_isValid(sts) )
    stop("Parameter 'sts' is not a valid 'sts' object.")

  # Return the sts if it is empty so pipelines don't break
  if ( sts_isEmpty(sts) )
    return(sts)

  # Remove any duplicate data records
  sts <- sts_distinct(sts)

  # Use internal function to determine the timezone to use
  timezone <- .determineTimezone(sts, startdate, timezone, verbose = TRUE)

  # ----- Get the start and end times ------------------------------------------

  timeRange <- MazamaCoreUtils::timeRange(
    starttime = startdate,
    endtime = enddate,
    timezone = timezone,
    unit = unit,
    ceilingStart = ceilingStart,
    ceilingEnd = ceilingEnd
  )

  # ----- Subset the 'sts' object ----------------------------------------------

  # NOTE:  When processing lots of data automatically, it is best not to stop()
  # NOTE:  when no data exist for a requested date range. Instead, return a
  # NOTE:  valid 'sts' object with zero rows of data.

  if (timeRange[1] > sts$data$datetime[length(sts$data$datetime)] |
      timeRange[2] < sts$data$datetime[1]) {

    message(sprintf(
      "sts (%s) does not contain requested time range",
      sts$meta$locationName
    ))

    data <- sts$data[0,]

  } else {

    if ( includeEnd ) {
      data <-
        sts$data %>%
        dplyr::filter(.data$datetime >= timeRange[1]) %>%
        dplyr::filter(.data$datetime <= timeRange[2])
    } else {
      data <-
        sts$data %>%
        dplyr::filter(.data$datetime >= timeRange[1]) %>%
        dplyr::filter(.data$datetime < timeRange[2])
    }

  }

  sts$data <- data

  # ----- Return ---------------------------------------------------------------

  return(sts)

}
