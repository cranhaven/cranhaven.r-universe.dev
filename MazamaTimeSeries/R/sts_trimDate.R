#' @export
#' @importFrom rlang .data
#'
#' @title Trim \emph{sts} time series object to full days
#'
#' @param sts SingleTimeSeries \emph{sts} object.
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description Trims the date range of a \emph{sts} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, from \code{sts$meta$timezone}.
#'
#' @return A subset of the incoming \emph{sts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' UTC_week <- sts_filterDate(
#'   example_sts,
#'   startdate = 20180808,
#'   enddate = 20180815,
#'   timezone = "UTC"
#' )
#'
#' # UTC day boundaries
#' head(UTC_week$data)
#'
#' # Trim to local time day boundaries
#' local_week <- sts_trimDate(UTC_week)
#' head(local_week$data)
#'

sts_trimDate <- function(
  sts = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sts)

  if ( !sts_isValid(sts) )
    stop("'sts' is not a valid 'sts' object")

  # Return the sts if it is empty so pipelines don't break
  if ( sts_isEmpty(sts) )
    return(sts)

  # Remove any duplicate data records
  sts <- sts_distinct(sts)

  # Use internal function to determine the timezone to use
  timezone <- .determineTimezone(sts, NULL, timezone, verbose = TRUE)

  # ----- Get the start and end times ------------------------------------------

  timeRange <- range(sts$data$datetime)

  # NOTE:  The dateRange() is used to restrict the time range to days that have
  # NOTE:  complete data.
  # NOTE:
  # NOTE:  floor/ceiling the start date depending on whether you are already
  # NOTE:  at the date boundary

  hour <-
    MazamaCoreUtils::parseDatetime(timeRange[1], timezone = timezone) %>%
    lubridate::hour() # hour resolution is good enough to count as an entire day

  if ( hour == 0 ) {
    ceilingStart = FALSE
  } else {
    ceilingStart = TRUE
  }

  dateRange <-
    MazamaCoreUtils::dateRange(
      startdate = timeRange[1],
      enddate = timeRange[2],
      timezone = timezone,
      unit = "sec",
      ceilingStart = ceilingStart, # date boundary *after* the start
      ceilingEnd = FALSE           # date boundary *before* the end
    )

  # ----- Subset the "sts" object ----------------------------------------------

  data <-
    sts$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])

  sts$data <- data

  # ----- Return ---------------------------------------------------------------

  return(sts)

}
