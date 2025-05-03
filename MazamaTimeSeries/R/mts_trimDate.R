#' @export
#' @importFrom rlang .data
#'
#' @title Trim \emph{mts} time series object to full days
#'
#' @param mts \emph{mts} object.
#' @param timezone Olson timezone used to interpret dates.
#' @param trimEmptyDays Logical specifying whether to remove days with no data
#' at the beginning and end of the time range.
#'
#' @description Trims the date range of an \emph{mts} object to local time date
#' boundaries which are within the time range of the \emph{mts} object.
#' This has the effect of removing partial-day data records at the start and
#' end of the timeseries and is useful when calculating full-day statistics.
#'
#' By default, multi-day periods of all-missing data at the beginning and end
#' of the timeseries are removed before trimming to date boundaries. If
#' \code{trimEmptyDays = FALSE} all records are retained except for partial days
#' beyond the first and after the last date boundary.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL},  \code{mts$meta$timezone}. Leaving \code{timezone = NULL}, the
#' default, results in "local time" date filtering which is the most
#' common use case.
#'
#' @return A subset of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' UTC_week <- mts_filterDate(
#'   example_mts,
#'   startdate = 20190703,
#'   enddate = 20190706,
#'   timezone = "UTC"
#' )
#'
#' # UTC day boundaries
#' range(UTC_week$data$datetime)
#'
#' # Trim to local time day boundaries
#' local_week <- mts_trimDate(UTC_week)
#' range(local_week$data$datetime)
#'

mts_trimDate <- function(
  mts = NULL,
  timezone = NULL,
  trimEmptyDays = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)

  if ( !mts_isValid(mts) )
    stop("Parameter 'mts' is not a valid 'mts' object.")

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  # Remove any duplicate data records
  mts <- mts_distinct(mts)

  # Use internal function to determine the timezone to use
  timezone <- .determineTimezone(mts, NULL, timezone, verbose = TRUE)

  # ----- Trim empty days ------------------------------------------------------

  if ( trimEmptyDays ) {

    # NOTE:  Use dplyr::select to ensure that single-time series objects remain
    # NOTE:  as tibbles and don't get converted to vectors.

    dataBrick <- dplyr::select(mts$data, -1)

    # Find records with any non-missing data values
    hasData <- apply(dataBrick, 1, function(x) { !all(is.na(x)) })
    firstIndex <- min(which(hasData))
    lastIndex <- max(which(hasData))
    mts$data <- dplyr::slice(mts$data, firstIndex:lastIndex)

  }

  # ----- Get the start and end times ------------------------------------------

  timeRange <- range(mts$data$datetime)

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

  # ----- Subset the "mts" object ----------------------------------------------

  data <-
    mts$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])

  mts$data <- data

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
