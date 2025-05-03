#' @export
#' @importFrom rlang .data
#' @importFrom dplyr across everything
#'
#' @title Create summary time series for an \emph{mts} time series object
#'
#' @param mts \emph{mts} object.
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Unit used to summarize by (\emph{e.g.} "day").
#' @param FUN Function used to summarize time series.
#' @param ... Additional arguments to be passed to \code{FUN}
#' (_e.g._ \code{na.rm = TRUE}).
#' @param minCount Minimum number of valid data records required to calculate
#' summaries. Time periods with fewer valid records will be assigned \code{NA}.
#'
#' @return An \emph{mts} time series object containing daily (or other)
#' statistical summaries.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description
#' Individual time series in \code{mts$data} are grouped by \code{unit} and then
#' summarized using \code{FUN}.
#'
#' The most typical use case is creating daily averages where each day begins at
#' midnight. This function interprets times using the \code{mts$data$datetime}
#' \code{tzone} attribute so be sure that is set properly.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, the most common (hopefully only!) time zone found in
#' \code{mts$meta$timezone}. Leaving \code{timezone = NULL}, the default,
#' results in "local time" date filtering which is the most common use case.
#'
#' @note
#' Because the returned \emph{mts} object is defined on a daily axis in a
#' specific time zone, it is important that the incoming \code{mts} contain
#' timeseries associated with a single time zone.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' daily <-
#'   mts_summarize(
#'     mts = Carmel_Valley,
#'     timezone = NULL,
#'     unit = "day",
#'     FUN = mean,
#'     na.rm = TRUE,
#'     minCount = 18
#'   )
#'
#' # Daily means
#' head(daily$data)
#'

mts_summarize <- function(
  mts,
  timezone = NULL,
  unit = c("day", "week", "month", "year"),
  FUN = NULL,
  ...,
  minCount = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)
  unit <- match.arg(unit)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(minCount)

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  if ( length(unique(mts$meta$timezone)) > 1 )
    stop("'mts' has muliple timezones")

  # Use internal function to determine the timezone to use
  timezone <- .determineTimezone(mts, NULL, timezone, verbose = TRUE)

  # ----- Summarize by time period ---------------------------------------------

  # See:  https://www.statology.org/aggregate-daily-data-in-r/
  # See:  https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html
  # See:  https://dplyr.tidyverse.org/articles/colwise.html

  customFUN <- function(x, ...) {
    if ( sum(!is.na(x)) >= minCount ) {
      return(FUN(x, ...))
    } else {
      return(NA)
    }
  }

  newData <-

    mts$data %>%

    # Create 'timeUnit' in the desired timezone as a grouping variable
    dplyr::mutate(
      timeUnit = lubridate::floor_date(lubridate::with_tz(.data$datetime, tz = timezone), unit)
    ) %>%
    dplyr::select(-.data$datetime) %>%
    dplyr::group_by(.data$timeUnit) %>%

    # Summarize using FUN (will ignore 'timeUnit' column)
    dplyr::summarize(across(everything(), customFUN, ...)) %>%

    # Replace +/-Inf and NaN with NA
    dplyr::mutate(across(everything(), function(x) { x[!is.finite(x)] <- NA; return(x) })) %>%

    # New, daily 'datetime'
    dplyr::rename(datetime = .data$timeUnit)


  # ----- Create the 'mts' object ----------------------------------------------

  mts$data <- newData

  # ----- Return ---------------------------------------------------------------

  return(invisible(mts))

}
