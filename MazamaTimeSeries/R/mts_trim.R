#' @export
#' @importFrom rlang .data
#'
#' @title Trim \emph{mts} time series by removing missing values
#'
#' @param mts \emph{mts} object.
#'
#' @description Trims the time range of an \emph{mts} object by removing
#' time steps from the start and end that contain only missing values.
#'
#' @return A subset of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Untrimmed range
#' range(example_mts$data$datetime)
#'
#' # Replace the first 50 data values for all non-"datetime" columns
#' example_mts$data[1:50, -1] <- NA
#'
#' # Trimmed range
#' mts_trimmed <- mts_trim(example_mts)
#' range(mts_trimmed$data$datetime)

mts_trim <- function(
  mts = NULL
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

  # ----- Trim empty timesteps -------------------------------------------------

  # NOTE:  Use dplyr::select to ensure that single-time series objects remain
  # NOTE:  as tibbles and don't get converted to vectors.

  dataBrick <- dplyr::select(mts$data, -1) # remove 'datetime' column

  # Find records with any non-missing data values
  hasData <- apply(dataBrick, 1, function(x) { !all(is.na(x)) })
  firstIndex <- min(which(hasData))
  lastIndex <- max(which(hasData))
  mts$data <- dplyr::slice(mts$data, firstIndex:lastIndex)

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
