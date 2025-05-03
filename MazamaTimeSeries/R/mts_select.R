#' @export
#' @importFrom rlang .data
#'
#' @title Reorder and subset time series within an \emph{mts} time series object
#'
#' @param mts \emph{mts} object.
#' @param deviceDeploymentID Vector of timeseries unique identifiers.
#'
#' @description
#' This function acts similarly to \code{dplyr::select()} working on
#' \code{mts$data}. The returned \emph{mts} object will contain only those
#' time series identified by \code{deviceDeploymentID} in the order specified.
#'
#' This can be used the specify a preferred order and is helpful when using
#' faceted plot functions based on
#' \href{https://ggplot2.tidyverse.org}{ggplot}
#' such as those found in the
#' \href{https://mazamascience.github.io/AirMonitorPlots/}{AirMonitorPlots} package.
#'
#' @return A reordered (subset) of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{mts_selectWhere}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Filter for "El Monte"
#' El_Monte <-
#'   example_mts %>%
#'   mts_filterMeta(communityRegion == "El Monte")
#'
#' ids <- El_Monte$meta$deviceDeploymentID
#' rev_ids <- rev(ids)
#'
#' print(ids)
#' print(rev_ids)
#'
#' rev_El_Monte <-
#'   example_mts %>%
#'   mts_select(rev_ids)
#'
#' print(rev_El_Monte$meta$deviceDeploymentID)
#'

mts_select <- function(
  mts = NULL,
  deviceDeploymentID = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)
  MazamaCoreUtils::stopIfNull(deviceDeploymentID)

  # No duplicate IDs allowed
  if ( any(duplicated(deviceDeploymentID)) ) {
    duplicateIDs <- deviceDeploymentID[duplicated(deviceDeploymentID)]
    stop(sprintf(
      "Duplicate IDs found in 'deviceDeploymentID': %s",
      paste(duplicateIDs, collapse = ", ")
    ))
  }

  # Warning message if missing ID is requested
  if ( !all(deviceDeploymentID %in% mts$meta$deviceDeploymentID) ) {
    missingIndices <- which(!deviceDeploymentID %in% mts$meta$deviceDeploymentID)
    missingIDs <- deviceDeploymentID[missingIndices]
    warning(sprintf(
      "Requested IDs not found in 'meta': %s\n  The returned object will have fewer time series than the number of 'deviceDeploymentIDs' requested.",
      paste(missingIDs, collapse = ", ")
    ))
  }

  # ----- Filter and reorder ---------------------------------------------------

  mts <-
    mts %>%
    mts_filterMeta(.data$deviceDeploymentID %in% !!deviceDeploymentID)

  mts$meta <-
    mts$meta %>%
    dplyr::arrange(factor(.data$deviceDeploymentID, levels = !!deviceDeploymentID))

  mts$data <-
    mts$data %>%
    dplyr::select(dplyr::all_of(c("datetime", mts$meta$deviceDeploymentID)))

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
