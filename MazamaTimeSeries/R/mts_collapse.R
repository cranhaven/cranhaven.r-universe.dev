#' @export
#' @importFrom rlang .data
#' @importFrom methods as
#'
#' @title Collapse an \emph{mts} time series object into a single time series
#'
#' @param mts \emph{mts} object.
#' @param longitude Longitude of the collapsed time series.
#' @param latitude Latitude of the collapsed time series.
#' @param deviceID Device identifier for the collapsed time series.
#' @param FUN Function used to collapse multiple time series.
#' @param na.rm Logical specifying whether NA values should be ignored when FUN
#' is applied.
#' @param ... additional arguments to be passed on to the \code{apply()} function.
#'
#' @return An \emph{mts} time series object representing a single time series.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description
#' Collapses data from all time series in \code{mts} into a
#' single-time series \emph{mts} object using the function provided in the
#' \code{FUN} argument. The single-time series result will be located at the mean
#' longitude and latitude unless \code{longitude} and \code{latitude}
#' are specified.
#'
#' Any columns of \code{mts$meta} that are constant across all records will be
#' retained in the returned \code{mts$meta}.
#'
#' The core metadata associated with this location (\emph{e.g.}
#' \code{countryCode, stateCode, timezone, ...}) will be determined from
#' the most common (or average) value found in \code{mts$meta}. This will be
#' a reasonable assumption for the vast majority of intended use cases where
#' data from multiple devices in close proximity are averaged together.
#'
#' @note
#' After \code{FUN} is applied, values of \code{+/-Inf} and \code{NaN} are
#' converted to \code{NA}. This is a convenience for the common case where
#' \code{FUN = min/max} or \code{FUN = mean} and some of the time steps have all
#' missing values. See the R documentation for \code{min} for an explanation.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' mon <-
#'   mts_collapse(
#'     mts = example_mts,
#'     deviceID = "example_ID"
#'   )
#'
#' # mon$data now only has 2 columns
#' names(mon$data)
#'
#' plot(mon$data, type = 'b', main = mon$meta$deviceID)
#'

mts_collapse <- function(
  mts,
  longitude = NULL,
  latitude = NULL,
  deviceID = "generatedID",
  FUN = mean,
  na.rm = TRUE,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)
  deviceID <- MazamaCoreUtils::setIfNull(deviceID, "generatedID")
  MazamaCoreUtils::stopIfNull(FUN)
  na.rm <- MazamaCoreUtils::setIfNull(na.rm, TRUE)

  if ( !is.null(longitude) && !is.null(latitude) ) {
    MazamaCoreUtils::validateLonLat(longitude, latitude)
  } else {
    longitude <- latitude <- NULL
  }

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  # ----- Generate meta --------------------------------------------------------

  meta <- mts$meta

  if ( !is.null(longitude) & !is.null(latitude) ) {
    newLat <- latitude
    newLon <- longitude
  } else {
    newLat <- mean(meta$latitude, na.rm = TRUE)
    newLon <- mean(meta$longitude, na.rm = TRUE)
  }

  locationID <- MazamaCoreUtils::createLocationID(newLon, newLat)

  deviceDeploymentID <- sprintf("%s_%s", locationID, deviceID)

  elevation <- mean(meta$elevation, na.rm = TRUE)
  if ( !is.finite(elevation) ) elevation <- as.numeric(NA)

  # Build new meta dataframe
  newMeta <-
    dplyr::tibble(
      deviceDeploymentID = deviceDeploymentID,
      deviceID = deviceID,
      locationID = locationID,
      locationName = as.character(NA),
      longitude = newLon,
      latitude = newLat,
      elevation = elevation,
      countryCode = .mostFrequentValue(mts, "countryCode"),
      stateCode = .mostFrequentValue(mts, "stateCode"),
      timezone = .determineTimezone(mts)
    )

  # Add all other metadata columns, retaining values that are shared
  extraColumns <- setdiff(colnames(meta), colnames(newMeta))
  for ( column in extraColumns ) {
    if ( length(unique(meta[[column]])) == 1 ) {
      newMeta[[column]] <- unique(meta[[column]])
    } else {
      if ( "POSIXt" %in% class(meta[[column]]) ) {
        newMeta[[column]] <- as.POSIXct(NA)
      } else {
        newMeta[[column]] <- as(NA, class(meta[[column]]))
      }
    }
  }

  newMeta <-
    newMeta %>%
    dplyr::select(dplyr::all_of(c(requiredMetaNames, extraColumns)))

  # ----- Collapse data --------------------------------------------------------

  data <- as.matrix(mts$data[,-1])

  collapsedData <- suppressWarnings({
    apply(data, MARGIN = 1, FUN = FUN, na.rm = na.rm, ...)
  })

  # Special handling for min/max/mean which return +/-Inf and NaN when any row has all NAs
  collapsedData[!is.finite(collapsedData)] <- NA

  newData <-
    dplyr::tibble(
      datetime = mts$data$datetime,
      dummyName = collapsedData
    )

  # Update newData with deviceDeploymentID
  colnames(newData) <- c('datetime', deviceDeploymentID)

  # ----- Create the 'mts' object ----------------------------------------------

  mts <- list(meta = newMeta, data = newData)
  class(mts) <- union("mts", class(mts))

  # ----- Return ---------------------------------------------------------------

  return(invisible(mts))

}
