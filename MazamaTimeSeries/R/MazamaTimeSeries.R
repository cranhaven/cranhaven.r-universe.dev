#'
#' @docType package
#' @name MazamaTimeSeries
#' @aliases MazamaTimeSeries-package
#' @title Core functionality for environmental time series
#' @description
#' Utility functions for working with environmental time series data from known
#' locations. The compact data model is structured as a list with two dataframes. A
#' meta' dataframe contains spatial and measuring device metadata associated with
#' deployments at known locations. A 'data' dataframe contains a 'datetime' column
#' followed by columns of measurements associated with each "device-deployment".
NULL

# ----- Internal Data ----------------------------------------------------------

#' requiredMetaNames
#'
#' @export
#' @docType data
#' @name requiredMetaNames
#' @title Required columns for the 'meta' dataframe
#' @format A vector with 10 elements
#' @description The 'meta' dataframe found in \emph{sts} and \emph{mts} objects
#' is required to have a minimum set of information for proper functioning of
#' the package. The names of these columns are specified in
#' \code{requiredMetaNames} and include:
#'
#' \itemize{
#'   \item{\code{deviceDeploymentID} -- unique identifier (see \href{https://mazamascience.github.io/MazamaLocationUtils/}{MazmaLocationUtils})}
#'   \item{\code{deviceID} -- device identifier}
#'   \item{\code{locationID} -- location identifier (see \href{https://mazamascience.github.io/MazamaLocationUtils/}{MazmaLocationUtils})}
#'   \item{\code{locationName} -- English language name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{timezone} -- Olson time zone}
#' }
#'

requiredMetaNames <- c(
  "deviceDeploymentID",
  "deviceID",
  "locationID",
  "locationName",
  "longitude",
  "latitude",
  "elevation",
  "countryCode",
  "stateCode",
  "timezone"
)

