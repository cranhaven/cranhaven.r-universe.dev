#' @export
#'
#' @title Check \emph{sts} object for validity
#'
#' @param sts \emph{sts} object.
#'
#' @description Checks on the validity of an \emph{sts} object. If any test
#' fails, this function will stop with a warning message.
#'
#' @return Returns \code{TRUE} invisibly if the \emph{sts} object is valid.
#'
#' @seealso \link{sts_isValid}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' sts_check(example_sts)
#'
#' # This would throw an error
#' if ( FALSE ) {
#'
#'   broken_sts <- example_sts
#'   names(broken_sts) <- c('meta', 'bop')
#'   sts_check(broken_sts)
#'
#' }
#'

sts_check <- function(sts) {
  tryCatch(
    sts_isValid(sts, verbose = TRUE),
    warning = function(w) stop(w),
    finally = invisible(TRUE)
  )
}


#' @export
#'
#' @name sts_isValid
#' @title Test \emph{sts} object for correct structure
#'
#' @param sts \emph{sts} object
#' @param verbose Logical specifying whether to produce detailed warning messages.
#'
#' @description The \code{sts} is checked for the presence of core
#' \code{meta} and \code{data} columns.
#'
#' Core \code{meta} columns include:
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
#' Core \code{data} columns include:
#'
#' \itemize{
#'   \item{\code{datetime} -- measurement time (UTC)}
#' }
#'
#' @return \code{TRUE} if \code{sts} has the correct structure,
#' \code{FALSE} otherwise.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' sts_isValid(example_sts)
#'
sts_isValid <- function(
  sts = NULL,
  verbose = FALSE
) {

  MazamaCoreUtils::stopIfNull(sts)

  msg <- ifelse(
    verbose,
    function(m) warning(m, call. = FALSE, immediate. = TRUE),
    function(m) NULL
  )

  # TODO:  Include class name check when this won't break AirSensor or RAWSmet
  # if (!"sts" %in% class(sts)) {
  #   msg("\n Not an `sts` class object. See help('is_sts').")
  #   return(invisible(FALSE))
  # }

  if ( !"meta" %in% names(sts) ) {
    msg("no 'meta' found in sts object")
    return(invisible(FALSE))
  }

  if ( !"data" %in% names(sts) ) {
    msg("no 'data' found in sts object")
    return(invisible(FALSE))
  }

  if ( !"data.frame" %in% class(sts$meta) ) {
    msg("sts$meta is not of class data.frame")
    return(invisible(FALSE))
  }

  if ( !"data.frame" %in% class(sts$data) ) {
    msg("sts$data is not of class data.frame")
    return(invisible(FALSE))
  }

  if ( !"datetime" %in% names(sts$data) ) {
    msg("sts$data$datetime axis is missing")
  }

  if ( !("POSIXct" %in% class(sts$data$datetime)) ) {
    msg("sts$data$datetime is not of class 'POSIXct'")
    return(invisible(FALSE))
  }

  if ( any(duplicated(sts$data$datetime)) ) {
    msg("duplicate timesteps found in sts$data$datetime")
    return(invisible(FALSE))
  }

  if ( !"deviceDeploymentID" %in% names(sts$meta) ) {
    msg("sts$meta$deviceDeploymentID is missing")
    return(invisible(FALSE))
  }

  if ( any(duplicated(sts$meta$deviceDeploymentID)) ||
       any(duplicated(names(sts$data))) ) {
    msg("sts contains duplicate deviceDeploymentIDs")
    return(invisible(FALSE))
  }

  if ( !all(requiredMetaNames %in% names(sts$meta)) ) {
    missingColumns <- setdiff(requiredMetaNames, names(sts$meta))
    msg(sprintf(
      "sts$meta must contain columns for '%s'",
      paste(missingColumns, collapse = ", ")
    ))
  }

  # Nothing failed so return TRUE
  return(invisible(TRUE))

}


#' @export
#'
#' @title Test for empty \emph{sts} object
#'
#' @param sts \emph{sts} object
#'
#' @return \code{TRUE} if no data exist in \code{sts}, \code{FALSE} otherwise.
#'
#' @description Convenience function for \code{nrow(sts$data) == 0}.
#' This makes for more readable code in functions that need to test for this.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' sts_isEmpty(example_sts)
#'
sts_isEmpty <- function(sts) {

  MazamaCoreUtils::stopIfNull(sts)
  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(sts) || !'data.frame' %in% class(sts$data) )
    stop("sts is not a valid 'sts' object")

  return( nrow(sts$data) == 0 || ncol(sts$data) == 1 )

}


#' @importFrom rlang .data
#' @export
#'
#' @title Retain only distinct data records in \code{sts$data}
#'
#' @param sts \emph{sts} object
#'
#' @return An \emph{sts} object where each record is associated with a unique
#' time.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description Three successive steps are used to guarantee that the
#' \code{datetime} axis contains no repeated values:
#'
#' \enumerate{
#' \item{remove any duplicate records}
#' \item{guarantee that rows are in \code{datetime} order}
#' \item{average together fields for any remaining records that share the same
#' \code{datetime}}
#' }
#'
sts_distinct <- function(sts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(sts) || !'data.frame' %in% class(sts$data) )
    stop("sts is not a valid 'sts' object")

  sts$data <-
    sts$data %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime) %>%
    .replaceRecordsWithDuplicateTimestamps()

  return( sts )

}


#' @title Extract dataframes from \emph{sts} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{sts} object. These functions are designed to be useful when
#' manipulating data in a pipeline using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \code{sts_extractData(sts)} is equivalent to \code{sts$data}.
#'
#' \code{sts_extractMeta(sts)} is equivalent to \code{sts$meta}.
#'
#' @param sts \emph{sts} object to extract dataframe from.
#'
#' @return A dataframe from the \emph{sts} object.
#'
#' @name sts_extractDataFrame
#' @aliases sts_extractData sts_extractMeta
#'
NULL


#' @export
#' @rdname sts_extractDataFrame
#'
sts_extractData <- function(sts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(sts) || !'data.frame' %in% class(sts$data) )
    stop("sts is not a valid 'sts' object")

  return(sts$data)

}


#' @export
#' @rdname sts_extractDataFrame
#'
sts_extractMeta <- function(sts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'meta' %in% names(sts) || !'data.frame' %in% class(sts$meta) )
    stop("sts is not valid 'sts' object")

  return(sts$meta)

}


# ===== INTERNAL FUNCTIONS =====================================================

.replaceRecordsWithDuplicateTimestamps <- function(df) {

  # NOTE:  Some time series datasets can have multiple records with the same
  # NOTE:  'datetime'. This might occur when times are forced to the nearest
  # NOTE:  second, minute or hour. This assumes that the incoming df is
  # NOTE:  arranged by datetime and averages together all records that share the
  # NOTE:  same 'datetime'.
  if ( any(duplicated(df$datetime)) ) {

    # Find duplicate records
    duplicateIndices <- which(duplicated(df$datetime))
    for ( index in duplicateIndices ) {

      # Record immediately prior will be the other record with this timestamp
      replacementRecord <-
        dplyr::slice(df, (index-1):index) %>%
        dplyr::summarise_all(mean, na.rm = TRUE)

      # Replace the original record with the mean record
      df[(index-1),] <- replacementRecord

    }

    # Kep all the non-duplicate timestamp records
    df <- df[!duplicated(df$datetime),]

  }

  return(df)

}


