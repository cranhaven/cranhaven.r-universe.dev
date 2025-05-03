#' @export
#'
#' @title Check \emph{mts} object for validity
#'
#' @param mts \emph{mts} object.
#'
#' @description Checks on the validity of an \emph{mts} object. If any test
#' fails, this function will stop with a warning message.
#'
#' @return Returns \code{TRUE} invisibly if the \emph{mts} object is valid.
#'
#' @seealso \link{mts_isValid}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' sts_check(example_mts)
#'
#' # This would throw an error
#' if ( FALSE ) {
#'
#'   broken_mts <- example_mts
#'   names(broken_mts) <- c('meta', 'bop')
#'   sts_check(broken_mts)
#'
#' }
#'

mts_check <- function(mts) {
  tryCatch(
    mts_isValid(mts, verbose = TRUE),
    warning = function(w) stop(w),
    finally = invisible(TRUE)
  )
}


#' @export
#'
#' @name mts_isValid
#' @title Test \emph{mts} object for correct structure
#'
#' @param mts \emph{mts} object
#' @param verbose Logical specifying whether to produce detailed warning messages.
#'
#' @description The \code{mts} is checked for the presence of core
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
#' @return Invisibly returns \code{TRUE} if \code{mts} has the correct structure,
#' \code{FALSE} otherwise.
#'
#' @seealso \link{mts_check}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' print(mts_isValid(example_mts))
#'
mts_isValid <- function(
  mts = NULL,
  verbose = FALSE
) {

  MazamaCoreUtils::stopIfNull(mts)

  msg <- ifelse(
    verbose,
    function(m) warning(m, call. = FALSE, immediate. = TRUE),
    function(m) NULL
  )

  # TODO:  Include class name check when this won't break AirSensor or RAWSmet
  # if (!"mts" %in% class(mts)) {
  #   msg("\n Not an `mts` class object. See help('is_mts').")
  #   return(invisible(FALSE))
  # }

  if ( !"meta" %in% names(mts) ) {
    msg("no 'meta' found in mts object")
    return(invisible(FALSE))
  }

  if ( !"data" %in% names(mts) ) {
    msg("no 'data' found in mts object")
    return(invisible(FALSE))
  }

  if ( !"data.frame" %in% class(mts$meta) ) {
    msg("mts$meta is not of class data.frame")
    return(invisible(FALSE))
  }

  if ( !"data.frame" %in% class(mts$data) ) {
    msg("mts$data is not of class data.frame")
    return(invisible(FALSE))
  }

  if ( !"datetime" %in% names(mts$data) ) {
    msg("mts$data$datetime axis is missing")
  }

  if ( !("POSIXct" %in% class(mts$data$datetime)) ) {
    msg("mts$data$datetime is not of class 'POSIXct'")
    return(invisible(FALSE))
  }

  if ( any(duplicated(mts$data$datetime)) ) {
    msg("duplicate timesteps found in mts$data$datetime")
    return(invisible(FALSE))
  }

  if ( !"deviceDeploymentID" %in% names(mts$meta) ) {
    msg("mts$meta$deviceDeploymentID is missing")
    return(invisible(FALSE))
  }

  if ( any(duplicated(mts$meta$deviceDeploymentID)) ||
       any(duplicated(names(mts$data))) ) {
    msg("mts contains duplicate deviceDeploymentIDs")
    return(invisible(FALSE))
  }

  if ( !all(requiredMetaNames %in% names(mts$meta)) ) {
    missingColumns <- setdiff(requiredMetaNames, names(mts$meta))
    msg(sprintf(
      "mts$meta must contain columns for '%s'",
      paste(missingColumns, collapse = ", ")
    ))
    return(invisible(FALSE))
  }

  # Guarantee that 'data' columns exactly match meta$deviceDeploymentID
  # NOTE:  This is a core guarantee of the 'mts' data model.
  if ( !identical(names(mts$data), c('datetime', mts$meta$deviceDeploymentID)) ) {
    msg(sprintf(
      "%s\n%s",
      "mismatch between mts$meta$deviceDeploymentID and names(mts$data)",
      "Columns in 'data' must be in the same order as rows in 'meta'."
    ))
    return(invisible(FALSE))
  }

  # Nothing failed so return TRUE
  return(invisible(TRUE))

}


#' @export
#'
#' @title Test for an empty \emph{mts} object
#'
#' @param mts \emph{mts} object
#'
#' @return \code{TRUE} if no data exist in \code{mts}, \code{FALSE} otherwise.
#'
#' @description Convenience function for \code{nrow(mts$data) == 0}.
#' This makes for more readable code in functions that need to test for this.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' mts_isEmpty(example_mts)
#'
mts_isEmpty <- function(mts) {

  MazamaCoreUtils::stopIfNull(mts)

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(mts) || !'data.frame' %in% class(mts$data) )
    stop("mts is not a valid 'mts' object")

  return( nrow(mts$data) == 0 || ncol(mts$data) == 1 )

}


#' @importFrom rlang .data
#' @export
#'
#' @title Retain only distinct data records in \code{mts$data}
#'
#' @param mts \emph{mts} object
#'
#' @return An \emph{mts} object where each record is associated with a unique
#' time.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description This function is primarily for internal use.
#'
#' Two successive steps are used to guarantee that the
#' \code{datetime} axis contains no repeated values:
#'
#' \enumerate{
#' \item{remove any duplicate records}
#' \item{guarantee that rows are in \code{datetime} order}
#' }
#'
mts_distinct <- function(mts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(mts) || !'data.frame' %in% class(mts$data) )
    stop("mts is not a valid 'mts' object")

  mts$data <-
    mts$data %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)

  if ( any(duplicated(mts$data$datetime)) )
    stop("duplicate timesteps with differing values found in 'mts' object")

  return(mts)

}


#' @title Extract dataframes from \emph{mts} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise an \emph{mts} object. These functions are designed to be useful when
#' manipulating data in a pipeline chain using \code{\%>\%}.
#'
#' \code{mts_extractData(mts)} is equivalent to \code{mts$data}.
#'
#' \code{mts_extractMeta(mts)} is equivalent to \code{mts$meta}.
#'
#' @param mts \emph{mts} object to extract dataframe from.
#'
#' @return A dataframe from the \emph{mts} object.
#'
#' @name mts_extractDataFrame
#' @aliases mts_extractData mts_extractMeta
#'
NULL


#' @export
#' @rdname mts_extractDataFrame
#'
mts_extractData <- function(mts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(mts) || !'data.frame' %in% class(mts$data) )
    stop("mts is not a valid 'mts' object")

  return(mts$data)

}


#' @export
#' @rdname mts_extractDataFrame
#'
mts_extractMeta <- function(mts) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'meta' %in% names(mts) || !'data.frame' %in% class(mts$meta) )
    stop("mts is not a valid 'mts' object")

  return(mts$meta)

}

