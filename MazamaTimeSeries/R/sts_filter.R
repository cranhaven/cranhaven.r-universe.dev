#' @export
#' @importFrom rlang .data
#'
#' @title General purpose data filtering for \emph{sts} time series objects
#'
#' @param sts \emph{sts} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{sts$data}.
#'
#' @description A generalized data filter for \emph{sts} objects to
#' choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' If an empty \emph{sts} object is passed in, it is immediately returned,
#' allowing for multiple filtering steps to be piped together and only checking
#' for an empty \emph{sts} object at the end of the pipeline.
#'
#' @note Filtering is done on values in \code{sts$data}.
#'
#' @return A subset of the incoming \code{sts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{sts_filterDate}
#' @seealso \link{sts_filterDatetime}
#' @examples
#'
#' library(MazamaTimeSeries)
#'
#' unhealthy <- sts_filter(example_sts, pm25_A > 55.5, pm25_B > 55.5)
#' head(unhealthy$data)
#'

sts_filter <- function(
  sts,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'sts'

  result <- try({
    if ( !sts_isValid(sts) )
      stop("First argument is not a valid 'sts' object.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'sts' object?)"))
    }
  }

  # Return the sts if it is empty so pipelines don't break
  if ( sts_isEmpty(sts) )
    return(sts)

  # Remove any duplicate data records
  sts <- sts_distinct(sts)

  # ----- Filter data ----------------------------------------------------------

  sts$data <-
    dplyr::filter(sts$data, ...)

  # ----- Return ---------------------------------------------------------------

  return(sts)

}
