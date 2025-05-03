#' @export
#' @importFrom rlang .data
#'
#' @title General purpose metadata filtering for \emph{mts} time series objects
#'
#' @param mts \emph{mts} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{mts$meta}.
#'
#' @description A generalized metadata filter for \emph{mts} objects to
#' choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to FALSE or
#' \code{NA} are dropped.
#'
#' If an empty \emph{mts} object is passed in, it is immediately returned,
#' allowing for multiple filtering steps to be piped together and only checking
#' for an empty \emph{mts} object at the end of the pipeline.
#'
#' @note Filtering is done on variables in \code{mts$meta}.
#'
#' @return A subset of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{mts_filterData}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Filter for all labels with "SCSH"
#' scap <-
#'   example_mts %>%
#'   mts_filterMeta(communityRegion == "El Monte")
#'
#' dplyr::select(scap$meta, ID, label, longitude, latitude, communityRegion)
#'
#' head(scap$data)
#'

mts_filterMeta <- function(
  mts,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'mts'

  result <- try({
    if ( !mts_isValid(mts) )
      stop("First argument is not a valid 'mts' object.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'mts' object?)"))
    }
  }

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  # Remove any duplicate data records
  mts <- mts_distinct(mts)

  # ----- Filter meta ----------------------------------------------------------

  mts$meta <-
    dplyr::filter(mts$meta, ...)

  # ----- Filter data ----------------------------------------------------------

  # NOTE:  The columns in 'data' must always match the rows in 'meta'

  colNames <- c('datetime', mts$meta$deviceDeploymentID)
  mts$data <-
    dplyr::select(mts$data, dplyr::all_of(colNames))

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
