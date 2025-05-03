#' @export
#' @importFrom rlang .data
#'
#' @title Order \emph{mts} time series by metadata values
#'
#' @param mts \emph{mts} object.
#' @param ... variables in \code{mts$meta}.
#'
#' @description The variable(s) in \code{...} are used to specify columns of
#' \code{mts$meta} to use for ordering. Under the hood, this
#' function uses \code{\link[dplyr]{arrange}} on \code{mts$meta} and then
#' reorders \code{mts$data} to match.
#'
#' @return A reorderd version of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' example_mts$meta$latitude[1:10]
#'
#' # Filter for all labels with "SCSH"
#' byElevation <-
#'   example_mts %>%
#'   mts_arrange(latitude)
#'
#' byElevation$meta$latitude[1:10]
#'

mts_arrange <- function(
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

  # ----- Order meta -----------------------------------------------------------

  mts$meta <-
    dplyr::arrange(mts$meta, ...)

  # ----- Order data -----------------------------------------------------------

  # NOTE:  The columns in 'data' must always match the rows in 'meta'

  colNames <- c('datetime', mts$meta$deviceDeploymentID)
  mts$data <-
    dplyr::select(mts$data, dplyr::all_of(colNames))

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
