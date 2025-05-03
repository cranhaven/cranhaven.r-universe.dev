#' @export
#' @importFrom rlang .data
#'
#' @title Extract a column of metadata or data
#'
#' @param mts \emph{mts} object.
#' @param var A variable name found in the \code{meta} or \code{data}
#' dataframe of the incoming \emph{mts} time series object.
#'
#' @description
#' This function acts similarly to \code{dplyr::pull()} working on
#' \code{mts$meta} or \code{mts$data}. Data are returned as a simple array.
#' Data are pulled from whichever dataframe contains \code{var}.
#'
#' @return An array of values.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Metadata
#' example_mts %>%
#'   mts_pull("communityRegion") %>%
#'   table() %>%
#'   sort(decreasing = TRUE)
#'
#' # Data for a specific ID
#' example_mts %>%
#'   mts_pull("da4cadd2d6ea5302_4686")
#'

mts_pull <- function(
  mts = NULL,
  var = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)
  MazamaCoreUtils::stopIfNull(var)

  if ( !is.character(var) ) {
    stop("var must be a character string name of a variable in mts$meta or mts$data")
  }

  if ( !var %in% names(mts$meta) && !var %in% names(mts$data) ) {
    stop(sprintf(
      "Variable '%s' not found in mts$meta or mts$data", var
    ))
  }

  # ----- Pull values ----------------------------------------------------------

  if ( var %in% names(mts$meta) ) {
    values <- dplyr::pull(mts$meta, var)
  } else {
    values <- dplyr::pull(mts$data, var)
  }

  # ----- Return ---------------------------------------------------------------

  return(values)

}


