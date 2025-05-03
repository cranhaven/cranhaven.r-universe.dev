#' @export
#' @importFrom rlang .data
#'
#' @title Data-based subsetting of time series within an \emph{mts} object.
#'
#' @param mts \emph{mts} object.
#' @param FUN A function applied to time series data that returns TRUE or FALSE.
#'
#' @description
#' Subsetting of \code{mts} acts similarly to \code{tidyselect::where()} working on
#' \code{mts$data}. The returned \emph{mts} object will contain only
#' those time series where \code{FUN} applied to the time series data returns \code{TRUE}.
#'
#' @return A subset of the incoming \emph{mts} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{mts_select}
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Show all Camp_Fire locations
#' Camp_Fire$meta$locationName
#'
#' # Set a threshold
#' threshold <- 500
#'
#' # Find time series with data at or above this threshold
#' worst_sites <-
#'   Camp_Fire %>%
#'   mts_selectWhere(
#'     function(x) { any(x >= threshold, na.rm = TRUE) }
#'   )
#'
#' # Show the worst locations
#' worst_sites$meta$locationName
#'

mts_selectWhere <- function(
  mts,
  FUN
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

  # Remove any duplicate data records
  mts <- mts_distinct(mts)

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  if ( !is.function(FUN) )
    stop("'FUN' is not a function.")

  if ( !is.logical(FUN(c(1:5,NA,6:10))) || is.na(FUN(c(1:5,NA,6:10))) )
    stop("'FUN' does not return TRUE/FALSE when NAs are found. Do you need to include 'na.rm = TRUE'?")

  # ----- Apply function -------------------------------------------------------

  # See https://dplyr.tidyverse.org/articles/colwise.html

  # Get dataBrick
  tbl <- mts$data[,-1]

  # Apply function
  mask <-
    tbl %>%
    dplyr::summarize(dplyr::across(.cols = dplyr::everything(), FUN)) %>%
    as.logical()

  # Get deviceDeploymentIDs where FUN returns TRUE
  ids <- names(tbl)[mask]

  # Select those ids
  mts <- mts %>% mts_select(ids)

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
