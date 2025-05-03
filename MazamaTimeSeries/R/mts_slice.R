#' @export
#' @importFrom rlang .data
#'
#' @title Subset time series based on their position
#'
#' @param mts \emph{mts} object.
#' @param n Number of rows of \code{mts$meta} to select.
#'
#' @description An \emph{mts} object is reduced so as to contain only the first
#' or last \code{n} timeseries. These functions work similarly to
#' \code{\link[dplyr:slice_head]{dplyr::slice_head}} and
#' \code{\link[dplyr:slice_tail]{dplyr::slice_tail}}
#' but apply to both dataframes in the \emph{mts} object.
#'
#' This is primarily useful when the \emph{mts} object has been ordered by a
#' previous call to \code{\link{mts_arrange}} or by some other means.
#'
#' \code{slice_head()} selects the first and \code{slice_tail()} the last timeseries
#' in the object.
#'
#' @return A subset of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' # Find lowest elevation sites
#' Camp_Fire %>%
#'   mts_filterMeta(!is.na(elevation)) %>%
#'   mts_arrange(elevation) %>%
#'   mts_slice_head(n = 5) %>%
#'   mts_extractMeta() %>%
#'   dplyr::select(elevation, locationName)
#'
#' # Find highest elevation sites
#' Camp_Fire %>%
#'   mts_filterMeta(!is.na(elevation)) %>%
#'   mts_arrange(elevation) %>%
#'   mts_slice_tail(n = 5) %>%
#'   mts_extractMeta() %>%
#'   dplyr::select(elevation, locationName)
#'

#' @export
#' @rdname mts_slice
mts_slice_head <- function(
  mts,
  n = 5
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

  # ----- Filter meta ----------------------------------------------------------

  mts$meta <-
    dplyr::slice_head(mts$meta, n = n)

  # ----- Filter data ----------------------------------------------------------

  # NOTE:  The columns in 'data' must always match the rows in 'meta'

  colNames <- c('datetime', mts$meta$deviceDeploymentID)
  mts$data <-
    dplyr::select(mts$data, dplyr::all_of(colNames))

  # ----- Return ---------------------------------------------------------------

  return(mts)

}


#' @export
#' @rdname mts_slice
mts_slice_tail <- function(
    mts,
    n = 5
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

  if ( mts_isEmpty(mts) )
    return(mts)

  # ----- Filter meta ----------------------------------------------------------

  mts$meta <-
    dplyr::slice_tail(mts$meta, n = n)

  # ----- Filter data ----------------------------------------------------------

  # NOTE:  The columns in 'data' must always match the rows in 'meta'

  colNames <- c('datetime', mts$meta$deviceDeploymentID)
  mts$data <-
    dplyr::select(mts$data, dplyr::all_of(colNames))

  # ----- Return ---------------------------------------------------------------

  return(mts)

}
