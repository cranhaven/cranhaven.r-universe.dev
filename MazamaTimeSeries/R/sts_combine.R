#' @export
#' @importFrom rlang .data
#'
#' @title Combine multiple \emph{sts} time series objects
#'
#' @param ... Any number of valid SingleTimeSeries \emph{sts} objects associated
#' with a single \code{deviceDeploymentID}.
#' @param replaceMeta Logical specifying whether to allow replacement of
#' metadata.
#'
#' @return A SingleTimeSeries \emph{sts} time series object containing
#' records from all incoming \code{sts} time series objects.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description Create a merged timeseries using of any number of \emph{sts}
#' objects for a single sensor. If \emph{sts} objects are non-contiguous, the
#' resulting \emph{sts} will have gaps.
#'
#' An error is generated if the incoming \emph{sts} objects have
#' non-identical \code{deviceDeploymentIDs}.
#'
#' @note Data are combined with a "later is better" sensibility where any
#' data overlaps exist. To handle this, incoming \emph{sts} objects are first
#' split into "shared" and "unshared" parts.
#'
#' Any "shared" parts are ordered based on the
#' time stamp of their last record. Then \code{dplyr::distinct()} is used to
#' remove records with duplicate \code{datetime} fields. Any data records found
#' in "later" \emph{sts} objects are preferentially retained before the "shared"
#' data are finally reordered by ascending \code{datetime}.
#'
#' The final step is combining the "shared" and "unshared" parts.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' aug01_08 <-
#'   example_sts %>%
#'   sts_filterDate(20180801, 20180808)
#'
#' aug15_22 <-
#'   example_sts %>%
#'   sts_filterDate(20180815, 20180822)
#'
#' aug01_22 <- sts_combine(aug01_08, aug15_22)
#'
#' plot(aug01_22$data$datetime)

sts_combine <- function(
  ...,
  replaceMeta = FALSE
) {

  # Accept any number of sts objects
  stsList <- list(...)

  # ----- Validate parameters --------------------------------------------------

  if ( length(stsList) == 0 )
    stop("no 'sts' arguments provided")

  # NOTE:  If the first element is just a plain "list" of length 1, assume we are
  # NOTE:  being handed a list of sts objects rather than separate sts objects.
  if ( length(class(stsList[[1]])) == 1 && inherits(stsList[[1]], "list") ) {
    stsList <- stsList[[1]]
  }

  # Return immediately if we only get a single sts
  if ( length(stsList) == 1 )
    return(invisible(stsList[[1]]))

  # NOTE:  This loop will stop() if there are any problems.
  for ( sts in stsList ) {
    if ( !sts_isValid(sts) )
      stop("invalid 'sts' object")
  }

  # TODO: Sort out why the above doesn't work in test-sts_combine.R

  # ----- Later is better order ------------------------------------------------

  dataList <- lapply(stsList, `[[`, "data")

  # NOTE:  To simplify use of our "later is better" approach, we organize our
  # NOTE:  dataframes in most-recent-first order so that application of
  # NOTE:  dplyr::distinct(), which preserves the first instance of a duplicate,
  # NOTE:  will retain the most recent record.

  lastDatetime <-
    sapply(dataList, function(x) { x$datetime[nrow(x)] } ) %>%
    as.numeric()
  dataOrder <- order(lastDatetime, decreasing = TRUE)

  dataList <- dataList[dataOrder]

  # ----- Combine 'meta' -------------------------------------------------------

  metaList <- lapply(stsList, `[[`, "meta")

  metaList <- metaList[dataOrder]

  ids <-
    dplyr::bind_rows(metaList) %>%
    dplyr::pull(.data$deviceDeploymentID) %>%
    unique()

  if ( length(ids) > 1 ){
    stop(sprintf(
      "'sts' objects have non-identical ids: %s",
      paste0(ids, collapse = ", ")
    ))
  }

  if ( replaceMeta ) {

    # Combine 'meta' tibbles using later-is-better replacement
    meta <-
      dplyr::bind_rows(metaList) %>%
      dplyr::distinct(.data$deviceDeploymentID, .keep_all = TRUE)

  } else {

    # Combine 'meta' tibbles without replacement
    meta <-
      dplyr::bind_rows(metaList) %>%
      dplyr::distinct()

    # NOTE:  We should stop if any 'deviceDeploymentIDs' have non-identical
    # NOTE:  metadata.

    duplicatedMask <- duplicated(meta$deviceDeploymentID)
    if ( sum(duplicatedMask) > 0 ) {
      stop(sprintf(
        "The following ids have non-identical metadata: %s",
        paste0(meta$deviceDeploymentID[duplicatedMask], collapse = ", ")
      ))
    }

  }

  # ----- Combine 'data' -------------------------------------------------------

  data <-
    dplyr::bind_rows(dataList) %>%
    dplyr::distinct(.data$datetime, .keep_all = TRUE) %>%
    dplyr::arrange(.data$datetime)

  # ----- Create the 'sts' object ----------------------------------------------

  sts <- list(meta = meta, data = data)
  class(sts) <- c("sts", class(sts))

  # ----- Return ---------------------------------------------------------------

  return(invisible(sts))

}
