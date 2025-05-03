#' @export
#' @importFrom rlang .data
#'
#' @title Combine multiple \emph{mts} time series objects
#'
#' @param ... Any number of valid \emph{mts} objects.
#' @param replaceMeta Logical specifying whether to allow replacement of
#' metadata associated with \code{deviceDeploymentIDs}.
#' @param overlapStrategy Strategy to use when data found in time series
#' overlaps.
#'
#' @return An \emph{mts} time series object containing all time series found
#' in the incoming \code{mts} objects.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description Create a combined \emph{mts} from any number of \emph{mts}
#' objects or from a list of \emph{mts} objects. The resulting \emph{mts}
#' object with contain all \code{deviceDeploymentIDs} found in any incoming
#' \emph{mts} and will have a regular time axis covering the the entire range
#' of incoming data.
#'
#' If incoming time ranges are non-contiguous, the resulting \emph{mts} will
#' have gaps filled with \code{NA} values.
#'
#' An error is generated if the incoming \emph{mts} objects have
#' non-identical metadata for the same \code{deviceDeploymentID} unless
#' \code{replaceMeta = TRUE}.
#'
#' @note Data for any \code{deviceDeploymentIDs} shared among \emph{mts}
#' objects are combined with a "later is better" sensibility where any
#' data overlaps exist. To handle this, incoming \emph{mts} objects are first
#' split into "shared" and "unshared" parts.
#'
#' Any "shared" parts are ordered based on the
#' time stamp of their last record. Then \code{dplyr::distinct()} is used to
#' remove records with duplicate \code{datetime} fields.
#'
#' With \code{overlapStrategy = "replace all"}, any data records found
#' in "later" \emph{mts} objects are preferentially retained before the "shared"
#' data are finally reordered by ascending \code{datetime}.
#'
#' With \code{overlapStrategy = "replace missing"}, only missing values in "earlier"
#' \emph{mts} objects are replaced with data records from "later" time series.
#'
#' The final step is combining the "shared" and "unshared" parts and placing
#' them on a uniform time axis.
#'
#' @examples
#' library(MazamaTimeSeries)
#'
#' ids1 <- example_mts$meta$deviceDeploymentID[1:5]
#' ids2 <- example_mts$meta$deviceDeploymentID[4:6]
#' ids3 <- example_mts$meta$deviceDeploymentID[8:10]
#'
#' mts1 <-
#'   example_mts %>%
#'   mts_filterMeta(deviceDeploymentID %in% ids1) %>%
#'   mts_filterDate(20190701, 20190703)
#'
#' mts2 <-
#'   example_mts %>%
#'   mts_filterMeta(deviceDeploymentID %in% ids2) %>%
#'   mts_filterDate(20190704, 20190706)
#'
#' mts3 <-
#'   example_mts %>%
#'   mts_filterMeta(deviceDeploymentID %in% ids3) %>%
#'   mts_filterDate(20190705, 20190708)
#'
#' mts <- mts_combine(mts1, mts2, mts3)
#'
#' # Should have 1:6 + 8:10 = 9 meta records and the full date range
#' nrow(mts$meta)
#' range(mts$data$datetime)
#'

mts_combine <- function(
  ...,
  replaceMeta = FALSE,
  overlapStrategy = c("replace all", "replace na")
) {

  # Accept any number of mts objects
  mtsList <- list(...)

  # ----- Validate parameters --------------------------------------------------

  if ( length(mtsList) == 0 )
    stop("no 'mts' arguments provided")

  # NOTE:  If the first element is just a plain "list" of length 1, assume we are
  # NOTE:  being handed a list of mts objects rather than separate mts objects.
  if ( length(class(mtsList[[1]])) == 1 && inherits(mtsList[[1]], "list") ) {
    mtsList <- mtsList[[1]]
  }

  # NOTE:  This loop will stop() if there are any problems.
  for ( mts in mtsList ) {
    result <- mts_check(mts)
  }

  overlapStrategy <- match.arg(overlapStrategy)

  # ----- Later is better order ------------------------------------------------

  dataList <- lapply(mtsList, `[[`, "data")

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

  metaList <- lapply(mtsList, `[[`, "meta")

  metaList <- metaList[dataOrder]

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

  # ----- Regular time axis ----------------------------------------------------

  allTimes <-
    lapply(dataList, `[[`, "datetime") %>%
    unlist() %>%
    as.POSIXct(origin = lubridate::origin, tz = "UTC")

  # Create the full time axis
  datetime <- seq(min(allTimes), max(allTimes), by = "hours")
  hourlyDF <- data.frame(datetime = datetime)

  # ----- Combine 'data' -------------------------------------------------------

  # NOTE:  Here we need to proceed carefully to accomplish two goals:
  # NOTE:   1) combine new time series
  # NOTE:   2) combine existing time series with "later is better" logic.
  # NOTE:
  # NOTE:  To do this, we need to march through the list of mts objects and
  # NOTE:  apply the following logic to each pair:
  # NOTE:   1) separate A-only, AB-shared and B-only tibbles
  # NOTE:   2) combine AB-shared with later-is-better
  # NOTE:   3) left join ((A, AB), B)

  # ===== BEGIN LOOP ===========================================================

  for ( i in seq_along(dataList) ) {

    tbl <- dataList[[i]]

    if ( i == 1 ) {
      ###data <- dplyr::left_join(hourlyDF, tbl, by = "datetime")
      data <- tbl
      next
    }

    A_names <- c("datetime", setdiff(names(data), names(tbl)))
    AB_names <- intersect(names(data), names(tbl))
    B_names <- c("datetime", setdiff(names(tbl), names(data)))

    # * A-only columns  -----

    A_data <-
      data %>%
      dplyr::select(dplyr::all_of(A_names)) %>%
      dplyr::arrange(.data$datetime)

    # * AB columns -----

    if ( overlapStrategy == "replace all" ) {

      # AB-shared columns with later-is-better logic
      AB_tbl <- tbl %>% dplyr::select(dplyr::all_of(AB_names))

      AB_data <-
        data %>%
        dplyr::select(dplyr::all_of(AB_names)) %>%
        dplyr::bind_rows(AB_tbl) %>%
        dplyr::distinct(.data$datetime, .keep_all = TRUE) %>%
        dplyr::arrange(.data$datetime)

      # * clean up -----
      rm(list = c("AB_tbl"))

    } else if ( overlapStrategy == "replace na" ) {

      # NOTE:  We put AB_tbl and AB_data on the same time axis so we can
      # NOTE:  replace missing values per-timeseries. With both tibbles having
      # NOTE:  an identical structure, we can apply a simple matrix mask.

      AB_tblBrick <-
        hourlyDF %>%
        dplyr::left_join(tbl, by = "datetime") %>%   # uniform time axis
        dplyr::select(dplyr::all_of(AB_names)) %>%   # shared columns
        dplyr::arrange(.data$datetime) %>%           # ordered by time
        dplyr::select(-1) %>%                        # remove 'datetime'
        as.matrix()                                  # convert to matrix

      AB_dataBrick <-
        hourlyDF %>%
        dplyr::left_join(data, by = "datetime") %>%
        dplyr::select(dplyr::all_of(AB_names)) %>%
        dplyr::arrange(.data$datetime) %>%
        dplyr::select(-1) %>%
        as.matrix()

      # NOTE:  This only works for matrices, not tibbles
      mask <- is.na(AB_dataBrick)
      AB_dataBrick[mask] <- AB_tblBrick[mask]

      AB_dataOnly <- dplyr::as_tibble(AB_dataBrick)

      AB_data <- dplyr::bind_cols(hourlyDF, AB_dataOnly)

      # * clean up -----
      rm(list = c("AB_tblBrick", "AB_dataBrick", "AB_dataOnly"))
    }

    # * B-only columns -----

    B_tbl <-
      tbl %>%
      dplyr::select(dplyr::all_of(B_names)) %>%
      dplyr::arrange(.data$datetime)

    # * combine A, AB and B -----

    # Start with A_data
    data <- A_data

    # Add AB_data
    if ( ncol(AB_data) > 1 ) {
      data <-
        data %>%
        dplyr::full_join(AB_data, by = "datetime") %>%
        dplyr::arrange(.data$datetime)
    }

    # Add B_tbl
    if ( ncol(B_tbl) > 1 ) {
      data <-
        data %>%
        dplyr::full_join(B_tbl, by = "datetime") %>%
        dplyr::arrange(.data$datetime)
    }


    # * clean up -----
    rm(list = c("tbl", "A_data", "AB_data", "B_tbl"))

  }

  # ===== END LOOP =============================================================

  # ----- Regular time axis ----------------------------------------------------

  data <-
    hourlyDF %>%
    dplyr::left_join(data, by = "datetime")

  # ----- Create the 'mts' object ----------------------------------------------

  # NOTE:  The columns in 'data' must always match the rows in 'meta'

  colNames <- c('datetime', meta$deviceDeploymentID)
  data <-
    dplyr::select(data, dplyr::all_of(colNames))

  mts <- list(meta = meta, data = data)
  class(mts) <- union("mts", class(mts))

  # ----- Return ---------------------------------------------------------------

  return(invisible(mts))

}

# ===== INTERNAL FUNCTIONS =====================================================

