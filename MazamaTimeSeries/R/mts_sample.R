#' @export
#' @importFrom rlang .data
#'
#' @title Sample time series for an \emph{mts} time series object
#'
#' @param mts \emph{mts} object.
#' @param sampleSize Non-negative integer giving the number of rows to choose.
#' @param seed Integer passed to \code{\link[base]{set.seed}} for reproducible sampling.
#' @param keepOutliers Logical specifying a graphics focused sampling algorithm
#' that retains outliers (see Details).
#' @param width Integer width of the rolling window used for outlier detection.
#' @param thresholdMin Numeric threshold for outlier detection.
#'
#' @return A subset of the given \emph{mts} object.
#' @return An \emph{mts} time series object with fewer timesteps.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description Reduce the number of records (timesteps) in the \code{data}
#' dataframe of the incoming \code{mts} through random sampling.
#'
#' @details When \code{keepOutliers = FALSE}, random sampling is used to provide
#' a statistically relevant subsample of the data.
#'
#' @section Outlier Detection:
#'
#' When \code{keepOutliers = TRUE}, a customized sampling algorithm is used that
#' attempts to create subsets for use in plotting that create plots that are
#' visually identical to plots using all data. This is accomplished by
#' preserving outliers and only sampling data in regions where overplotting
#' is expected.
#'
#' The process is as follows:
#' \enumerate{
#' \item{find outliers using \code{MazamaRollUtils::findOutliers()}}
#' \item{create a subset consisting of only outliers}
#' \item{sample the remaining data}
#' \item{merge the outliers and sampled data}
#' }
#'
#' This algorithm works best when the \emph{mts} object has only one or two
#' timeseries.
#'
#' The \code{width} and \code{thresholdMin} parameters determine the number of
#' outliers detected. For hourly data, a \code{width} of 5 and a \code{thresholdMin}
#' of 3 or 4 seem to find many visually obvious outliers.
#'
#' Users attempting to optimize plotting speed for lengthy time series are
#' encouraged to experiment with these two parameters along with
#' \code{sampleSize} and review the results visually.
#'
#' See \code{MazamaRollUtils::findOutliers()}.
#'

mts_sample <- function(
  mts = NULL,
  sampleSize = 5000,
  seed = NULL,
  keepOutliers = FALSE,
  width = 5,
  thresholdMin = 3
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(mts)

  # Return the mts if it is empty so pipelines don't break
  if ( mts_isEmpty(mts) )
    return(mts)

  if ( sampleSize > nrow(mts$data) )
    return(mts)

  # ----- Simple filtering -----------------------------------------------------

  if ( keepOutliers == FALSE ) {
    mts$data <- .sample(mts$data, sampleSize, sampleFraction = NULL, seed)
    return(mts)
  }

  # ----- Keep outliers --------------------------------------------------------

  # * Find indices for all outliers -----

  outlierIndices <-
    # First, apply findOutliers() to each numeric column
    lapply(
      dplyr::select(mts$data, -"datetime"),
      MazamaRollUtils::findOutliers,
      width = width,
      thresholdMin = thresholdMin,
      selectivity = NA,
      fixedThreshold = TRUE
    ) %>%
    # Then, create the union of all indices found in each column
    unlist() %>%
    as.numeric() %>%
    sort() %>% unique()

  # * Separate outliers and boring stuff -----

  outlierData <- mts$data[outlierIndices,]
  boringData <- mts$data[-outlierIndices,]

  if ( length(outlierIndices) > sampleSize ) {
    # Issue a warning (which could be suppressed) but still return
    warning(sprintf("Number of outliers: %d exceeds sampleSize: %d.",
                    length(outlierIndices), sampleSize))
    mts$data <- outlierData
    return(mts)
  }

  # * Sample boring stuff and recombine -----

  remainingSampleSize <- sampleSize - length(outlierIndices)

  data <-
    boringData %>%
    .sample(
      sampleSize = remainingSampleSize,
      sampleFraction = NULL,
      seed = seed
    ) %>%
    dplyr::bind_rows(outlierData)

  mts$data <- data
  return(mts)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(MazamaTimeSeries)

  wa <-
    PWFSLSmoke::airnow_loadAnnual(2021) %>%
    PWFSLSmoke::monitor_subset(stateCodes = "WA")

  meta <- wa$meta
  data <- wa$data

  meta1 <- wa$meta[1:5,]
  data1 <- wa$data[,1:6]

  mts <- example_mts
  mts$meta <- meta1
  mts$data <- data1


  sampleSize <- 5000
  seed <- 123
  keepOutliers <- TRUE


}
