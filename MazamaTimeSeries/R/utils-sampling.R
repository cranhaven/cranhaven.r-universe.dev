#' @keywords internal
#' @title General table row sampling
#'
#' @description This is a wrapper around \code{\link[base]{sample}} to make it
#' easy to select random rows from a table. Supports either integer
#' (\code{sampleSize}) or fractional (\code{sampleFraction}) row sampling.
#' For reproducible debugging, specify \code{seed}.
#'
#' If both \code{sampleSize} and \code{sampleFraction} are specified,
#' \code{sampleSize} takes precedence.
#'
#' Specifying \code{sampleSize} greater than the number of rows in the
#' dataframe or \code{sampleFraction > 1} will use all rows.
#'
#' @param data Dataframe to be sampled.
#' @param sampleSize Non-negative integer giving the number of rows to choose.
#' @param sampleFraction Fraction of rows to sample.
#' @param seed Integer passed to \code{\link[base]{set.seed}} for reproducible sampling.
#'
#' @return A data.frame
#'

.sample <- function(
  data,
  sampleSize = NULL,
  sampleFraction = NULL,
  seed = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(data)

  if ( !is.data.frame(data) )
    stop("'data' must be a dataframe")

  if ( is.null(sampleSize) && is.null(sampleFraction) )
    stop("Either 'sampleSize' or 'sampleFraction' must be specified")

  if ( is.numeric(sampleSize) &&  (sampleSize < 1) )
    stop("'sampleSize' must be a positive integer")

  if ( is.null(sampleSize) ) {
    if ( sampleFraction < 0 )
      stop("'sampleFraction' must be greater than 0")
  }

  if ( !is.null(seed) ) set.seed(seed)

  # ----- Sample ---------------------------------------------------------------

  if ( !is.null(sampleSize) ) {

    if ( sampleSize > nrow(data) ) {
      sampleSize <- nrow(data)
    }

  } else {

    sampleSize <- nrow(data) * sampleFraction

  }

  subset <-
    data[
      base::sample(
        x = nrow(data),
        size = sampleSize,
        replace = FALSE,
        prob = NULL
      ),]

  return(subset)

}


#' @keywords internal
#' @title Flag outliers in vectorized data
#'
#' @description This function uses Hampel filter outlier detection to flag
#' outliers in \code{parameter} column of the incoming dataframe. The
#' \code{width} and \code{thresholdMin} parameters as passed on to the
#' \code{\link[MazamaRollUtils]{findOutliers}} function.
#'
#' An additional boolean column named \code{<parameter>_outlierFlag} is added
#' to the dataframe. This column will have \code{TRUE} whenever an outlier is
#' detected for the chosen \code{parameter}.
#'
#' See \code{\link[MazamaRollUtils]{findOutliers}} for further details.
#'
#' @param df Data frame.
#' @param parameter Data frame parameter to use for outlier detection.
#' @param width Width the rolling window.
#' @param thresholdMin Minimum threshold value used to detect outliers.
#'
#' @return A dataframe with an additional column identifying outliers.
#'

.flagOutliers <- function(
  df = NULL,
  parameter = NULL,
  width = 23,
  thresholdMin = 8
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(df)
  MazamaCoreUtils::stopIfNull(parameter)
  MazamaCoreUtils::stopIfNull(width)
  MazamaCoreUtils::stopIfNull(thresholdMin)

  # ----- Flag outliers --------------------------------------------------------

  columnData<- df[[parameter]]
  outlierFlagName <- paste0(parameter, "_outlierFlag")

  # Identify outliers
  result <- try({

    outlierIndices <-
      MazamaRollUtils::findOutliers(
        x = columnData,
        width = width,
        thresholdMin = thresholdMin,
        selectivity = NA,
        fixedThreshold = TRUE
      )

    }, silent = TRUE)

  if ( 'try-error' %in% class(result) ) {
    df[[outlierFlagName]] <- FALSE # if error with Hampel filter, ignore outlier flagging
  } else {
    # Make a new logical column
    df[[outlierFlagName]] <- FALSE
    df[[outlierFlagName]][outlierIndices] <- TRUE
  }

  return(df)

}

