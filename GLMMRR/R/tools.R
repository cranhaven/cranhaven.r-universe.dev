#' Get unique groups of covariates
#'
#' @param x
#' a matrix-like object containing the covariates.
#' @return
#' a factor of unique groups.
#'
#' @export
getUniqueGroups <- function(x)
{
  # Determine possible unique groups, or clusters in the data
  # First create a data frame of predictors
  df.x <- as.data.frame(x)

  # One predictor
  if(ncol(df.x) == 1)
    return(as.factor(x))

  # Next convert each column into a factor, keep existing factors
  for (ii in 1:ncol(df.x)) {
    if (!is.factor(df.x[,ii])) {
      df.x[,ii] <- as.factor(df.x[,ii])
    }
  }
  #df.x <- data.frame(lapply(1:ncol(df.x), function(jj, df.x){ as.factor(df.x[, jj])}, df.x=df.x))

  # Finally merge the columns to obtain a single values for each row
  df.x$merged <- paste(as.numeric(df.x[, 1]))
  for (ii in 2:ncol(x))
  {
    df.x$merged <- paste(df.x$merged, as.numeric(df.x[, ii]), sep = "_")
  }

  # Unique groups
  factor.groups <- as.factor(df.x$merged)

  return(factor.groups)
}

#' Get cell means for unique groups of covariates
#'
#' @param x
#' a matrix-like object containing the covariates.
#' @param y
#' a vector of values to compute the means from.
#' @param factor.groups
#' a factor of unique groups of covariates.
#' @return
#' the cell means.
#'
#' @export
getCellMeans <- function(x, y, factor.groups)
{
  if(missing(x) && missing(factor.groups))
    stop("Provide either covariates or unique groups of covariates")
  if(missing(factor.groups))
    factor.groups <- getUniqueGroups(x)
  return(tapply(y, factor.groups, mean))
}

#' Get number of units in each cell
#'
#' @param x
#' a matrix-like object containing the covariates.
#' @param n
#' the total number of units.
#' @param factor.groups
#' a factor of unique groups of covariates.
#' @return
#' the number of units in each cell.
#'
#' @export
getCellSizes <- function(x, n, factor.groups)
{
  if(missing(x) && missing(factor.groups))
    stop("Provide either covariates or unique groups of covariates")
  if(missing(factor.groups))
    factor.groups <- getUniqueGroups(x)
  return(tapply(rep(1, n), factor.groups, sum))
}
