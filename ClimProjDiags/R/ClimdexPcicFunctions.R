#-----------------------------------------------------------
# Auxiliary functions from package climdex.pcic
# (2023-06-01) The package was removed from CRAN. To maintain ClimProjDiags,
# we copy the necessary functions here.
# We will recover the dependency when climdex.pcic is on CRAN again.
#-----------------------------------------------------------
# Package name: climdex.pcic
# Version: 1.1-11
# Date: 2020-01-21
# Title: PCIC Implementation of Climdex Routines
# Author: David Bronaugh <bronaugh@uvic.ca> for the Pacific Climate Impacts
#     Consortium
# Maintainer: James Hiebert <hiebert@uvic.ca>
# Depends:
#     R (>= 2.12.0),
#     PCICt (>= 0.5-4)
# Imports:
#     methods,
#     Rcpp (>= 0.11.4)
# Suggests:
#     compiler,
#     RUnit
# LinkingTo: Rcpp
# Description: PCIC's implementation of Climdex routines for computation of
#     extreme climate indices. Further details on the extreme climate indices
#     can be found at <http://etccdi.pacificclimate.org/list_27_indices.shtml>
#     and in the package manual.
# License: GPL-3
# URL: https://www.r-project.org
# LazyData: yes
# BugReports: https://github.com/pacificclimate/climdex.pcic/issues/
# RoxygenNote: 6.0.1
#-----------------------------------------------------------

## Helper functions:

# Lower overhead version of tapply
tapply.fast <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
  FUN <- if (!is.null(FUN))
    match.fun(FUN)
  
  if(!is.factor(INDEX))
    stop("INDEX must be a factor.")
  
  if (length(INDEX) != length(X))
    stop("arguments must have same length")
  
  if (is.null(FUN))
    return(INDEX)
  
  namelist <- levels(INDEX)
  ans <- lapply(split(X, INDEX), FUN, ...)
  
  ans <- unlist(ans, recursive = FALSE)
  names(ans) <- levels(INDEX)
  return(ans)
}

# Get year
get.years <- function(dates) {
  return(as.POSIXlt(dates)$year + 1900)
}

# Get NA mask given threshold and split factor
get.na.mask <- function(x, f, threshold) {
  return(c(1, NA)[1 + as.numeric(tapply.fast(is.na(x), f, function(y) { return(sum(y) > threshold) } ))])
}

#-----------------------------------------------------------

# Functions

#'Get series length at ends
#' 
#'This function takes a series of boolean values and returns a list of
#'integers of the same length corresponding to the lengths at the ends of
#'sequences of TRUE values.
#' 
#'It can often be useful to know how long a series of boolean values is. This
#'function provides a method of knowing where and how long such sequences are.
#' 
#'@param x Sequence of booleans.
#'@param na.value Value to replace NAs with.
#'@return A vector consisting of the lengths of sequences of TRUE values at
#'the location of the last TRUE value in the sequence, and zeroes elsewhere.
#'@keywords ts climate
#'@examples 
#'# Get lengths of sequences of TRUE values in a sequence
#'series.lengths <- get.series.lengths.at.ends(c(TRUE, TRUE, TRUE, FALSE,
#'TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
#'@noRd 
get.series.lengths.at.ends <- function(x, na.value=FALSE) {
  stopifnot(is.logical(x) && is.logical(na.value))
  n <- length(x)
  if(n == 1)
    return(as.numeric(x))

  res <- rep(0, n)
  x[is.na(x)] <- na.value

  ## Compare series to lag-1 and lag+1 series; false added to trigger state transition from TRUE at ends of series
  start <- which(x & !(c(FALSE, x[1:(n - 1)])))
  end <- which(x & !(c(x[2:n], FALSE)))
  res[end] <- end - start + 1
  return(res)
}

#'Lengths of strings of TRUE values
#' 
#'Computes fraction of days above or below the baseline threshold for each
#'day, and averages them using the date factor passed in.
#' 
#'This function computes fractions of days above or below baseline thresholds
#'for each day, then aggregates them using \code{date.factor}. It is used to
#'implement TN/TX 10/90p.
#' 
#'@param temp Sequence of temperature values.
#'@param dates Sequence of associated dates.
#'@param jdays Sequence of associated days of year.
#'@param date.factor Factor to aggregate data using.
#'@param threshold.outside.base Sequence of thresholds to be used for data
#'  outside the base period.
#'@param base.thresholds Data structure containing sets of thresholds to be
#'  used inside the base period; see \link{climdexInput-class}.
#'@param base.range Date range (type PCICt) of the baseline period.
#'@param op Comparison operator to use.
#'@param max.missing.days Maximum number of NA values per time period.
#'@return A vector consisting of the mean fraction of days above or below the
#'  supplied set of thresholds.
#'@note If date.factor is omitted, daily series will be returned.
#'@keywords ts climate
#'@noRd 
percent.days.op.threshold <- function(temp, dates, jdays, date.factor, 
                                      threshold.outside.base, base.thresholds, 
                                      base.range, op='<', max.missing.days) {
  f <- match.fun(op)
  dat <- f(temp, threshold.outside.base[jdays])
  
  inset <- dates >= base.range[1] & dates <= base.range[2]
  ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
  if(sum(inset) > 0 && length(dates) >= 360 * 2) {
    jdays.base <- jdays[inset]
    years.base <- get.years(dates[inset])

    ## Get number of base years, subset temp data to base period only.
    temp.base <- temp[inset]
    years.base.range <- range(years.base)
    byrs <- (years.base.range[2] - years.base.range[1] + 1)

    ## Linearize thresholds, then compare them to the temperatures
    bdim <- dim(base.thresholds)
    dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
    yday.byr.indices <- jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
    f.result <- f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
    dim(f.result) <- c(length(yday.byr.indices), bdim[3])

    ## Chop up data along the 2nd dim into a list; sum elements of the list
    dat[inset] <- rowSums(f.result, na.rm=TRUE) / (byrs - 1)
  }
  dat[is.nan(dat)] <- NA
  if(missing(date.factor))
    return(dat)
  na.mask <- get.na.mask(dat, date.factor, max.missing.days)
  ## FIXME: Need to monthly-ize the NA mask calculation, which will be ugly.
  ret <- tapply.fast(dat, date.factor, mean, na.rm=TRUE) * 100 * na.mask
  ret[is.nan(ret)] <- NA
  return(ret)
}

#'Sum of spell lengths exceeding daily threshold
#' 
#'This function returns the number of spells of more than \code{min.length}
#'days which exceed or are below the given threshold.
#' 
#'@details
#'This routine compares data to the thresholds using the given operator,
#'generating a series of TRUE or FALSE values; these values are then filtered
#'to remove any sequences of less than \code{min.length} days of TRUE values.
#'It then computes the lengths of the remaining sequences of TRUE values
#'(spells) and sums their lengths.
#' 
#'The \code{spells.can.span.years} option controls whether spells must always
#'terminate at the end of a period, or whether they may continue until the
#'criteria ceases to be met or the end of the data is reached. The default for
#'fclimdex is FALSE.
#' 
#'@param daily.temp Data to compute index on.
#'@param date.factor Date factor to split by.
#'@param jdays Timeseries of days of year.
#'@param thresholds The thresholds to compare to.
#'@param op The operator to use to compare data to threshold.
#'@param min.length The minimum spell length to be considered.
#'@param spells.can.span.years Whether spells can span years.
#'@param max.missing.days Maximum number of NA values per time period.
#'@return A timeseries of maximum spell lengths for each period.
#'@seealso \code{\link{climdex.wsdi}}.
#'@keywords ts climate
#'@examples
#'prec.dat <- c(0.1, 3.0, 4.3, 1.9, 1.3, 6.0, 0, 0, 4.0, 1)
#'phony.date.factor <- factor(rep(1:2, each=5))
#' 
#'## With spells spanning years...
#'alttedi <- threshold.exceedance.duration.index(prec.dat,
#'phony.date.factor, rep(1:5, 2), rep(1, 5), ">=", 2, TRUE, 1)
#' 
#'## Without spells spanning years...
#'tedi <- threshold.exceedance.duration.index(prec.dat, phony.date.factor,
#'rep(1:5, 2), rep(1, 5), ">=", 2, FALSE, 1)
#'@noRd 
threshold.exceedance.duration.index <- function(daily.temp, date.factor, jdays, 
                                                thresholds, op=">", 
                                                min.length=6, 
                                                spells.can.span.years=TRUE, 
                                                max.missing.days) {
  stopifnot(is.numeric(c(daily.temp, thresholds, min.length)), is.factor(date.factor),
            is.function(match.fun(op)),
            min.length > 0)
  f <- match.fun(op)
  na.mask <- get.na.mask(is.na(daily.temp + thresholds[jdays]), date.factor, max.missing.days)

  if(spells.can.span.years) {
    periods <- select.blocks.gt.length(f(daily.temp, thresholds[jdays]), min.length - 1)
    return(tapply.fast(periods, date.factor, sum) * na.mask)
  } else {
    ## fclimdex behaviour...
    return(tapply.fast(1:length(daily.temp), date.factor, function(idx) { sum(select.blocks.gt.length(f(daily.temp[idx], thresholds[jdays[idx]]), min.length - 1)) } ) * na.mask)
  }
}

#'Number of days (less than, greater than, etc) a threshold
#' 
#'@description
#'Produces sums of values that exceed (or are below) the specified threshold.
#' 
#'@details
#'This function takes a data series, the number of days in the running window,
#'a date factor to aggregate by, and an optional modifier parameter
#'(center.mean.on.last.day). It computes the n-day running sum of
#'precipitation and returns the maximum n-day total precipitation per unit
#'time, as defined by \code{date.factor}.
#' 
#'@param daily.prec Daily timeseries of precipitation.
#'@param date.factor Factor to aggregate by.
#'@param ndays Number of days in the running window.
#'@param center.mean.on.last.day Whether to center the n-day running mean on
#'  the last day of the series, instead of the middle day.
#'@return A vector consisting of the maximum n-day sum of precipitation per
#'time interval.
#'@keywords ts climate
#'@noRd 
nday.consec.prec.max <- function(daily.prec, date.factor, ndays, 
                                 center.mean.on.last.day=FALSE) {
  if (ndays == 1) {
    return(suppressWarnings(tapply.fast(daily.prec, date.factor, max, na.rm=TRUE)))
  }
  ## Ends of the data will be de-emphasized (padded with zero precip data); NAs replaced with 0
  daily.prec[is.na(daily.prec)] <- 0
  prec.runsum <- running.mean(daily.prec, ndays)
  prec.runsum[is.na(prec.runsum)] <- 0
  if(center.mean.on.last.day) {
      k2 = ndays %/% 2
      prec.runsum <- c(rep(0, k2), prec.runsum[1:(length(prec.runsum) - k2)])
  }
  return(tapply.fast(prec.runsum, date.factor, max) * ndays)
}

#'Maximum spell length
#' 
#'@description
#'This function returns the longest string of days which exceed or are below
#'the given threshold.
#' 
#'@details
#'This routine compares data to the threshold using the given operator,
#'generating a series of TRUE or FALSE values. It then computes the lengths of
#'sequences of TRUE values (spells) and chooses the longest spell in each
#'period (as defined by date.factor).
#' 
#'The \code{spells.can.span.years} option controls whether spells must always
#'terminate at the end of a period, or whether they may continue until the
#'criteria ceases to be met or the end of the data is reached. The default for
#'fclimdex is TRUE.
#' 
#'@param daily.prec Data to compute index on.
#'@param date.factor Date factor to split by.
#'@param threshold The threshold to compare to.
#'@param op The operator to use to compare data to threshold.
#'@param spells.can.span.years Whether spells can span years.
#'@return A timeseries of maximum spell lengths for each period.
#'@seealso \code{\link{climdex.cdd}}.
#'@keywords ts climate
#'@examples
#' 
#'prec.dat <- c(0.1, 3.0, 4.3, 1.9, 1.3, 6.0, 0, 0, 4.0, 1)
#'phony.date.factor <- factor(rep(1:2, each=5))
#' 
#'## With spells spanning years...
#'cwd <- spell.length.max(prec.dat, phony.date.factor, 1, ">=", TRUE)
#' 
#'## Without spells spanning years...
#'altcwd <- spell.length.max(prec.dat, phony.date.factor, 1, ">=", FALSE)
#'@noRd 
spell.length.max <- function(daily.prec, date.factor, threshold, op, 
                             spells.can.span.years) {
  bools <- match.fun(op)(daily.prec, threshold)

  if(spells.can.span.years) {
    all.true <- tapply.fast(bools, date.factor, all)
    max.spell <- tapply.fast(get.series.lengths.at.ends(bools), date.factor, max)
    
    ## Mask out values which are in the middle of a spell with NA
    na.mask <- c(1, NA)[as.integer((max.spell == 0) & all.true) + 1]
    return(max.spell * na.mask)
  } else {
    return(tapply.fast(bools, date.factor, function(x) { max(get.series.lengths.at.ends(x)) }))
  }
}

#'Select blocks of TRUE values of sufficient length.
#' 
#'Produces a sequence of booleans of the same length as input, with sequences
#'of TRUE values shorter than n replaced with FALSE.
#' 
#'This function takes a series of booleans and returns a sequence of booleans
#'of equal length, with all sequences of TRUE of length \code{n} or shorter
#'replaced with sequences of FALSE. NA values are replaced with
#'\code{na.value}.
#' 
#'@param d Sequence of booleans.
#'@param n Longest sequence of TRUE to replace with FALSE.
#'@param na.value Values to replace NAs with.
#'@return A vector of booleans, with the length \code{n} or less sequences of
#'TRUE replaced with FALSE.
#'@keywords ts climate
#'@examples
#' 
#'## Return only the first sequence of TRUE... second sequence will be FALSE.
#'foo <- select.blocks.gt.length(c(rep(TRUE, 4), FALSE, rep(TRUE, 3)), 3)
#'@noRd 
select.blocks.gt.length <- function(d, n, na.value = FALSE) {
  stopifnot(is.logical(d), is.numeric(n))

  if(n < 1)
    return(d)

  if(n >= length(d))
    return(rep(FALSE, length(d)))

  d[is.na(d)] <- na.value
  
  d2 <- Reduce(function(x, y) { return(c(rep(FALSE, y), d[1:(length(d) - y)]) & x) }, 1:n, d)
  return(Reduce(function(x, y) { return(c(d2[(y + 1):length(d2)], rep(FALSE, y)) | x) }, 1:n, d2))
}

#'Running Mean of a Vector
#'
#'@description Calculates the running means of a vector with a shifting window
#'
#'@details Returns a new vector the same length as vec, where the ith
#'element is the mean of the bin of elements centered at the ith element
#'of the original vector. Means cannot be calculated for elements less
#'than half the width of the bin from the beginning or end of the vector;
#'the result vector has NA in those positions.
#'
#'@param vec A vector
#'@param bin The number of entries to average over for each mean
#'
#'@return a vector containing the running mean of bin elements of vec
#'
#'@examples 
#'running.mean(c(1, 2, 3, 4, 5, 6), 2) 
#'running.mean(c(5, 5, 5, 5, 5), 4) 
#'@noRd 
running.mean <- function(vec, bin) {
  vec = as.vector(vec)
  len = length(vec)
  if (bin<=1) {
    return (vec)
  }
  if (bin > len) {
    bin = len
  }
  left.bin = bin%/%2

  means = double(len)

  right.bin = bin - left.bin - 1
  means = c( sum(vec[1:bin]), diff(vec,bin) ) # find the first sum and the differences from it
  means = cumsum(means)/bin                  # apply precomputed differences
  means = c(rep(NA,left.bin), means, rep(NA,right.bin))   # extend to original vector length
  return(means)
}
