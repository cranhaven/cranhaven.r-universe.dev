#' Create data for map
#' 
#' Provide a data.frame with samples.
#' 
#' @param time A vector for cumulative time
#' @param intervals A vector for time intervals
#' @return A dataframe
#' 
#' @note
#' - If both time and intervals are used, time is used.
#' - map.time is given by a special case of map.group.
#' 
#' 
#' @examples
#' x <- runif(10)
#' 
#' dat <- data.frame.map.time(time=x)
#' mean(dat)
#' print(dat)
#' 
#' @export

data.frame.map.time <- function(time, intervals) {
  if (missing(intervals) && missing(time)) {
      stop("Error: either time or intervals is needed")
  }
  if (!missing(time)) {
    time <- sort(time)
    if (time[1] != 0) {
      intervals <- diff(c(0, time))
    } else {
      intervals <- diff(time)
    }
  }
  counts <- rep(0, length(intervals))
  instants <- rep(1, length(intervals))
  data <- list(
    counts = counts,
    intervals = intervals,
    instants = instants,
    maxinterval = max(intervals),
    maxcount = max(counts))
  class(data) <- "map.time"
  data
}

#' @aliases data.frame.map.time
#' @export
print.map.time <- function(x, ...) {
  print(data.frame(intervals=x$intervals))
}

#' @aliases data.frame.map.time
#' @export
mean.map.time <- function(x, ...) {
  sum(x$intervals) / length(x$intervals)
}

#' Create group data for map
#' 
#' Provide the data.frame for group data.
#' 
#' @param counts A vector of the number of samples
#' @param breaks A vector of break points
#' @param intervals A vector of differences of time
#' @param instants A vector meaning whether a sample is observed at the end of break.
#' @return A dataframe
#' 
#' @examples
#' t <- c(1,1,1,1,1)
#' n <- c(1,3,0,0,1)
#' 
#' dat <- data.frame.map.group(counts=n, intervals=t)
#' mean(dat)
#' print(dat)
#' 
#' @export

data.frame.map.group <- function(counts, breaks, intervals, instants) {
  # replace na to -1
  counts[is.na(counts)] <- -1
  
  if (missing(breaks)) {
    if (missing(intervals)) {
      breaks <- 0:length(counts)
    } else {
      breaks <- c(0,cumsum(intervals))
    }
  }

  # check instants
  if (missing(instants)) {
    instants <- array(0, length(counts))
  }
  # check for left-truncation
  if (breaks[1] != 0) {
    breaks <- c(0, breaks)
    counts <- c(NA, counts)
    instants <- c(0, instants)
  }
  dt <- diff(breaks)
  
  # check
  if (! (length(dt) == length(counts) && length(dt) == length(instants))) {
    stop(sprintf("The length of time, counts, indicators should be same. intervals=%d, counts=%d, instants=%d",
         length(dt), length(counts), length(instants)))
  }
  
  data <- list(
    counts = counts,
    intervals = dt,
    instants = instants,
    maxinterval = max(dt),
    maxcount = max(counts))
  class(data) <- "map.group"
  data
}

#' @aliases data.frame.map.group
#' @export
print.map.group <- function(x, ...) {
  print(data.frame(counts=x$counts, intervals=x$intervals, instants=x$instants))
}

#' @aliases data.frame.map.group
#' @export
mean.map.group <- function(x, ...) {
  sum(x$intervals) / (sum(x$counts) + sum(x$instants))
}
