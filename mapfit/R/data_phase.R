#' Create data for phase with weighted sample
#' 
#' Provide a data.frame with weighted samples.
#' 
#' @param x A vector of point (quantiles)
#' @param weights A vector of weights
#' @return A dataframe
#' 
#' @note 
#' The point time is sorted and their differences are stored as the column of `time`
#' 
#' @examples
#' x <- runif(10)
#' w <- runif(10)
#' 
#' dat <- data.frame.phase.time(x=x, weights=w)
#' print(dat)
#' mean(dat)
#' 
#' @export

data.frame.phase.time <- function(x, weights) {
  if (missing(weights)) {
    weights <- array(1, length(x))
  }
  tt <- diff(c(0, sort(x)))

  # check
  if (! (length(tt) == length(weights))) {
    stop(sprintf("The length of time and weights should be same. time=%d, weights=%d",
                 length(time), length(weights)))
  }

  data <- list(time=tt, weights=weights[order(x)],
               maxtime=max(tt))
  class(data) <- "phase.time"
  data
}

#' @aliases data.frame.phase.time
#' @export
print.phase.time <- function(x, ...) {
  print(data.frame(x=x$time, weights=x$weights))
}

#' @aliases data.frame.phase.time
#' @export
mean.phase.time <- function(x, ...) {
  sum(cumsum(x$time) * x$weights) / sum(x$weights)
}

#' Create group data for phase
#' 
#' Provide the data.frame for group data.
#' 
#' @param counts A vector of the number of samples
#' @param breaks A vector of break points
#' @param intervals A vector of differences of time
#' @param instants A vector meaning whether a sample is observed at the end of break.
#' @return A dataframe
#' @examples
#' dat <- data.frame.phase.group(counts=c(1,2,1,1,0,0,1,4))
#' print(dat)
#' mean(dat)
#' 
#' @export

data.frame.phase.group <- function(counts, breaks, intervals, instants) {
  # replace na to -1
  counts[is.na(counts)] <- -1
  
  if (missing(breaks)) {
    if (missing(intervals)) {
      breaks <- 0:length(counts)
    } else {
      breaks <- c(0,cumsum(intervals))
    }
  }
  # check for glast
  if (is.infinite(breaks[length(breaks)])) {
    glast <- counts[length(counts)]
    counts <- counts[-length(counts)]
    breaks <- breaks[-length(breaks)]
  } else {
    glast <- 0
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
    maxcount = max(counts),
    lastcount = glast)
  class(data) <- "phase.group"
  data
}

#' @aliases data.frame.phase.group
#' @export
print.phase.group <- function(x, ...) {
  print(data.frame(counts=c(x$counts,x$last), intervals=c(x$intervals,+Inf), instants=c(x$instants,NA)))
}

#' @aliases data.frame.phase.group
#' @export
mean.phase.group <- function(x, ...) {
  i <- is.finite(x$intervals) & is.finite(x$counts) & is.finite(x$instants)
  time <- cumsum(x$intervals[i])
  ind <- x$instants[i] == 1
  counts <- x$counts[i]
  m <- sum(time * counts) + sum(time[ind])
  m / (sum(counts) + sum(ind))
}
