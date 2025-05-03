#'Daily thresholds based on quantiles for n-dimensional arrays
#'
#'This function computes the threshold based on a quantile value for each day of 
#'the year of the daily data input.
#'
#'@param data A numeric n-dimensional array containing daily data.
#'@param dates A vector of dates with a calendar attributes. If NULL (by 
#'  default), the 'time' attributes of parameter 'data' is considered.
#'@param calendar A character indicating the calendar type.
#'@param base.range The years used for computing the threshold.
#'@param qtiles Numeric vector with values between 0 and 1 indicating the 
#'  quantiles to be computed.
#'@param ncores The number of cores to be used when computing the threshold.
#'@param na.rm A logical value. If TRUE, any NA and NaN's are removed before the 
#'  quantiles are computed (default as FALSE).
#'
#'@return An array with similar dimensions as the \code{data} input, but without 
#''time' dimension, and a new 'jdays' dimension.
#'
#'@import multiApply
#'@import PCICt
#'@importFrom stats quantile
#'@examples
#'##Example synthetic data:
#'data <- 1:(2 * 3 * 372 * 1)
#'dim(data) <- c(time = 372, lon = 2, lat = 3, model = 1)
#'time <- as.POSIXct(paste(sort(rep(1900:1911, 31)), 1, 1:31, sep = "-"), 
#'                   tz = "CET")
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time',  
#'                 calendar = 'noleap', 
#'                 units = 'days since 1970-01-01 00:00:00', prec = 'double', 
#'                 dim = list(list(name = 'time', unlim = FALSE))))
#'attr(time, "variables") <- metadata
#'attr(data, 'Variables')$dat1$time <- time
#'
#'a <- Threshold(data, dates = NULL, base.range = NULL, qtiles = 0.9, 
#'               ncores = NULL)
#'str(a)
#'
#'@export
Threshold <- function(data, dates = NULL, calendar = NULL, base.range = NULL, qtiles = 0.9, ncores = NULL, na.rm = FALSE) {
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {
    dim(data) <- c(time = length(data))
  }
  if (length(dim(data)) == 1 && is.null(names(dim(data)))) {
    names(dim(data)) <- 'time'
  }
  time_dim <- 1
  if (length(dim(data)) > 1) {
    if (!('time' %in% names(dim(data)))) {
      stop("Parameter 'data' must contain a dimension named 'time'.")
    } else {
      time_dim <- which(names(dim(data)) == 'time')
    }
  }
  if (is.null(dates)) {
    dates <- attr(data, 'Variables')$common$time
    if (is.null(dates)) {
      dates <- attr(data, 'Variables')$dat1$time
    }
    if (is.null(dates)) {
      stop("No dates provided in parameter 'dates' nor as attribute of parameter 'data'.")
    }
  }
  if (is.null(calendar)) {
    calendar <- attributes(dates)$calendar
    if (is.null(calendar)) {
      calendar <- attributes(dates)$variables$time$calendar
    }
    if (is.null(calendar)) {
      stop("The attribute 'calendar' must be present in the parameter 'dates' or specified in parameter 'calendar'.")
    }
  }  
  if (!any(class(dates) %in% c('POSIXct'))) {
    dates <- try({
      if (is.character(dates)) {
        as.POSIXct(dates, format = "%Y%m%d")
      } else {
        as.POSIXct(dates)
      }
    })
    if ('try-error' %in% class(dates) | sum(is.na(dates)) == length(dates)) {
      stop("Dates provided in parameter 'dates' or as attribute of parameter 'data' must be of class 'POSIXct' or convertable to 'POSIXct'.")
    }
  }
  stop_error <- FALSE
  if (length(dim(data)) == 1) {
    if (length(dates) != length(data)) {
      stop_error <- TRUE
    }
  } else {
    if (length(dates) != dim(data)['time']) {
      stop_error <- TRUE
    }
  }
  if (stop_error) {
    stop("Parameter 'dates' must be of the same length as the 'time' dimension of the parameter 'data'.")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be logical.")
  }
  if (length(na.rm) > 1) {
    na.rm <- na.rm[1]
    warning("Parameter 'na.rm' has length > 1 and only the first ",
            "element will be used.")
  }
  dates <- as.PCICt(dates, cal = calendar)
  dates <- as.POSIXlt(as.character(dates), format = "%Y-%m-%d")
  jdays <- as.numeric(strftime(dates, format = "%j"))
  if (calendar == "gregorian" | calendar == "standard" | calendar == "proleptic_gregorian") {
    year <- as.numeric(strftime(dates, format = "%Y"))
    pos <- ((year / 100) %% 1 == 0) + ((year / 4) %% 1 == 0) + ((year / 400) %% 1 == 0)
    pos <- which(pos == 0 | pos == 2 | pos == 4)
    if (length(pos) > 0) {
      pos <- pos[which(jdays[pos] > 59)] 
      jdays[pos] <- jdays[pos] + 1
    }
  }  
  date.factor <- as.factor(substr(dates, 1, 4))
  years <- levels(date.factor)
  if (is.null(base.range)) {
    base.range <- c(as.numeric(years[1]), as.numeric(years[length(years)]))
  } else if (!is.numeric(base.range)) {
    stop("Parameter 'base.range' must be numeric.")
  } else if (length(base.range) != 2) {
    stop("Parameter 'base.range' must be of length 2.")
  }
  if (!is.numeric(qtiles)) {
    stop("Parameter 'qtiles' must be numeric.")
  } else if (length(qtiles) < 1) {
    stop("Parameter 'qtiles' must contain at least one threshold.")
  } else if (qtiles < 0 || qtiles > 1) {
    stop("Parameter 'qtiles' must be in the range [0, 1].")
  }
  if (!is.numeric(ncores) & !is.null(ncores)) {
    stop("Parameter 'ncores' must be numeric.")
  }
  if (length(ncores) == 0 & !is.null(ncores)) {
    stop("Parameter 'ncores' must be of length 1.")
  }
  if (length(ncores) > 1) {
    ncores = ncores[1]
    warning("Parameter 'ncores' has length > 1 and only the first element will be used.")
  }
  if (!is.null(ncores)) {
    ncores <- round(ncores)
    if (ncores == 0) {
      ncores = NULL
    }
  }
  if (is.null(names(dim(data)))) {
    dim_names <- paste0('dim', 1:length(dim(data)))
  } else {
    dim_names <- names(dim(data))
  }
  dims <- 1 : length(dim(data))
  margins <- list(c(1 : length(dim(data)))[-c(time_dim)])
  if (!is.null(base.range)) {
    year <- as.numeric(substr(dates, 1, 4))
    pos <-  year >= base.range[1] & year <= base.range[2]
    if (length(dim(data)) == 1) {
     data <- data[pos] 
     jdays <- jdays[pos]
    } else {
     data <- apply(data, margins[[1]], function(x){x[pos]})
     jdays <- jdays[pos]
     names(dim(data))[1] = 'time'
     pos <- NULL
     pos[time_dim] <- 1
     if (length(dim(data)) - time_dim > 0 ) {
       pos[(time_dim + 1):(time_dim + length(dim(data)) - time_dim)] = 
         (time_dim + 1):(time_dim + length(dim(data)) - time_dim)
     }
     if (time_dim > 1) {
       pos[1 : (time_dim - 1)] <- 2 : time_dim
     }
     data <- aperm(data, pos)
     names(dim(data)) <- dim_names
    }
  }
  if (length(dim(data)) > 1) {
    result <- Apply(data = data, margins = margins, 
                    fun = .Threshold, indices = jdays, qtiles = qtiles, 
                    ncores = ncores, na.rm = na.rm)
    names(dim(result$output1)) <- c("jdays", dim_names[-time_dim])
  } else {
    result <- list() 
    result$output1 <- .Threshold(data, indices = jdays, qtiles = qtiles)
    dim(result$output1) <- c(jdays = length(result$output1)) 
  }
  return(result$output1)
}
.Threshold <- function(data, indices,  qtiles, na.rm = FALSE) {
  tapply(X = data, INDEX = indices, FUN = quantile, probs = qtiles, na.rm = na.rm)
} 
