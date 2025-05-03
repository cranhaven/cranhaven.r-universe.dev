#'Sum of spell lengths exceeding daily threshold for n-dimensional arrays
#'
#'@description This function returns the number of spells of more than 
#'\code{min.length} days which exceed or are below the given \code{threshold} 
#'from daily data.
#'
#'@param data A n-dimensional array containing daily data.
#'@param threshold A n-dimensional array with the threshold to be/not to be 
#'  reach, usually given by the a percentile computed with the \code{Threshold} 
#'  function.
#'@param op The operator to use to compare data to threshold.
#'@param min.length The minimum spell length to be considered.
#'@param spells.can.span.years Whether spells can span years.
#'@param max.missing.days Maximum number of NA values per time period.
#'@param dates A vector of dates with a calendar attributes. If NULL 
#'  (by default), the 'time' attributes of parameter 'data' are considered.
#'@param timedim An integer number indicating the position of the time dimension 
#'  in the parameter \code{data}. If NULL (by default), the dimension called 
#'  'time' in parameter \code{data}.
#'@param calendar A character indicating the calendar type.
#'@param ncores The number of cores to be used when computing the extreme.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{\code{$output1}, an array with the same dimensions as the original 
#'        \code{data}, except the time dimension which is reduced to annual 
#'        resolution given a timeseries of maximum spell lengths for each year.}
#'  \item{\code{$year}, a vector indicating the corresponding years.}
#'}
#'@details This routine compares data to the thresholds using the given 
#'operator, generating a series of TRUE or FALSE values; these values are then 
#'filtered to remove any sequences of less than \code{min.length} days of TRUE 
#'values. It then computes the lengths of the remaining sequences of TRUE values 
#'(spells) and sums their lengths. The \code{spells.can.spa .years} option 
#'controls whether spells must always terminate at the end of a period, or 
#'whether they may continue until the criteria ceases to be met or the end of 
#'the data is reached. The default for fclimdex is FALSE.
#'
#'@import multiApply
#'@import PCICt
#'@examples
#'##Example synthetic data:
#'data <- 1:(2 * 3 * 310 * 1)
#'dim(data) <- c(time = 310, lon = 2, lat = 3, model = 1)
#'time <- as.POSIXct(paste(sort(rep(1902:1911, 31)), 1, 1:31, sep = "-"), tz = "CET")
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time', 
#'                             calendar = 'noleap', 
#'                             units = 'days since 1970-01-01 00:00:00', 
#'                             prec = 'double', 
#'                             dim = list(list(name = 'time', unlim = FALSE))))
#'attr(time, "variables") <- metadata
#'attr(data, 'Variables')$dat1$time <- time
#'threshold <- Threshold(data, dates = NULL, base.range = NULL, qtiles = 0.9, 
#'                       ncores = NULL)
#'res <- Extremes(data, threshold = threshold, op = ">", min.length = 6, 
#'                spells.can.span.years = TRUE, max.missing.days = 5, 
#'                ncores = NULL)
#'str(res)
#'
#'@export
Extremes <- function(data, threshold, op = ">", min.length = 6, 
                     spells.can.span.years = TRUE, max.missing.days = 5, 
                     dates = NULL, timedim = NULL, calendar = NULL, 
                     ncores = NULL) {
  if (is.null(data) | is.null(threshold)) {
    stop("Parameter 'data' and 'threshold' cannot be NULL.")
  }
  if (!is.numeric(data) | !is.numeric(threshold)) {
    stop("Parameter 'data' and 'threshold' must be numeric.")
  }
  if (is.null(dim(data))) {
    dim(data) = c(time = length(data))
    timedim = 1
  }
  if (is.null(dim(threshold))) {
    if (length(threshold) > 1) {
      dim(threshold) <- c(jdays = length(threshold))
    } else {
      threshold = rep(threshold, dim(data)[timedim])
      dim(threshold) <- c(jdays = length(threshold))
    }
  }
  if (!is.null(names(dim(threshold)))) {
    time_dim_threshold <- which(names(dim(threshold)) == "jdays" | names(dim(threshold)) == "time") 
  } else {
    stop("Parameter 'threshold' must have a dimension called 'jdays'.")
  }
  if (is.null(timedim)) {
    timedim <- which(names(dim(data)) == "time")
  }
  if (is.null(timedim)) {
    stop("No time dimension provided in parameter 'timedim' nor as dimension names of parameter 'data'.")
  }
  # Check dates
  if (is.null(dates)) {
    dates <- attr(data, 'Variables')$common$time
    if (is.null(dates)) {
      dates <- attr(data, 'Variables')$dat1$time
    }
  }
  if (is.null(dates)) {
    stop("No dates provided in parameter 'dates' nor as attribute of parameter 'data' or 'dates'.")
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
    dates <- try( {
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
    if (length(dates) != dim(data)[timedim]) {
      stop_error <- TRUE
    }
  }
  if (stop_error) {
    stop("Parameter 'dates' must be of the same length as the 'time' dimension of the parameter 'data'.")
  }
  dates <- as.PCICt(dates, cal = calendar)
  dates = as.character(dates)
  jdays <- as.numeric(strftime(dates, format = "%j"))
  if (calendar == "gregorian" | calendar == "standard" | calendar == "proleptic_gregorian") {
    year <- as.numeric(strftime(dates, format = "%Y"))
    if (length(unique(year)) > 1) {
      pos <- ((year / 100) %% 1 == 0) + ((year / 4) %% 1 == 0) + ((year / 400) %% 1 == 0)
      pos <- which(pos == 0 | pos == 2 | pos == 4)
      if (length(pos) > 0) {
        pos <- pos[which(jdays[pos] > 59)] 
        jdays[pos] <- jdays[pos] + 1
      }
    }
  }
  if (!is.numeric(min.length) | !is.numeric(max.missing.days)) {
    stop("Parameters 'min.length' and 'max.missing.days' must be numeric.")
  }
  if (length(min.length) > 1) {
    min.length = min.length[1]
    warning("Parameter 'min.length' has length > 1 and only the first element will be used.")
  }
  if (length(max.missing.days) > 1) {
    max.missing.days = max.missing.days[1]
    warning("Parameter 'max.missing.days' has length > 1 and only the first element will be used.")
  }
  if (!is.logical(spells.can.span.years)) {
    stop("Parameter 'spells.can.span.years' must be logical.")
  }
  if (length(spells.can.span.years) > 1) {
    spells.can.span.years = spells.can.span.years[1]
    warning("Parameter 'spells.can.span.years' has length > 1 and only the first element will be used.")
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
  
  if (length(unique(jdays)) != dim(threshold)[time_dim_threshold]) {
    if (dim(threshold)[time_dim_threshold] != 1) {
      stop("Length of 'jdays' dimension provided in parameter 'threshold' must be the consistent with 'jdays' provided in parameter 'dates.")
    } else {
      threshold = rep(threshold, length(unique(jdays)))
      dim(threshold) = c(jdays = length(unique(jdays)))
      warning("Parameter 'threshold' has been recycled.")
    }
  }
  if (length(dim(threshold)) == 1) {
    if (length(dim(data)) > 1) {
      threshold <- rep(threshold, prod(dim(data)[-timedim]))
      dim(threshold) <- c(jdays = length(unique(jdays)), dim(data)[-timedim])
      names(dim(threshold)[-time_dim_threshold]) <- names(dim(data)[-timedim])
      warning("Parameter 'threshold' has been recycled.")
    }
  }
  margins <- list(c(1 : length(dim(data)))[-c(timedim)], 
                  c(1 : length(dim(threshold)))[-c(time_dim_threshold)])
  data <- list(data, threshold)
  date.factor <- as.factor(substr(dates, 1, 4))
  if (length(dim_names) > 1) {  
    exceedance <- Apply(data = data, target_dims = list(timedim, time_dim_threshold), 
                        fun = .Extremes,
                        date.factor = date.factor, jdays = jdays, op = op, 
                        min.length = min.length, 
                        spells.can.span.years = spells.can.span.years, 
                        max.missing.days = max.missing.days, ncores = ncores)
    names(dim(exceedance$output1)) <- dim_names[-timedim]
    exceedance$year <- unique(as.numeric(as.vector(date.factor)))
  } else {
    exceedance <- list()
    exceedance$output1 <- .Extremes(data = data[[1]], threshold = data[[2]], date.factor = date.factor, 
                                    jdays = jdays, op = op, min.length = min.length, 
                                    spells.can.span.years = spells.can.span.years, 
                                    max.missing.days = max.missing.days) 
    dim(exceedance$output1) <- c(year = length(exceedance$output1))
    exceedance$year <- unique(as.numeric(as.vector(date.factor)))
  }
  return(exceedance = exceedance)
}
.Extremes <- function(data, threshold, date.factor, jdays, op, min.length, spells.can.span.years,
                      max.missing.days) {
  result <- threshold.exceedance.duration.index(data, date.factor, jdays, 
                                                threshold,op, min.length, 
                                                spells.can.span.years, max.missing.days)
}

