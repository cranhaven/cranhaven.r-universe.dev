#'Wrapper for applying the climdex routine  ETCCDI climate change indices to 
#'n-dimensional arrays.
#'
#'@description This function computes the t90p, t10p, cdd or rx5day indices from 
#'n-dimensional arrays.
#'
#'@param data A numeric n-dimensional array containing daily maximum or minimum 
#'  temperature, wind speed or precipitation amount.
#'@param metric The metric to be computed, either 't90p', 't10p', 'Wx', 'cdd' or 
#'  'rx5day'.
#'@param threshold For the 't90p' and 't10p' metrics, an array of the 90th/10th 
#'  percentiles must be included. This parameter can be computed with the 
#'  \code{Threshold} function.
#'@param base.range The years used for the reference period. If NULL 
#'  (by default), all years are used.
#'@param dates A vector of dates with a calendar attributes. If NULL 
#'  (by default), the 'time' attributes of parameter 'data' are considered.
#'@param timedim An integer number indicating the position of the time dimension 
#'  in the parameter \code{data}. If NULL (by default), the dimension called 
#'  'time' in parameter \code{data} is considered as temporal dimension.
#'@param calendar A character indicating the calendar type.
#'@param ncores The number of cores to be used when computing the index.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{\code{$result}, an array with the same dimensions as the input array, 
#'        except for the temporal dimension which is renamed to 'year', moved 
#'        to the first dimension position and reduce to annual resolution.}
#'  \item{\code{$years}, a vector of the corresponding years.}
#'}
#'
#'@import multiApply
#'@import PCICt
#'@examples 
#'##Example synthetic data:
#'data <- 1:(2 * 3 * 372 * 1)
#'dim(data) <- c(lon = 2, lat = 3, time = 372, model = 1)
#'time <- c(seq(ISOdate(1900, 1, 1), ISOdate(1900, 1, 31), "day"), 
#'          seq(ISOdate(1901, 1, 1), ISOdate(1901, 1, 31), "day"),
#'          seq(ISOdate(1902, 1, 1), ISOdate(1902, 1, 31), "day"),
#'          seq(ISOdate(1903, 1, 1), ISOdate(1903, 1, 31), "day"),
#'          seq(ISOdate(1904, 1, 1), ISOdate(1904, 1, 31), "day"),
#'          seq(ISOdate(1905, 1, 1), ISOdate(1905, 1, 31), "day"),
#'          seq(ISOdate(1906, 1, 1), ISOdate(1906, 1, 31), "day"),
#'          seq(ISOdate(1907, 1, 1), ISOdate(1907, 1, 31), "day"),
#'          seq(ISOdate(1908, 1, 1), ISOdate(1908, 1, 31), "day"),
#'          seq(ISOdate(1909, 1, 1), ISOdate(1909, 1, 31), "day"),
#'          seq(ISOdate(1910, 1, 1), ISOdate(1910, 1, 31), "day"),
#'          seq(ISOdate(1911, 1, 1), ISOdate(1911, 1, 31), "day"))
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time', 
#'                             calendar = 'gregorian', 
#'                             units = 'days since 1970-01-01 00:00:00', 
#'                             prec = 'double', 
#'                             dim = list(list(name = 'time', unlim = FALSE))))
#'attr(time, "variables") <- metadata
#'attr(data, 'Variables')$dat1$time <- time
#'
#'thres <- rep(10, 31 * 2 * 3)
#'dim(thres) <- c(jdays = 31, lon = 2, lat = 3,  model = 1)
#'str(thres)
#'
#'clim <- Climdex(data, metric = "t90p", threshold = thres)
#'str(clim)
#'@references David Bronaugh for the Pacific Climate Impacts Consortium (2015).
#'  climdex.pcic: PCIC Implementation of Climdex Routines. R package
#'  version 1.1-6. http://CRAN.R-project.org/package=climdex.pcic
#'@export
Climdex <- function(data, metric, threshold = NULL, base.range = NULL, 
                    dates = NULL, timedim = NULL, calendar = NULL, 
                    ncores = NULL) {
  if (is.null(data) | is.null(metric)) {
    stop("Parameters 'data' and 'metric' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameters 'data' must be numeric.")
  }
  if (is.null(dim(data))) {
    dim(data) = c(time = length(data))
    timedim = 1
  }
  if (is.null(timedim) & !is.null(names(dim(data)))) {
    timedim <- which(names(dim(data)) == "time")
  }
  if (is.null(timedim)) {
    stop("No time dimension provided in parameter 'timedim' nor as dimension names of parameter 'data'.")
  }
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
  if (!any(class(dates) %in% 'POSIXct')) {
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
  if (!is.character(metric) | (metric != "cdd" & metric != "t90p" & metric != "t10p" & metric != "rx5day" & 
                               metric != "Wx")) {
    stop("Parameter 'metric' must be a character indicating the metric to be computed, either 't90p', 't10p', 'Wx', 'cdd' or 'rx5day'.")
  }
  if (length(metric) > 1) {
    metric = metric[1]
    warning("Parameter 'metric' has length > 1 and only the first element will be used.")
  }
  if ((metric == "t90p" | metric == "t10p" | metric == "Wx") & is.null(threshold)) {
    stop("Parameter 'threshold' cannot be NULL for metric 't90p', 't10p' or 'Wx'.")
  }
  if ((metric == "cdd" | metric == "rx5day") & !is.null(threshold)) {
    threshold = NULL
    warning("Parameter 'threshold' haven't be used when computing metric 'cdd' or 'rx5day'.")
  }
  if ((metric == "t90p" | metric == "t10p" | metric == "Wx") & !is.null(threshold)) { 
    if (!is.null(names(dim(threshold)))) {
      time_dim_threshold <- which(names(dim(threshold)) == "jdays" | names(dim(threshold)) == "time") 
    } else {
      if (is.null(dim(threshold))) {
        time_dim_threshold <- 1
        dim(threshold) <- c(jdays = length(threshold))
      } else {
        stop("Parameter 'threshold' must have a dimension called 'jdays'.")
      }
    }
  }
  if (!is.null(threshold)) {
    if (!is.numeric(threshold)) {
      stop("Parameter 'threshold' must be numeric.")
    }
  }
  if (is.null(names(dim(data)))) {
    names(dim(data)) <- paste0("dims", 1 : length(dim(data)))
    names(dim(data))[timedim] <- "time"
  } 
  dims <- dim(data)
  if (metric == "t90p" | metric == "t10p" | metric == "Wx") {
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
  if (metric == "cdd" || metric == "rx5day") {
    dates <- as.factor(substr(dates, 1, 4))
    date.factor <- NULL
    jdays <- NULL
    years <- levels(dates)
    data <- list(data)
    target_dims <- list(timedim)
  } else if (metric == "t90p" || metric == "t10p" || metric == "Wx") {
    date.factor <- as.factor(substr(dates, 1, 4))
    years <- levels(date.factor)
    data <- list(data, threshold)
    target_dims <- list('time', 'jdays')
  }
  if (length(dims) > 1) {
    result <- Apply(data = data, fun = .Climdex,
                    # margins = list(2, 1),
                    target_dims = target_dims, 
                    output_dims = 'year',
                    dates = dates, date.factor = date.factor, metric = metric,
                    jdays = jdays, base.range = base.range, ncores = ncores)
  } else {
    result <- list()
    result$output1 <- .Climdex(data = data[[1]], threshold = data[[2]], dates = dates, metric = metric, 
                               jdays = jdays, date.factor = date.factor, base.range = base.range)
  }
  return(list(result = result$output1, years = as.numeric(years)))
}
.Climdex <- function(data, threshold, dates = dates, metric = metric,
                     jdays = jdays, date.factor = NULL, base.range = NULL) {
   if (metric == "cdd") {
    result <- spell.length.max(daily.prec = as.vector(data), date.factor = dates, 1, "<", FALSE)
  } else if (metric == "rx5day") {
    result <- nday.consec.prec.max(daily.prec = as.vector(data), date.factor = dates, 
                                   ndays = 5, center.mean.on.last.day = FALSE)
  } else if (metric == "t90p" || metric == "Wx") {
    result <- percent.days.op.threshold(temp = data, dates = dates, jdays = jdays, 
                                        date.factor = date.factor, threshold.outside.base = threshold, 
                                        base.thresholds = threshold, base.range = base.range, 
                                        op = ">", 20)
  } else if (metric == "t10p") {
    result <- percent.days.op.threshold(temp = data, dates = dates, jdays = jdays, 
                                        date.factor = date.factor, threshold.outside.base = threshold, 
                                        base.thresholds = threshold, base.range = base.range, 
                                        op = "<", 20)
  }
  return(result)
}


