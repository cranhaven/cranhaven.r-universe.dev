#'Heat and cold waves duration for n-dimensional arrays
#'
#'This function computes the duration of a heat/cold wave as the number of 
#'consecutive days for which the maximum/minimum temperature is exceeding/below 
#'a threshold over a minimum number of days in month or seasonal resolution.
#'
#'@param data A numeric n-dimensional array containing daily maximum or minimum 
#'  temperature 
#'@param threshold An array with the threshold to be/not to be reach, usually 
#'  given by the 90th/10th percentiles for heat/cold waves computed with the 
#'  \code{Threshold} function.
#'@param op A character ">" (by default) or ">=" for heat waves and "<" or "<=" 
#'  for cold waves indicating the operator must be used  to compare data to 
#'  threshold.
#'@param spell.length A number indicating the number of consecutive days with 
#'  extreme temperature to be considered heat or cold wave.
#'@param by.seasons If TRUE (by default), the wave duration is computed for each 
#'  season (DJF/MAM/JJA/SON) separately. If FALSE is specified, the monthly wave 
#'  duration is computed.
#'@param dates A vector of dates including calendar attributes. If NULL (by 
#'  default), the 'time' attributes of parameter 'data' is used.
#'@param calendar A character indicating the calendar type.
#'@param ncores The number of cores to be used when computing the wave duration.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{\code{$result}, an array with the same dimensions as the input 
#'        \code{data}, but with the time dimension reduce from daily to monthly 
#'        or seasonal resolution depending on the selected resolution in 
#'        \code{by.season}.}
#'  \item{\code{$years}, a vector of the years and season/months corresponding 
#'        to the resolution selected in \code{by.season} and temporal length of 
#'        the input \code{data}.}
#'}
#'
#'@import multiApply   
#'@import PCICt
#'@examples
#'##Example synthetic data:
#'data <- 1:(2 * 3 * 31 * 5)
#'dim(data) <- c(lon = 2, lat = 3, time = 31, model = 5)
#'time <- as.POSIXct(paste(paste(1900, 1, 1:31, sep = "-"), paste(12, 0, 0.0, 
#'                   sep = ":")), tz = "CET")
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time', 
#'                 calendar = 'standard', 
#'                 units = 'days since 1970-01-01 00:00:00', prec = 'double', 
#'                 dim = list(list(name ='time', unlim = FALSE))))
#'attr(time, "variables") <- metadata                 
#'attr(data, 'Variables')$dat1$time <- time
#'threshold <- rep(40, 31)
#'
#'a <- WaveDuration(data, threshold, op = ">", spell.length = 6, 
#'                  by.seasons = TRUE, ncores = NULL)
#'str(a)
#'@export
WaveDuration <- function(data, threshold, op = ">", spell.length = 6, 
                         by.seasons = TRUE, dates = NULL, 
                         calendar = NULL, ncores = NULL) {
  if (is.null(data) | is.null(threshold)) {
    stop("Parameter 'data' and 'threshold' cannot be NULL.")
  }
  if (!is.numeric(data) | !is.numeric(threshold) | !is.numeric(spell.length)) {
    stop("Parameter 'data', 'threshold' and 'spell.length' must be a numeric.")
  }
  if (is.null(dim(data))) {
    dim(data) <- c(time = length(data))
  }
  time_dim <- which(names(dim(data)) == "time")
  if (is.null(dim(threshold))) {
    if (length(threshold) > 1) {
      dim(threshold) <- c(jdays = length(threshold))
    } else {
      threshold = rep(threshold, dim(data)[time_dim])
      dim(threshold) <- c(jdays = length(threshold))
    }
  }
  if (length(time_dim) == 0) {
    if (length(dim(data)) == 1 && is.null(names(dim(data)))) {
      names(dim(data)) <- 'time'
    }
  } else if (length(time_dim) > 1) {
    stop("Parameter 'data' must have only one 'time' dimension.")
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
        if (length(pos) > 0) {
          jdays[pos] <- jdays[pos] + 1
        }
      }
    }
  }

  if (!is.character(op) | (op != ">" & op != ">=" & op != "<" & op != "<=")) {
    stop("Parameter 'op' must be a character indicating the operator to use to compare data to threshold.")
  }
  if (length(spell.length) > 1) {
    spell.length = spell.length[1]
    warning("Parameter 'spell.length' has length > 1 and only the first element will be used.")
  }
  if (!is.logical(by.seasons)) {
    stop("Parameter 'by.seasons' must be logical.")
  }
  if (length(by.seasons) > 1) {
    by.seasons = by.seasons[1]
    warning("Parameter 'by.seasons' has length > 1 and only the first element will be used.")
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
  if (!is.null(names(dim(threshold)))) {
    time_dim_threshold <- which(names(dim(threshold)) == "jdays" | names(dim(threshold)) == "time") 
  } else {
    stop("Parameter 'threshold' must have a dimension called 'jdays'.")
  }
  
  if (dim(threshold)[time_dim_threshold] != length(unique(jdays))) {
    stop("Parameter 'threshold' has different temporal dimension length than the 'jdays' identified in parameter 'dates'.") 
  } else {
    if (length(unique(jdays)) < 360 ) {
      jdays <- jdays - min(jdays) + 1
    }
  }
  if (by.seasons == FALSE) {
    date.factor <- as.factor(substr(dates, 1, 7))
  } else {
    date.factor <- as.factor(substr(dates, 1, 8))
    december <- grep("-12-", date.factor)
    if (length(december) > 0) {
      new.level <- unique(paste0(as.numeric(substr(date.factor[december], 1, 4)) + 1, "-12-"))
      date.factor <- factor(date.factor, levels = unique(c(levels(date.factor), new.level)))
      date.factor[december] = new.level
    }
    date.factor <- gsub("-12-", "-DJF", date.factor)
    date.factor <- gsub("-01-", "-DJF", date.factor)
    date.factor <- gsub("-02-", "-DJF", date.factor)
    date.factor <- gsub("-03-", "-MAM", date.factor)
    date.factor <- gsub("-04-", "-MAM", date.factor)
    date.factor <- gsub("-05-", "-MAM", date.factor)
    date.factor <- gsub("-06-", "-JJA", date.factor)
    date.factor <- gsub("-07-", "-JJA", date.factor)
    date.factor <- gsub("-08-", "-JJA", date.factor)
    date.factor <- gsub("-09-", "-SON", date.factor)
    date.factor <- gsub("-10-", "-SON", date.factor)
    date.factor <- gsub("-11-", "-SON", date.factor)
    date.factor <- as.factor(date.factor)
  }
  years <- levels(date.factor)
  margins <- list(c(1 : length(dim(data)))[-c(time_dim)], 
                  c(1 : length(dim(threshold)))[-c(time_dim_threshold)])
  data <- list(data , threshold )
  if (length(dim_names) > 1) {
    result <- Apply(data = data, margins = margins, 
                    fun = .WaveDuration, date.factor = date.factor, jdays = jdays,
                    op = op, spell.length = spell.length, ncores = ncores)
  } else {
    result <- list()
    result$output1 <- .WaveDuration(data = data[[1]], threshold = data[[2]], 
                                    date.factor = date.factor, 
                                    jdays = jdays, op = op, spell.length = spell.length)
  }
  if (length(years) > 1) {
    if (length(dim_names) > 1) {
      names(dim(result$output1)) <- c("year", dim_names[-time_dim])
    } else {
      dim(result$output1) <- c(year = length(result$output1))
    }
  } else {
    if (length(dim_names) > 1){
    names(dim(result$output1)) <- dim_names[-time_dim]
    }
    else {
      dim(result$output1) <- c(year = 1)
    }
  }
  return(list(result = result$output1, years = years))
}
.WaveDuration <- function(data, threshold, date.factor = date.factor,
                          jdays = jdays,  op = op, spell.length = spell.length) {
  result <- threshold.exceedance.duration.index(daily.temp = data, 
                                                date.factor = date.factor, 
                                                jdays = jdays,
                                                thresholds = threshold,
                                                op = op, min.length = spell.length, 
                                                spells.can.span.years = TRUE, 1)
  return(result)
}

