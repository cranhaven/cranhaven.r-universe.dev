#'Selects a season from daily data for multidimensional arrays
#'
#'@description This function selects the daily data corresponding to the 
#'specified season.
#'
#'@param data A numeric multidimensional array containing daily data.
#'@param season A charcater string indicating the season by the three months 
#'  initials in capitals: 'DJF' for winter (summer), 'MAM' spring (autumn), 
#'  'JJA' for summer (winter) or 'SON' for autumn (spring) in the northern 
#'  (southern) hemisphere.
#'@param dates A vector of dates with a calendar attributes. If NULL (by 
#'  default), the 'time' attributes of parameter 'data' are considered.
#'@param timedim An integer number indicating the position of the time dimension 
#'  in the parameter \code{data}. If NULL (by default), the dimension called 
#'  'time' in parameter \code{data}.
#'@param calendar A character indicating the calendar type.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{\code{$data}, a vector or array containing the daily values for the 
#'        selected season, with the same dimensions as \code{data} input but the 
#'        'time' dimension reduce to the number of days corresponding to the 
#'        selected season.}
#'  \item{\code{$dates}, a vector of dates reduce to the number of days 
#'        corresponding to the selected season.}
#'}
#'
#'@import PCICt
#'@examples
#'## Example with synthetic data:
#'data <- 1:(2 * 3 * (366 + 365) * 2)
#'dim(data) <- c(lon = 2, lat = 3, time = 366 + 365, model = 2)
#'time <- seq(ISOdate(1903,1,1), ISOdate(1904,12,31), "days")
#'time <- as.POSIXct(time, tz = "CET")
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time', 
#'                             calendar = 'noleap',
#'                             units = 'days since 1970-01-01 00:00:00', 
#'                             prec = 'double', 
#'                             dim = list(list(name ='time', unlim = FALSE))))
#'attr(time, "variables") <- metadata
#'attr(data, 'Variables')$dat1$time <- time
#'attr(data, 'Variables')$dat2$time <- time
#'attr(data, 'Variables')$common[[2]]$dim[[3]]$len = length(time)
#'attr(data, 'Variables')$common[[2]]$dim[[3]]$vals <- time
#'
#'a <- SeasonSelect(data = data, season = 'JJA')
#'str(a)
#'@export
SeasonSelect <- function(data, season, dates = NULL, timedim = NULL, calendar = NULL) {
  if (is.null(data) | is.null(season)) {
    stop("Parameters 'data' and 'season' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  if (is.null(dim(data))) {
    dim(data) = c(time = length(data))
    timedim = 1
  }
  if (!is.character(season) | (season != "DJF" & season != "MAM" & season != "JJA" & season != "SON")) {
    stop("Parameter 'season' must be a character string indicating the season by the three months initials in capitals: 'DJF' for winter (summer), 'MAM' spring (autumn), 'JJA' for summer (winter) or 'SON' for autumn (spring) in the northern (southern) hemisphere.")
  }
  if (is.null(timedim)) {
    timedim <- which(names(dim(data)) == "time")
  }
  if (length(timedim) == 0) {
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
      pos <- ((year / 100) %% 1 == 0) + ((year / 4) %% 1 == 0) + ((year / 400) %% 1 == 0)
      pos <- which(pos == 0 | pos == 2 | pos == 4)
        if (length(pos) > 0) {
        pos <- pos[which(jdays[pos] > 59)] 
        jdays[pos] <- jdays[pos] + 1
    }
  }  
  if (season == "DJF") {
    if (calendar == "360_day" | calendar == "360") {
      days <- c(c(331 : 360), c(1 : 58))
    } else if (calendar == "standard" | calendar == "gregorian" | calendar == "proleptic_gregorian") {
      days <- c(c(336 : 366), c(1 : 60))
    } else {  
      days <- c(c(335 : 365), c(1 : 59))
    } 
  } else if (season == "MAM") {
    if (calendar == "360_days" | calendar == "360") {
      days <- c(59 : 149)
    } else if (calendar == "standard" | calendar == "gregorian" | calendar == "proleptic_gregorian") {
      days <- c(61 : 152)
    } else {
      days <- c(60 : 151)
    }
  } else if (season == "JJA") {
    if (calendar == "360_days" | calendar == "360") {
      days <- c(150: 240)
    } else if (calendar == "standard" | calendar == "gregorian" | calendar == "proleptic_gregorian") {
      days <- c(153 : 244)
    } else {
      days <- c(152 : 243)
    }
  } else {
    if (calendar == "360_days" | calendar == "360") {
      days <- c(241 : 330)
    } else if (calendar == "standard" | calendar == "gregorian" | calendar == "proleptic_gregorian") {
      days <- c(245 : 335)
    } else {
      days <- c(244 : 334)
    }
  }
  index <- which(jdays %in% days)
  data <- Subset(data, along = timedim, indices = index, drop = "non-selected")
  dates <- dates[index]
  dimensions <- dim(data)
  attributes(data) <- NULL
  dim(data) <- dimensions
  list(data = data, dates = as.POSIXct(dates))
}
