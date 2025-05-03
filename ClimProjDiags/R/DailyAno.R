#'Daily anomalies
#'
#'@description This function computes daily anomalies from a vector containing 
#'the daily time series.
#'
#'@param data A vector of daily data.
#'@param jdays A vector of the corresponding day of the year. This vector must 
#'  be the same length as parameter \code{data}.
#'@param dates If \code{jdays} is not supplied, a vector of dates corresponding 
#'  to the observations in \code{data} with defined calendar attributes.
#'@param calendar A character indicating the calendar type.
#'@param na.rm A logical indicating whether missing values should be removed. If 
#'  \code{na.rm} is FALSE an NA value in any of the arguments will cause a value 
#'  of NA to be returned, otherwise (TRUE by default) NA values are ignored.
#'
#'@return A vector of daily anomalies of the same length as parameter 
#'\code{data}.
#'
#'@examples
#'# Time series in a vector example:
#'data <- 1:10
#'jdays <- c(rep(1, 5), rep(2, 5))
#'daily_anomaly <- DailyAno(data = data, jdays = jdays, na.rm = TRUE)
#'print(daily_anomaly)
#'@export
DailyAno <- function(data, jdays = NULL, dates = NULL, calendar = NULL, na.rm = TRUE) {
  if (is.null(data)) {
    stop("Parameters 'data' cannot be NULL.")
  }
  if (!is.vector(data)) {
    stop("Parameters 'data' and 'jdays' must be a vector.")
  }
  if (is.null(jdays) & is.null(dates)) {
    stop("At least one of the parameters 'jdays' or 'dates' must be supplied.")
  }
  if (is.null(jdays)) {
    if (is.null(calendar)) {
      calendar <- attributes(dates)$calendar
      if (is.null(calendar)) {
        calendar <- attributes(dates)$variables$time$calendar
      }
      if (is.null(calendar)) {
        stop("The attribute 'calendar' must be present in the parameter 'dates' or specified in parameter 'calendar'.")
      }
      # (end identifying)
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
        stop("Dates provided in parameter 'dates' must be of class 'POSIXct' or convertable to 'POSIXct'.")
      }
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
  }
  if (length(data) != length(jdays)) {
    stop("Parameters 'data' and 'jdays' must have the same lenght.")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be logical.")
  }
  if (length(na.rm) > 1) {
    na.rm = na.rm[1]
    warning("Parameter 'na.rm' has length > 1 and only the first element will be used.")
  }
  climatology <- tapply(data, INDEX = jdays, FUN = mean, na.rm = na.rm)
  anomalies <- c()
  for (i in 1 : length(data)) {
    index <-  which(names(climatology) == jdays[i])
    anomalies[i] <- data[i] - climatology[index]
  }
  return(anomalies)
}