#'Diurnal temperature range of multidimensional arrays
#'
#'@description This function computes the mean diurnal temperature range 
#'(tmax - tmin).
#'
#'@param tmax A numeric multidimensional array containing daily maximum 
#'  temperature.
#'@param tmin A numeric multidimensional array containing daily minimum 
#'  temperature.
#'@param by.seasons If TRUE (by default), the DTR is computed for each season 
#'  (December-January-February, March-April-May, June-July-August and 
#'  September-October-November) seperately. If FALSE is specified, the montly 
#'  mean DTR is computed.
#'@param dates A vector of dates with a calendar attributes. If NULL (by 
#'  default), the 'time' attributes of parameter 'tmax' and 'tmin' are 
#'  considered.
#'@param timedim An integer number indicating the position of the time dimension 
#'  in the parameters \code{tmax} and \code{tmin}. If NULL (by default), the 
#'  dimension called 'time' in parameter \code{tmax} and \code{tmin} is 
#'  considered as time dimension.
#'@param calendar A character indicating the calendar type.
#'@param na.rm A logical indicating whether missing values should be removed. If 
#'  \code{na.rm} is FALSE an NA value in any of the arguments will cause a value 
#'  of NA to be returned, otherwise (TRUE by default) NA values are ignored.
#'@param ncores The number of cores to be used when computing the index.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{\code{$dtr.ref}, an array with the same dimensions as the input 
#'        \code{data}, but with the time dimension reduce from daily to monthly 
#'        or seasonal resolution depending on the selected resolution in 
#'        \code{by.season}.}
#'\item{\code{$season}, a vector of the season or months corresponding to the 
#'      resolution selected in \code{by.season}.}
#'}
#'
#'@details The function returns a reordered array with 'time' dimension in the 
#'  first position in the \code{dtr.ref} label.
#'
#'@import multiApply
#'@examples
#'##Exmaple with synthetic data:
#'tmax <- 1:(2 * 3 * 365 * 1)
#'dim(tmax) <- c(lon = 2, lat = 3, time = 365, model = 1)
#'tmin <- (1:(2 * 3 * 365 * 1))-1
#'dim(tmin) <- c(lon = 2, lat = 3, time = 365, model = 1)
#'time <- seq.Date(as.Date("1900-01-01", format = "%Y-%d-%m"), 
#'                 as.Date("1900-31-12", format = "%Y-%d-%m"), 1)
#'time <- as.POSIXct(time, tz = "CET")
#'metadata <- list(time = list(standard_name = 'time', long_name = 'time', 
#'                             calendar = 'noleap',
#'                             units = 'days since 1970-01-01 00:00:00', 
#'                             prec = 'double', 
#'                             dim = list(list(name ='time', unlim = FALSE))))
#'attr(time, "variables") <- metadata
#'attr(tmax, 'Variables')$dat1$time <- time
#'attr(tmax, 'Variables')$common[[2]]$dim[[3]]$len = length(time)
#'attr(tmax, 'Variables')$common[[2]]$dim[[3]]$vals <- time
#'attr(tmin, 'Variables')$dat1$time <- time
#'attr(tmin, 'Variables')$common[[2]]$dim[[3]]$len = length(time)
#'attr(tmin, 'Variables')$common[[2]]$dim[[3]]$vals <- time
#'
#'a <- DTRRef(tmax, tmin, by.seasons = FALSE, ncores = NULL)
#'str(a)
#'
#'tmax <- 1:(2 * 3 * 365 * 1)
#'dim(tmax) <- c(2, 3, 365)
#'tmin <- (1:(2 * 3 * 365 * 1))-1
#'dim(tmin) <- c(2, 3, 365)
#'
#'a <- DTRRef(tmax, tmin, by.seasons = FALSE, dates = time,  timedim = 3, 
#'            ncores = NULL)
#'str(a)
#'@export
DTRRef <- function(tmax, tmin, by.seasons = TRUE, dates = NULL, timedim = NULL, calendar = NULL,
                   na.rm = TRUE, ncores = NULL) {
  if (is.null(tmax) | is.null(tmin)) {
    stop("Parameters 'tmax' and 'tmin' cannot be NULL.")
  }
  if (!is.numeric(tmax) | !is.numeric(tmin)) {
    stop("Parameters 'tmax' and 'tmin' must be a numeric.")
  }
  if (is.null(dim(tmax)) | is.null(dim(tmax)) | length(dim(tmax)) < 1 | length(dim(tmin)) < 1) {
    stop("Parameters 'tmax' and 'tmin' must have at least two dimensions.")
  }
  if (length(dim(tmax)) != length(dim(tmin))) {
    stop("Parameters 'tmax' and 'tmin' must be the same number of dimensions.")
  }
  if (sum(dim(tmax) != dim(tmin)) != 0) {
    stop("Parameters 'tmax' and 'tmin' must be the same dimensions.")
  }
  if (is.null(timedim) | length(timedim) == 0) {
    if (is.null(names(dim(tmax))) | is.null(names(dim(tmin)))) {
      stop("No time dimension provided in parameter 'timedim' nor as attribute of parameter 'tmax' or 'tmin'.")
    }
    if (sum(names(dim(tmax)) != names(dim(tmin))) != 0) {
      stop("Parameters 'tmax' and 'tmin' must be the same dimension names in the same order.")
    }
    timedim <- which(names(dim(tmax)) == "time")
    if(is.null(timedim)) {
      timedim <- which(names(dim(tmin)) == "time")
    }
    if (length(timedim) == 0) {
      stop("No time dimension provided in parameter 'timedim' nor as dimension names of parameter 'tmax' or 'tmin'.")
    }
  }    
  if (is.null(names(dim(tmax)))) {
    if  (is.null(names(dim(tmin)))) {
      dim_names <- paste0('dim', 1:length(dim(tmax)))
    } else {
      if (sum(names(dim(tmax)) != names(dim(tmin))) != 0) {
        stop("Parameters 'tmax' and 'tmin' have different dimension names.")
      }
      dim_names <- names(dim(tmin))
    }
  } else {
    if (sum(names(dim(tmax)) != names(dim(tmin))) != 0) {
      stop("Parameters 'tmax' and 'tmin' have different dimension names.")
    }
    dim_names <- names(dim(tmax))
  }
  if (is.null(dates)) {
    dates.tmax <- attr(tmax, 'Variables')$common$time
    if (is.null(dates.tmax)) {
      dates.tmax <- attr(tmax, 'Variables')$dat1$time
    }
    dates.tmin <- attr(tmin, 'Variables')$common$time
    if (is.null(dates.tmin)) {
        dates.tmin <- attr(tmin, 'Variables')$dat1$time
    }
    if (length(dates.tmax) != length(dates.tmin)) {
      stop("Time attributes of parameter 'tmax' and 'tmin' must be the same length.")
    }
    if (sum(dates.tmax == dates.tmin) == length(dates.tmax)) {
      dates = dates.tmax
    } else {
     stop("Time attributes of parameters 'tmax' and 'tmin' are different.") 
    }
    }
    if (is.null(dates)) {
      stop("No dates provided in parameter 'dates' nor as attribute of parameter 'tmax', 'tmin' or 'dates'.")
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
      stop("Dates provided in parameter 'dates' or as attribute of parameter 'tmax' and 'tmin' must be of class 'POSIXct' or convertable to 'POSIXct'.")
    }
  }
  stop_error <- FALSE
  if (length(dim(tmax)) == 1 | length(dim(tmin)) == 1) {
    if (length(dates) != length(tmax) | length(dates) != length(tmin)) {
      stop_error <- TRUE
    }
  } else {
    if (length(dates) != dim(tmax)[timedim] | length(dates) != dim(tmin)[timedim]) {
      stop_error <- TRUE
    }
  }
  if (stop_error) {
    stop("Parameter 'dates' must be of the same length as the 'time' dimension of the parameter 'tmax' and 'tmin'.")
  }
  dates <- as.PCICt(dates, cal = calendar)
  if (!is.logical(by.seasons)) {
    stop("Parameter 'by.seasons' must be logical.")
  }
  if (length(by.seasons) > 1) {
    by.seasons = by.seasons[1]
    warning("Parameter 'by.seasons' has length > 1 and only the first element will be used.")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be logical.")
  }
  if (length(na.rm) > 1) {
    na.rm = na.rm[1]
    warning("Parameter 'na.rm' has length > 1 and only the first element will be used.")
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
  dims <- 1 : length(dim(tmax))
  margins <- c(1 : length(dim(tmax)))[-c(timedim)]
  date.factor <- as.factor(substr(dates, 6, 7))
  if (by.seasons == TRUE) {
    levels(date.factor)[which(levels(date.factor) == "06" 
                              | levels(date.factor) == "07"|levels(date.factor) == "08")] <- "JJA"
    levels(date.factor)[which(levels(date.factor) == "09" 
                              | levels(date.factor) == "10"|levels(date.factor) == "11")] <- "SON"
    levels(date.factor)[which(levels(date.factor) == "12" 
                              | levels(date.factor) == "01"|levels(date.factor) == "02")] <- "DJF"
    levels(date.factor)[which(levels(date.factor) == "03" 
                              | levels(date.factor) == "04"|levels(date.factor) == "05")] <- "MAM"
  }
  dtr.ref <- Apply(list(tmax,tmin), margins = list(margins, margins), 
                   fun = .DTRRef, date.factor = date.factor, 
                   na.rm = na.rm, ncores = ncores)
  indices <- levels(date.factor)
  dtr.ref = dtr.ref$output1
  names(dim(dtr.ref)) <- c("time", dim_names[c(-timedim)])
  return(list(dtr.ref = dtr.ref, season = indices))
}
.DTRRef <- function(tmax, tmin, ref, date.factor, na.rm = na.rm) {
  result <- tapply(tmax - tmin, INDEX = date.factor, FUN = mean, na.rm = na.rm)
  return(result)
}
