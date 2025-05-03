#'Diurnal temperature range indicator (DTR) of multidimensional arrays
#'
#'@description This function computes the diurnal temperature indicator, defined 
#'as the number of days where the diurnal temperature variation exceeds the 
#'vulnerability threshold (defined as the mean(tmax -tmin) + 5 from the 
#'reference period).
#'
#'@param tmax A numeric multidimensional array containing daily maximum 
#'  temperature.
#'@param tmin A numeric multidimensional array containing daily minimum 
#'  temperature. This array must be the same dimensions as \code{tmax} 
#'  parameter.
#'@param ref An output list from the \code{DTRRef} function with the same 
#'  dimensions as parameters \code{tmax} and \code{tmin}, except the time 
#'  dimension, containing the mean diurnal temperature variation for the 
#'  reference period.
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
#'@param ncores The number of cores to be used when computing the index.
#'
#'@return A list of length 3:
#'\itemize{
#'  \item{\code{$dtr.ref}, an array with the same dimensions as the input 
#'        \code{data}, but with the time dimension reduce from daily to monthly 
#'        or seasonal resolution depending on the selected resolution in 
#'        \code{by.season}.}
#'  \item{\code{$year}, a vector of the corresponding years.}
#'  \item{\code{$season}, a vector of the seasons or months corresponding to the 
#'        resolution selected in \code{by.season}.}
#'}
#'
#'@import multiApply
#'@examples
#'##Exmaple with synthetic data:
#'tmax <- 1 : (2 * 3 * 730 * 1)
#'dim(tmax) <- c(lon = 2, lat = 3, time = 730, model = 1)
#'tmin <- (1 : (2 * 3 * 730 * 1)) - 1
#'dim(tmin) <- c(lon = 2, lat = 3, time = 730, model = 1)
#'time <- seq(as.POSIXct("1900-01-01 12:00:00", tz = "", 
#'                       format = "%Y-%d-%m %H:%M:%S"), 
#'            as.POSIXct("1901-31-12 18:00:00", tz = "", 
#'                       format = "%Y-%d-%m %H:%M:%S"), "day")
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
#'a <- DTRRef(tmax, tmin, by.seasons = FALSE, ncores = NULL)
#'
#'aa <- DTRIndicator(tmax, tmin, ref = a, by.seasons = FALSE, ncores = NULL)
#'str(aa)
#'dim(aa$indicator)
#'@export
DTRIndicator <- function(tmax, tmin, ref, by.seasons = TRUE, dates = NULL, timedim = NULL, 
                         calendar = NULL, ncores = NULL) {
  if (is.null(tmax) | is.null(tmin) | is.null(ref)) {
    stop("Parameters 'tmax', 'tmin' and 'ref' cannot be NULL.")
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
  if (!is.list(ref) | length(ref) != 2) {
    stop("Parameter 'ref' should be a list of two elements, output of DTRRef() function.")
  }
  if (sum(names(ref) != c("dtr.ref", "season")) != 0) {
    stop("Names of list elements of parameter 'ref' must be 'dtr.ref' and 'season' as DTRRef() function output.")
  }
  if (names(dim(ref$dtr.ref))[1] != 'time') {
    stop("The first dimension of the element 'dtr.ref' in parameter 'ref' must be called 'time'.")
  }
  if (length(ref$season) != dim(ref$dtr.ref)[1]) {
    stop("Length of element 'season' and time dimension of element 'dtr.ref' in parameter 'ref' must be equal.")
  }
  if (is.null(timedim) | length(timedim) == 0) {
    if (is.null(names(dim(tmax))) | is.null(names(dim(tmin)))) {
      stop("No time dimension provided in parameter 'timedim' nor as attribute of parameter 'tmax' or 'tmin'.")
    }
    if (sum(names(dim(tmax)) != names(dim(tmin))) != 0) {
      stop("Parameters 'tmax' and 'tmin' must be the same dimension names in the same order.")
    }
    timedim <- which(names(dim(tmax)) == "time")
    if (is.null(timedim)) {
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
    dates.max <- attr(tmax, 'Variables')$common$time
    if (is.null(dates.max)) {
      dates.tmax <- attr(tmax, 'Variables')$dat1$time
    }
    dates.min <- attr(tmin, 'Variables')$common$time
    if (is.null(dates.min)) {
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
  if (length(by.seasons) > 1){
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
  time_dim_dtr <- timedim
  time_dim_ref <- which(names(dim(ref$dtr.ref)) == "time")
  dims <- 1 : length(dim(tmax))
  margins_dtr <- c(1 : length(dim(tmax)))[-c(time_dim_dtr)]
  margins_ref <- c(1 : length(dim(ref$dtr.ref)))[-c(time_dim_ref)] 
  if (by.seasons == TRUE) {
    date.factor <- as.factor(substr(dates, 1, 8))
    december <- grep("-12-", date.factor)
    if (length(december) > 0) {
      new.level <- unique(paste0(as.numeric(substr(date.factor[december], 1, 4)) + 1, "-12-"))
      date.factor <- factor(date.factor, levels = unique(c(levels(date.factor), new.level)))
      date.factor[december] <- paste0(as.numeric(substr(date.factor[december], 1, 4)) + 1, "-12-")
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
    season.factor <- levels(as.factor(substr(date.factor, 6, 8)))
    year.factor <- levels(as.factor(substr(date.factor, 1, 4)))
  } else {
    date.factor <- as.factor(substr(dates, 1, 7))
    season.factor <- levels(as.factor(substr(dates, 6, 7)))
    year.factor <- levels(as.factor(substr(dates, 1, 4)))
  }
  dtr <- (tmax - tmin) 
  indicator = Apply(list(dtr, ref$dtr.ref), margins = list(margins_dtr, margins_ref), 
                    fun = .DTRIndicator, date.factor = date.factor, 
                    ref_seasons = ref$season, ncores = ncores)
  names(dim(indicator$output1)) <- c("year", "season", dim_names[c(-time_dim_dtr)])

  names(dim(indicator$output1))[3 : length(dim(indicator$output1))] <- c(names(dim(tmax))[c(-time_dim_dtr)])
  return(list(indicator = indicator$output1, year = year.factor,
              season = season.factor))
}
.DTRIndicator <- function(dtr, ref, date.factor, ref_seasons) {
  result <- c()
  season.factor <- as.factor(substr(date.factor, 6,8))
  year.factor <- as.factor(substr(date.factor, 1,4))
  for (yr in 1 : length(levels(year.factor))) {
    subset_dtr <- dtr[year.factor == levels(year.factor)[yr]]
    dtr.indicator <- c()
    for (seas in 1 : length(levels(season.factor))) {
      subset.factor <- season.factor[year.factor == levels(year.factor)[yr]]
      dtr.indicator[seas] <- sum(subset_dtr[subset.factor == levels(season.factor)[seas]] >  (ref[which(ref_seasons ==levels(season.factor)[seas])] + 5))
    }
    result <- rbind(result, dtr.indicator)
  }
  names(dim(result)) <- c("year", "season")
  return(result)
}
