#'Period Variance on 's2dv_cube' objects
#'
#'Period Variance computes the average (var) of a given variable in a period.
#'Two bioclimatic indicators can be obtained by using this function:
#'\itemize{
#'  \item{'BIO4', (Providing temperature data) Temperature Seasonality 
#'        (Standard Deviation). The amount of temperature variation  
#'        over a given year (or averaged years) based on the standard 
#'        deviation (variation) of monthly temperature averages.}
#'  \item{'BIO15', (Providing precipitation data) Precipitation Seasonality 
#'        (CV). This is a measure of the variation in monthly precipitation 
#'        totals over the course of the year. This index is the ratio of the 
#'        standard deviation of the monthly total precipitation to the mean 
#'        monthly total precipitation (also known as the coefficient of 
#'        variation) and is expressed as a percentage.}
#'}
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. More than one 
#'  dimension name matching the dimensions provided in the object 
#'  \code{data$data} can be specified.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return An 's2dv_cube' object containing the indicator in the element 
#'\code{data} with dimensions of the input parameter 'data' except the 
#'dimension where the var has been computed (specified with 'time_dim'). A new 
#'element called 'time_bounds' will be added into the 'attrs' element in the 
#''s2dv_cube' object. It consists of a list containing two elements, the start 
#'and end dates of the aggregated period with the same dimensions of 'Dates' 
#'element.
#'
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(45), dim = c(member = 7, sdate = 4, time = 3))
#'Dates <- c(seq(as.Date("2000-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2001-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2001-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2002-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2002-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2003-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2003-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2004-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"))
#'dim(Dates) <- c(sdate = 4, time = 3)
#'exp$attrs$Dates <- Dates
#'class(exp) <- 's2dv_cube'
#'
#'res <- CST_PeriodVariance(exp, start = list(01, 12), end = list(01, 01))
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
CST_PeriodVariance <- function(data, start = NULL, end = NULL,
                           time_dim = 'time', na.rm = FALSE,
                           ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }

  # Dates subset
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(data$attrs$Dates))) {
      warning("Dimensions in 'data' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    }
  }

  Dates <- data$attrs$Dates
  total <- PeriodVariance(data = data$data, dates = Dates, start = start, end = end,
                          time_dim = time_dim, na.rm = na.rm, ncores = ncores)
  
  data$data <- total
  data$dims <- dim(total)
  data$coords[[time_dim]] <- 1 : length(data$dims[[time_dim]])

  if (!is.null(Dates)) {
    if (!is.null(start) && !is.null(end)) {
      Dates <- SelectPeriodOnDates(dates = Dates, start = start, end = end,
                                   time_dim = time_dim, ncores = ncores)
    }
    if (is.null(dim(Dates))) {
      warning("Element 'Dates' has NULL dimensions. They will not be ", 
              "subset and 'time_bounds' will be missed.")
      data$attrs$Dates <- Dates
    } else {
      # Create time_bounds
      time_bounds <- NULL
      time_bounds$start <- ClimProjDiags::Subset(x = Dates, along = time_dim, 
                                                 indices = 1, drop = FALSE)
      time_bounds$end <- ClimProjDiags::Subset(x = Dates, along = time_dim, 
                                               indices = dim(Dates)[time_dim], 
                                               drop = FALSE)

      # Add Dates in attrs
      data$attrs$Dates <- time_bounds$start
      data$attrs$time_bounds <- time_bounds
    }
  }
  return(data)
}

#'Period Variance on multidimensional array objects
#'
#'Period Variance computes the average (var) of a given variable in a period.
#'Two bioclimatic indicators can be obtained by using this function:
#'\itemize{
#'  \item{'BIO4', (Providing temperature data) Temperature Seasonality 
#'        (Standard Deviation). The amount of temperature variation  
#'        over a given year (or averaged years) based on the standard 
#'        deviation (variation) of monthly temperature averages.}
#'  \item{'BIO15', (Providing precipitation data) Precipitation Seasonality 
#'        (CV). This is a measure of the variation in monthly precipitation 
#'        totals over the course of the year. This index is the ratio of the 
#'        standard deviation of the monthly total precipitation to the mean 
#'        monthly total precipitation (also known as the coefficient of 
#'        variation) and is expressed as a percentage.}
#'}
#'
#'@param data A multidimensional array with named dimensions.
#'@param dates A multidimensional array of dates with named dimensions matching 
#'  the temporal dimensions on parameter 'data'. By default it is NULL, to  
#'  select aperiod this parameter must be provided.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. More than one 
#'  dimension name matching the dimensions provided in the object 
#'  \code{data$data} can be specified.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the 
#'indicator in the element \code{data}.
#'
#'@examples
#'data <- array(rnorm(45), dim = c(member = 7, sdate = 4, time = 3))
#'Dates <- c(seq(as.Date("2000-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2001-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2001-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2002-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2002-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2003-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"),
#'           seq(as.Date("2003-11-01", "%Y-%m-%d", tz = "UTC"), 
#'               as.Date("2004-01-01", "%Y-%m-%d", tz = "UTC"), by = "month"))
#'dim(Dates) <- c(sdate = 4, time = 3)
#'res <- PeriodVariance(data, dates = Dates, start = list(01, 12), end = list(01, 01))
#'
#'@import multiApply
#'@importFrom stats setNames
#'@export
PeriodVariance <- function(data, dates = NULL, start = NULL, end = NULL,
                           time_dim = 'time', na.rm = FALSE, ncores = NULL) {
  # Initial checks
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!is.array(data)) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }

  if (!is.null(start) && !is.null(end)) {
    if (is.null(dates)) {
      warning("Parameter 'dates' is NULL and the average of the ",
              "full data provided in 'data' is computed.")
    } else {
      if (!any(c(is.list(start), is.list(end)))) {
      stop("Parameter 'start' and 'end' must be lists indicating the ",
            "day and the month of the period start and end.")
      }
      if (!is.null(dim(dates))) {
        data <- SelectPeriodOnData(data = data, dates = dates, start = start, 
                                   end = end, time_dim = time_dim, 
                                   ncores = ncores)
      } else {
        warning("Parameter 'dates' must have named dimensions if 'start' and ",
                "'end' are not NULL. All data will be used.")
      }
    }
  }
  total <- Apply(list(data), target_dims = time_dim, 
                 fun = .periodvariance,
                 na.rm = na.rm, ncores = ncores)$output1
  dim(total) <- c(dim(total), setNames(1, time_dim))
  return(total)
}

.periodvariance <- function(data, na.rm) {
  var <- sum((data - mean(data, na.rm = na.rm))^2) / (length(data)-1)
  return(var)
}


