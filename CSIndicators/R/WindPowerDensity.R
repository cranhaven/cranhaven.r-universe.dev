#'Wind power density on s2dv_cube objects
#'
#'@author Llorenç Lledó, \email{llledo@bsc.es}
#'@description Wind Power Density computes the wind power that is available for 
#'extraction per square meter of swept area. 
#'@description It is computed as 0.5*ro*wspd^3. As this function is non-linear, 
#'it will give inaccurate results if used with period means.
#'
#'@param wind An 's2dv_cube' object with instantaneous wind speeds expressed in 
#'  m/s obtained from CST_Start or s2dv_cube functions from CSTools pacakge.
#'@param ro A scalar, or alternatively a multidimensional array with the same
#'  dimensions as wind, with the air density expressed in kg/m^3. By default it
#'  takes the value 1.225, the standard density of air at 15ºC and 1013.25 hPa.
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
#'@param ncores An integer indicating the number of cores to use in parallel
#'  computation for temporal subsetting.
#'
#'@return An s2dv_cube object containing Wind Power Density expressed in W/m^2.
#'
#'@examples
#'wind <- NULL
#'wind$data <- array(rweibull(n = 100, shape = 2, scale = 6), 
#'                   c(member = 5, sdate = 3, time = 214, lon = 2, lat = 5))
#'wind$coords <- list(lat =  c(40, 41), lon = 1:5)
#'variable <- list(varName = 'sfcWind', 
#'                 metadata = list(sfcWind = list(level = 'Surface')))
#'wind$attrs <- list(Variable = variable, Datasets = 'synthetic', 
#'                   when = Sys.time(), Dates = '1990-01-01 00:00:00')
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, time = 214)
#'wind$attrs$Dates <- Dates
#'class(wind) <- 's2dv_cube'
#'WPD <- CST_WindPowerDensity(wind, start = list(21, 4), 
#'                            end = list(21, 6))
#'
#'@export
CST_WindPowerDensity <- function(wind, ro = 1.225, start = NULL, end = NULL, 
                                 time_dim = 'time', ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(wind, 's2dv_cube')) {
    stop("Parameter 'wind' must be of the class 's2dv_cube'.")
  }
  # Dates subset
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(wind$attrs$Dates))) {
      warning("Dimensions in 'wind' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    }
  }
  WindPower <- WindPowerDensity(wind = wind$data, ro = ro, 
                                dates = wind$attrs$Dates, start = start, 
                                end = end, time_dim = time_dim, 
                                ncores = ncores)
  wind$data <- WindPower
  wind$dims <- dim(WindPower)
  if ('Variable' %in% names(wind$attrs)) {
    if ('varName' %in% names(wind$attrs$Variable)) {
      wind$attrs$Variable$varName <- 'WindPowerDensity'
    }
  }
  if (!is.null(start) && !is.null(end)) {
    wind$attrs$Dates <- SelectPeriodOnDates(dates = wind$attrs$Dates,
                                            start = start, end = end,
                                            time_dim = time_dim, 
                                            ncores = ncores)
  }	
  return(wind)
}

#'Wind power density on multidimensional array objects
#'
#'@author Llorenç Lledó, \email{llledo@bsc.es}
#'@description Wind Power Density computes the wind power that is available for 
#'extraction per square meter of swept area. 
#'@description It is computed as 0.5*ro*wspd^3. As this function is non-linear, 
#'it will give inaccurate results if used with period means.
#'
#'@param wind A multidimensional array, vector or scalar with instantaneous wind
#'  speeds expressed in m/s.
#'@param ro A scalar, or alternatively a multidimensional array with the same
#'  dimensions as wind, with the air density expressed in kg/m^3. By default it
#'  takes the value 1.225, the standard density of air at 15ºC and 1013.25 hPa.
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
#'@param ncores An integer indicating the number of cores to use in parallel
#'  computation for temporal subsetting.
#'
#'@return An array with the same dimensions as wind, containing Wind Power 
#'Density expressed in W/m^2.
#'
#'@examples
#'wind <- array(rweibull(n = 32100, shape = 2, scale = 6), 
#'              c(member = 5, sdate = 3, time = 214, lon = 2, lat = 5))
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, time = 214)
#'WPD <- WindPowerDensity(wind, dates = Dates, start = list(21, 4), 
#'                        end = list(21, 6))
#'
#'@export
WindPowerDensity <- function(wind, ro = 1.225, dates = NULL, start = NULL, 
                             end = NULL, time_dim = 'time', ncores = NULL) {

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
        wind <- SelectPeriodOnData(data = wind, dates = dates, start = start, 
                                   end = end, time_dim = time_dim, 
                                   ncores = ncores)
      } else {
        warning("Parameter 'wind' must have named dimensions if 'start' and ",
                "'end' are not NULL. All data will be used.")
      }
    }
  }
	return(0.5 * ro * wind^3)
}
