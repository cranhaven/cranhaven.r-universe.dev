#'Sample Of Observational And Experimental Data For Forecast Verification As Area Averages
#'
#'This data set provides area averaged data for the variable 'tos', i.e. sea 
#'surface temperature, over the mediterranean zone from the example datasets 
#'attached to the package. See examples on Load() for more details.\cr\cr
#'The data is provided through a variable named 'sampleTimeSeries' and is 
#'structured as expected from the 'Load()' function in the 's2dv' 
#'package if was called as follows:\cr\cr
#'  \preformatted{
#'data_path <- system.file('sample_data', package = 's2dv')
#'exp <- list(
#'         name = 'experiment',
#'         path = file.path(data_path, 'model/$EXP_NAME$/monthly_mean',
#'                          '$VAR_NAME$_3hourly/$VAR_NAME$_$START_DATES$.nc')
#'       )
#'obs <- list(
#'         name = 'observation',
#'         path = file.path(data_path, 'observation/$OBS_NAME$/monthly_mean',
#'                          '$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$.nc')
#'       )
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(exp), list(obs), startDates,
#'                   output = 'areave', latmin = 27, latmax = 48, lonmin = -12,
#'                   lonmax = 40)
#'  }
#'Check the documentation on 'Load()' in the package 's2dv' for more information.
#'
#'@usage data(sampleTimeSeries)
#'@format The data set provides with a variable named 'sampleTimeSeries'.\cr\cr
#'
#'sampleTimeSeries$mod is an array that contains the experimental data and the dimension meanings and values are:\cr
#'  c(# of experimental datasets, # of members, # of starting dates, # of lead-times)\cr
#'  c(1, 3, 5, 60)\cr\cr
#'
#'sampleTimeSeries$obs is an array that contains the observational data and the dimension meanings and values are:\cr
#'  c(# of observational datasets, # of members, # of starting dates, # of lead-times)\cr
#'  c(1, 1, 5, 60)\cr\cr
#'
#'sampleTimeSeries$lat is an array with the 2 latitudes covered by the data that was area averaged to calculate the time series (see examples on Load() for details on why such low resolution).\cr\cr
#'
#'sampleTimeSeries$lon is an array with the 3 longitudes covered by the data that was area averaged to calculate the time series (see examples on Load() for details on why such low resolution).
#'
#' @name sampleTimeSeries
#' @docType data
sampleTimeSeries <- function(){}
