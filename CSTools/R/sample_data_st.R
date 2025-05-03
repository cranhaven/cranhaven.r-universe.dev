#'Sample Of Experimental And Observational Climate Data In Function Of Longitudes And Latitudes with Start
#'
#'This sample data set contains gridded seasonal forecast and corresponding 
#'observational data from the Copernicus Climate Change ECMWF-System 5 forecast
#'system, and from the Copernicus Climate Change ERA-5 reconstruction. 
#'Specifically, for the 'tas' (2-meter temperature) variable, for the 15 first 
#'forecast ensemble members, monthly averaged, for the 3 first forecast time 
#'steps (lead months 1 to 4) of the November start dates of 2000 to 2005, for 
#'the Mediterranean region (27N-48N, 12W-40E). The data was generated on (or 
#'interpolated onto, for the reconstruction) a rectangular regular grid of size
#'360 by 181.
#' 
#'The `CST_Start` call used to generate the data set in the infrastructure of 
#'the Earth Sciences Department of the Barcelona Supercomputing Center is shown
#'next. Note that `CST_Start` internally calls `startR::Start` and then uses 
#'`as.s2dv_cube` that converts the `startR_array` into `s2dv_cube`.
#'\preformatted{
#'  lonlat_temp_st <- NULL
#'  repos_exp <- paste0('/esarchive/exp/ecmwf/system5c3s/monthly_mean/',
#'                      '$var$_f6h/$var$_$sdate$.nc')
#'  sdates <- sapply(2000:2005, function(x) paste0(x, '1101'))
#'  lonmax <- 40
#'  lonmin <- -12
#'  latmax <- 48
#'  latmin <- 27
#'  lonlat_temp_st$exp <- CST_Start(dataset = repos_exp,
#'                                  var = 'tas',
#'                                  member = startR::indices(1:15),
#'                                  sdate = sdates,
#'                                  ftime = startR::indices(1:3),
#'                                  lat = startR::values(list(latmin, latmax)),
#'                                  lat_reorder = startR::Sort(decreasing = TRUE), 
#'                                  lon = startR::values(list(lonmin, lonmax)),
#'                                  lon_reorder = startR::CircularSort(0, 360),
#'                                  synonims = list(lon = c('lon', 'longitude'),
#'                                                  lat = c('lat', 'latitude'),
#'                                                  member = c('member', 'ensemble'),
#'                                                  ftime = c('ftime', 'time')),
#'                                  return_vars = list(lat = NULL, 
#'                                                     lon = NULL, ftime = 'sdate'),
#'                                                     retrieve = TRUE)
#'  
#'  dates <- c(paste0(2000, c(11, 12)), paste0(2001, c('01', 11, 12)),
#'             paste0(2002, c('01', 11, 12)),  paste0(2003, c('01', 11, 12)),
#'             paste0(2004, c('01', 11, 12)),  paste0(2005, c('01', 11, 12)), 200601)
#'  dates <- sapply(dates, function(x) {paste0(x, '01')})
#'  dates <- as.POSIXct(dates, format = '%Y%m%d', 'UTC')
#'  dim(dates) <- c(ftime = 3, sdate = 6)
#' 
#'  dates <- t(dates)
#'  names(dim(dates)) <- c('sdate', 'ftime')
#'
#'  path.obs <- '/esarchive/recon/ecmwf/era5/monthly_mean/$var$_f1h-r1440x721cds/$var$_$date$.nc'
#'  lonlat_temp_st$obs <- CST_Start(dataset = path.obs,
#'                                  var = 'tas',
#'                                  date = unique(format(dates, '%Y%m')),
#'                                  ftime = startR::values(dates), 
#'                                  ftime_across = 'date',
#'                                  ftime_var = 'ftime',
#'                                  merge_across_dims = TRUE,
#'                                  split_multiselected_dims = TRUE,
#'                                  lat = startR::values(list(latmin, latmax)),
#'                                  lat_reorder = startR::Sort(decreasing = TRUE),
#'                                  lon = startR::values(list(lonmin, lonmax)),
#'                                  lon_reorder = startR::CircularSort(0, 360),
#'                                  synonims = list(lon = c('lon', 'longitude'),
#'                                                  lat = c('lat', 'latitude'),
#'                                                  ftime = c('ftime', 'time')),
#'                                  transform = startR::CDORemapper,
#'                                  transform_extra_cells = 2,
#'                                  transform_params = list(grid = 'r360x181',
#'                                                          method = 'conservative'),
#'                                  transform_vars = c('lat', 'lon'),
#'                                  return_vars = list(lon = NULL,
#'                                                     lat = NULL,
#'                                                     ftime = 'date'),
#'                                  retrieve = TRUE)
#' 
#'  library(lubridate)
#'  dates_exp <- lonlat_temp_st$exp$attrs$Dates
#'  lonlat_temp_st$exp$attrs$Dates <- floor_date(ymd_hms(dates_exp), unit = "months")
#'  dim(lonlat_temp_st$exp$attrs$Dates) <- dim(dates_exp)
#'  
#'  dates_obs <- lonlat_temp_st$obs$attrs$Dates
#'  lonlat_temp_st$obs$attrs$Dates <- floor_date(ymd_hms(dates_obs), unit = "months")
#'  dim(lonlat_temp_st$obs$attrs$Dates) <- dim(dates_obs)
#' 
#'}
#' 
#'@name lonlat_temp_st
#'@docType data
#'@author Nicolau Manubens \email{nicolau.manubens@bsc.es}
#'@keywords data
NULL

#'Sample Of Experimental Precipitation Data In Function Of Longitudes And Latitudes with Start
#'
#'This sample data set contains a small cutout of gridded seasonal precipitation
#'forecast data from the Copernicus Climate Change ECMWF-System 5 forecast 
#'system, to be used to demonstrate downscaling. Specifically, for the 'pr' 
#'(precipitation) variable, for the first 6 forecast ensemble members, daily
#'values, for all 31 days in March following the forecast starting dates in 
#'November of years 2010 to 2012, for a small 4x4 pixel cutout in a region in
#'the North-Western Italian Alps (44N-47N, 6E-9E). The data resolution is 1 
#'degree.
#' 
#'The `CST_Start` call used to generate the data set in the infrastructure of 
#'the Marconi machine at CINECA is shown next, working on files which were 
#'extracted from forecast data available in the MEDSCOPE internal archive.
#'
#'\preformatted{
#'  path <- paste0('/esarchive/exp/ecmwf/system5c3s/daily_mean/',
#'                 '$var$_s0-24h/$var$_$sdate$.nc')
#'  sdates = c('20101101', '20111101', '20121101')
#'  latmin <- 44
#'  latmax <- 47
#'  lonmin <- 6
#'  lonmax <- 9
#'
#'  lonlat_prec_st <- CST_Start(dataset = path,
#'                              var = 'prlr',
#'                              member = startR::indices(1:6),
#'                              sdate = sdates,
#'                              ftime = 121:151,
#'                              lat = startR::values(list(latmin, latmax)),
#'                              lat_reorder = startR::Sort(decreasing = TRUE),
#'                              lon = startR::values(list(lonmin, lonmax)),
#'                              lon_reorder = startR::CircularSort(0, 360),
#'                              synonims = list(lon = c('lon', 'longitude'),
#'                                              lat = c('lat', 'latitude'),
#'                                              ftime = c('time', 'ftime'),
#'                                              member = c('member', 'ensemble')),
#'                               return_vars = list(ftime = 'sdate',
#'                                                  lon = NULL, lat = NULL),
#'                               retrieve = TRUE)
#'}
#'
#'@name lonlat_prec_st
#'@docType data
#'@author Jost von Hardenberg \email{j.vonhardenberg@isac.cnr.it}
#'@author An-Chi Ho \email{an.ho@bsc.es}
#'@keywords data
NULL
