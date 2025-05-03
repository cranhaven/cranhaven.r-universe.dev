#'Sample Of Experimental And Observational Climate Data In Function Of Longitudes And Latitudes
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
#' It is recommended to use the data set as follows:
#'\preformatted{
#' require(zeallot)
#' c(exp, obs) %<-% CSTools::lonlat_temp
#'}
#'
#'The `CST_Load` call used to generate the data set in the infrastructure of 
#'the Earth Sciences Department of the Barcelona Supercomputing Center is shown
#'next. Note that `CST_Load` internally calls `s2dv::Load`, which would require 
#'a configuration file (not provided here) expressing the distribution of the 
#''system5c3s' and 'era5' NetCDF files in the file system.
#'\preformatted{
#' library(CSTools)
#' require(zeallot)
#'
#' startDates <- c('20001101', '20011101', '20021101',
#'                 '20031101', '20041101', '20051101')
#'
#' lonlat_temp <- 
#'   CST_Load(
#'     var = 'tas', 
#'     exp = 'system5c3s', 
#'     obs = 'era5', 
#'     nmember = 15,
#'     sdates = startDates,
#'     leadtimemax = 3,
#'     latmin = 27, latmax = 48,
#'     lonmin = -12, lonmax = 40, 
#'     output = 'lonlat',
#'     nprocs = 1
#'   )
#'}
#'
#' @name lonlat_temp
#' @docType data
#' @author Nicolau Manubens \email{nicolau.manubens@bsc.es}
#' @keywords data
NULL

#'Sample Of Experimental Precipitation Data In Function Of Longitudes And Latitudes
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
#'The `CST_Load` call used to generate the data set in the infrastructure of 
#'the Marconi machine at CINECA is shown next, working on files which were 
#'extracted from forecast data available in the MEDSCOPE internal archive.
#'
#'\preformatted{
#' library(CSTools)
#' infile <- list(path = paste0('/esarchive/exp/ecmwf/system5c3s/daily_mean/',
#'                              '$VAR_NAME$_s0-24h/$VAR_NAME$_$START_DATE$.nc'))
#' lonlat_prec <- CST_Load('prlr', exp = list(infile), obs = NULL,
#'                         sdates = c('20101101', '20111101', '20121101'),
#'                         leadtimemin = 121, leadtimemax = 151,
#'                         latmin = 44, latmax = 47,
#'                         lonmin = 6, lonmax = 9,
#'                         nmember = 6,
#'                         storefreq = "daily", sampleperiod = 1,
#'                         output = "lonlat"
#'                        )
#'}
#'
#' @name lonlat_prec
#' @docType data
#' @author Jost von Hardenberg \email{j.vonhardenberg@isac.cnr.it}
#' @author An-Chi Ho \email{an.ho@bsc.es}
#' @keywords data
NULL
