#'@rdname CST_Analogs
#'@title Downscaling using Analogs based on large scale fields.
#' 
#'@author M. Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mariadm.chaves@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Nuria Perez-Zanon \email{nuria.perez@bsc.es}
#'
#'@description This function perform a downscaling using Analogs. To compute 
#'the analogs, the function search for days with similar large scale conditions
#'to downscaled fields to a local scale. The large scale and the local scale 
#'regions are defined by the user. The large scale is usually given by 
#'atmospheric circulation as sea level pressure or geopotential height 
#'(Yiou et al, 2013) but the function gives the possibility to use another 
#'field. The local scale will be usually given by precipitation or temperature 
#'fields, but might be another variable.The analogs function will find the best
#'analogs based in Minimum Euclidean distance in the large scale pattern
#'(i.e.SLP).
#'
#'The search of analogs must be done in the longest dataset posible. This is 
#'important since it is necessary to have a good representation of the 
#'possible states of the field in the past, and therefore, to get better 
#'analogs. 
#'This function has not constrains of specific regions, variables to downscale,
#'or data to be used (seasonal forecast data, climate projections data, 
#'reanalyses data). The regrid into a finner scale is done interpolating with 
#'CST_Start. Then, this interpolation is corrected selecting the analogs in the 
#'large and local scale in based of the observations. The function is an 
#'adapted version of the method of Yiou et al 2013. For an advanced search of 
#'Analogs (multiple Analogs, different criterias, further information from the
#'metrics and date of the selected Analogs) use the'Analog'
#'function within 'CSTools' package.    
#'
#'@references Yiou, P., T. Salameh, P. Drobinski, L. Menut, R. Vautard,
#' and M. Vrac, 2013 : Ensemble reconstruction of the atmospheric column 
#' from surface pressure using analogues.  Clim. Dyn., 41, 1419-1437. 
#' \email{pascal.yiou@lsce.ipsl.fr}
#'
#'@param expL An 's2dv_cube' object containing the experimental field on the 
#'  large scale for which the analog is aimed. This field is used to in all the
#'  criterias. If parameter 'expVar' is not provided, the function will return 
#'  the expL analog. The element 'data' in the 's2dv_cube' object must have, at
#'  least, latitudinal and longitudinal dimensions. The object is expect to be 
#'  already subset for the desired large scale region. Latitudinal dimension 
#'  accepted names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. Longitudinal 
#'  dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 'nav_lon'.
#'@param obsL An 's2dv_cube' object containing the observational field on the
#'  large scale. The element 'data' in the 's2dv_cube' object must have the same 
#'  latitudinal and longitudinal dimensions as parameter 'expL' and a temporal 
#'  dimension with the maximum number of available observations.
#'@param expVar An 's2dv_cube' object containing the experimental field on the
#'  local scale, usually a different variable to the parameter 'expL'. If it is 
#'  not NULL (by default, NULL), the returned field by this function will be the 
#'  analog of parameter 'expVar'.
#'@param obsVar An 's2dv_cube' containing the field of the same variable as the 
#'  passed in parameter 'expVar' for the same region.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param region A vector of length four indicating the minimum longitude, the 
#'  maximum longitude, the minimum latitude and the maximum latitude.
#'@param criteria A character string indicating the criteria to be used for the
#'  selection of analogs:
#'  \itemize{
#'  \item{Large_dist} minimum Euclidean distance in the large scale pattern; 
#'  \item{Local_dist} minimum Euclidean distance in the large scale pattern 
#'  and minimum Euclidean distance in the local scale pattern; and
#'  \item{Local_cor} minimum Euclidean distance in the large scale pattern, 
#'  minimum Euclidean distance in the local scale pattern and highest 
#'  correlation in the local variable to downscale.}
#'  Criteria 'Large_dist' is recommended for CST_Analogs, for an advanced use of 
#'  the criterias 'Local_dist' and 'Local_cor' use 'Analogs' function.
#'@param excludeTime An array of N named dimensions (coinciding with time 
#'  dimensions in expL)of character string(s) indicating the date(s) of the 
#'  observations in the format "dd/mm/yyyy" to be excluded during the search of 
#'  analogs. It can be NULL but if expL is not a forecast (time_expL contained in
#'  time_obsL), by default time_expL will be removed during the search of analogs.
#'@param time_expL A character string indicating the date of the experiment 
#'  in the same format than time_obsL (i.e. "yyyy-mm-dd"). By default it is NULL 
#'  and dates are taken from element \code{$attrs$Dates} from expL.
#'@param time_obsL A character string indicating the date of the observations 
#'  in the date format (i.e. "yyyy-mm-dd"). By default it is NULL and dates are 
#'  taken from element \code{$attrs$Dates} from obsL. It must have time 
#'  dimensions.
#'@param region A vector of length four indicating the minimum longitude, 
#'  the maximum longitude, the minimum latitude and the maximum latitude.
#'@param nAnalogs Number of Analogs to be selected to apply the criterias 
#'  'Local_dist' or 'Local_cor'. This is not the necessary the number of analogs 
#'  that the user can get, but the number of events with minimum distance in 
#'  which perform the search of the best Analog. The default value for the 
#'  'Large_dist' criteria is 1, for 'Local_dist' and 'Local_cor' criterias must
#'  be greater than 1 in order to match with the first criteria, if nAnalogs is
#'  NULL for 'Local_dist' and 'Local_cor' the default value will be set at the 
#'  length of 'time_obsL'. If AnalogsInfo is FALSE the function returns just 
#'  the best analog.
#'@param AnalogsInfo A logical value. TRUE to get a list with two elements: 
#'  1) the downscaled field and 2) the AnalogsInfo which contains: 
#'  a) the number of the best analogs, b) the corresponding value of the metric 
#'  used in the selected criteria (distance values for Large_dist and Local_dist, 
#'  correlation values for Local_cor), c)dates of the analogs). The analogs are 
#'  listed in decreasing order, the first one is the best analog (i.e if the 
#'  selected criteria is Local_cor the best analog will be the one with highest 
#'  correlation, while for Large_dist criteria the best analog will be the day 
#'  with minimum Euclidean distance). Set to FALSE to get a single analog, the 
#'  best analog, for instance for downscaling. 
#'@param ncores The number of cores to use in parallel computation
#'
#'@seealso \code{\link{CST_Start}}, \code{\link[startR]{Start}}
#'
#'@return An 's2dv_cube' object containing an array with the dowscaled values of 
#'the best analogs in element 'data'. If 'AnalogsInfo' is TRUE, 'data' is a list 
#'with an array of the downscaled fields and the analogs information in 
#'elements 'analogs', 'metric' and 'dates'.
#'@examples
#'expL <- rnorm(1:200)
#'dim(expL) <- c(member = 10, lat = 4, lon = 5)
#'obsL <- c(rnorm(1:180), expL[1, , ]*1.2)
#'dim(obsL) <- c(time = 10, lat = 4, lon = 5)
#'time_obsL <- as.POSIXct(paste(rep("01", 10), rep("01", 10), 1994:2003, sep = "-"), 
#'                        format = "%d-%m-%y")
#'dim(time_obsL) <- c(time = 10)
#'time_expL <- time_obsL[1]
#'dim(time_expL) <- c(time = 1)
#'lon <-  seq(-1, 5, 1.5)
#'lat <- seq(30, 35, 1.5)
#'coords <- list(lon = seq(-1, 5, 1.5), lat = seq(30, 35, 1.5))
#'attrs_expL <- list(Dates = time_expL)
#'attrs_obsL <- list(Dates = time_obsL)
#'expL <- list(data = expL, coords = coords, attrs = attrs_expL)
#'obsL <- list(data = obsL, coords = coords, attrs = attrs_obsL)
#'class(expL) <- 's2dv_cube'
#'class(obsL) <- 's2dv_cube'
#'region <- c(min(lon), max(lon), min(lat), max(lat))
#'downscaled_field <- CST_Analogs(expL = expL, obsL = obsL, region = region)
#' 
#'@import multiApply
#'@import abind
#'@import s2dv
#'@importFrom ClimProjDiags SelBox Subset
#'@export
CST_Analogs <- function(expL, obsL, expVar = NULL, obsVar = NULL, 
                        sdate_dim = 'sdate', region = NULL,
                        criteria = 'Large_dist', excludeTime = NULL,
                        time_expL = NULL, time_obsL = NULL,
                        nAnalogs = NULL, AnalogsInfo = FALSE,
                        ncores = NULL) {

  # Check 's2dv_cube'
  if (!inherits(expL, "s2dv_cube") || !inherits(obsL, "s2dv_cube")) {
    stop("Parameter 'expL' and 'obsL' must be of the class 's2dv_cube'.")
  }
  if (!is.null(expVar) && !inherits(expVar, "s2dv_cube")) {
    stop("Parameter 'expVar' must be of the class 's2dv_cube'.")
  }        
  if (!is.null(obsVar) && !inherits(obsVar, "s2dv_cube")) {
    stop("Parameter 'obsVar' must be of the class 's2dv_cube'.")
  }

  # Check 'obsL' object structure
  if (!all(c('data', 'coords', 'attrs') %in% names(obsL))) {
    stop("Parameter 'obsL' must have 'data', 'coords' and 'attrs' elements ",
         "within the 's2dv_cube' structure.")
  }
  
  if (!any(names(obsL$coords) %in% .KnownLonNames()) | 
      !any(names(obsL$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names of parameter 'obsL' do not match any ",
         "of the names accepted by the package.")
  }

  lon_name <- names(obsL$coords)[[which(names(obsL$coords) %in% .KnownLonNames())]]
  lat_name <- names(obsL$coords)[[which(names(obsL$coords) %in% .KnownLatNames())]]

  # Check 'obsVar' object structure
  if (!is.null(obsVar)) {
    if (!all(c('data', 'coords', 'attrs') %in% names(obsVar))) {
      stop("Parameter 'obsVar' must have 'data', 'coords' and 'attrs' elements ",
           "within the 's2dv_cube' structure.")
    }
    if (!any(names(obsVar$coords) %in% .KnownLonNames()) | 
        !any(names(obsVar$coords) %in% .KnownLatNames())) {
      stop("Spatial coordinate names of parameter 'obsVar' do not match any ",
           "of the names accepted by the package.")
    }
    lonVar <- obsVar$coords[[which(names(obsVar$coords) %in% .KnownLonNames())]]
    latVar <- obsVar$coords[[which(names(obsVar$coords) %in% .KnownLatNames())]]
  } else {
    lonVar <- NULL
    latVar <- NULL
  }

  # Check temporal dimensions
  if (any(names(dim(obsL$data)) %in% 'sdate')) {
    if (any(names(dim(obsL$data)) %in% 'ftime')) {
      obsL <- CST_MergeDims(obsL, c('ftime', 'sdate'), rename_dim = 'time')
    } else if (any(names(dim(obsL$data)) %in% 'time')) {
      obsL <- CST_MergeDims(obsL, c('time', 'sdate'), rename_dim = 'time')
    }
  }
  if (!is.null(obsVar)) {
    if (any(names(dim(obsVar$data)) %in% 'sdate')) {
      if (any(names(dim(obsVar$data)) %in% 'ftime')) {
        obsVar <- CST_MergeDims(obsVar, c('ftime', 'sdate'),rename_dim = 'time')
      } else if (any(names(dim(obsVar$data)) %in% 'time')) {
        obsVar <- CST_MergeDims(obsVar, c('time', 'sdate'), rename_dim = 'time')
      }
    }
  } 
  if (is.null(time_expL)) {
    time_expL <- expL$attrs$Dates
  }
  if (is.null(time_obsL)) {
    time_obsL <- obsL$attrs$Dates
  }

  res <- Analogs(expL$data, obsL$data, time_obsL = time_obsL, 
                 time_expL = time_expL, 
                 lonL = as.vector(obsL$coords[[lon_name]]),
                 latL = as.vector(obsL$coords[[lat_name]]), 
                 expVar = expVar$data,
                 obsVar = obsVar$data, sdate_dim = sdate_dim,
                 criteria = criteria, 
                 excludeTime = excludeTime, region = region,
                 lonVar = as.vector(lonVar), latVar = as.vector(latVar),
                 nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo,
                 ncores = ncores)
                 
  if (AnalogsInfo) {
    if (is.numeric(res$dates)) {
      res$dates <- as.POSIXct(res$dates, origin = '1970-01-01', tz = 'UTC')
    }
  }

  expL$data <- res
  expL$dims <- dim(res)

  if (!is.null(obsL$coords[[lon_name]]) | !is.null(obsL$coords[[lat_name]])) {
    if (is.null(region)) {
      expL$coords[[lon_name]] <- obsL$coords[[lon_name]]
      expL$coords[[lat_name]] <- obsL$coords[[lat_name]]
    } else {
      expL$coords[[lon_name]] <- SelBox(obsL$data, 
                                        lon = as.vector(obsL$coords[[lon_name]]), 
                                        lat = as.vector(obsL$coords[[lat_name]]),
                                        region = region, 
                                        londim = lon_name, 
                                        latdim = lat_name)$lon
      expL$coords[[lat_name]] <- SelBox(obsL$data, 
                                        lon = as.vector(obsL$coords[[lon_name]]), 
                                        lat = as.vector(obsL$coords[[lat_name]]),
                                        region = region,
                                        londim = lon_name, 
                                        latdim = lat_name)$lat
    }
  }
 
  return(expL)
}

#'@rdname Analogs
#'@title Analogs based on large scale fields.
#' 
#'@author M. Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mariadm.chaves@cmcc.it }
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Nuria Perez-Zanon \email{nuria.perez@bsc.es}
#'        
#'@description This function perform a downscaling using Analogs. To compute 
#'the analogs, the function search for days with similar large scale conditions
#'to downscaled fields in the local scale. The large scale and the local scale 
#'regions are defined by the user. The large scale is usually given by 
#'atmospheric circulation as sea level pressure or geopotential height (Yiou 
#'et al, 2013) but the function gives the possibility to use another field. The
#'local scale will be usually given by precipitation or temperature fields, but
#'might be another variable. 
#'The analogs function will find the best analogs based in three criterias:
#' (1) Minimum Euclidean distance in the large scale pattern (i.e. SLP)
#' (2) Minimum Euclidean distance in the large scale pattern (i.e. SLP) 
#' and minimum Euclidean distance in the local scale pattern (i.e. SLP). 
#' (3) Minimum Euclidean distance in the large scale pattern (i.e. SLP), 
#' minimum distance in the local scale pattern (i.e. SLP) and highest 
#' correlation in the local variable to downscale (i.e Precipitation).
#'The search of analogs must be done in the longest dataset posible. This is 
#'important since it is necessary to have a good representation of the 
#'possible states of the field in the past, and therefore, to get better 
#'analogs. Once the search of the analogs is complete, and in order to used the 
#'three criterias the user can select a number of analogs , using parameter 
#''nAnalogs' to restrict 
#'the selection of the best analogs in a short number of posibilities, the best
#'ones. This function has not constrains of specific regions, variables to 
#'downscale, or data to be used (seasonal forecast data, climate projections 
#'data, reanalyses data). The regrid into a finner scale is done interpolating 
#'with CST_Start. Then, this interpolation is corrected selecting the analogs in
#'the large and local scale in based of the observations. The function is an 
#'adapted version of the method of Yiou et al 2013.       
#'
#'@references Yiou, P., T. Salameh, P. Drobinski, L. Menut, R. Vautard,
#'and M. Vrac, 2013 : Ensemble reconstruction of the atmospheric column 
#'from surface pressure using analogues.  Clim. Dyn., 41, 1419-1437. 
#'\email{pascal.yiou@lsce.ipsl.fr}
#' 
#'@param expL An array of N named dimensions containing the experimental field
#'  on the large scale for which the analog is aimed. This field is used to in 
#'  all the criterias. If parameter 'expVar' is not provided, the function will
#'  return the expL analog. The element 'data' in the 's2dv_cube' object must 
#'  have, at least, latitudinal and longitudinal dimensions. The object is 
#'  expect to be already subset for the desired large scale region. Latitudinal 
#'  dimension accepted names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. 
#'  Longitudinal dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 
#'  'nav_lon'.
#'@param obsL An array of N named dimensions containing the observational field 
#'  on the large scale. The element 'data' in the 's2dv_cube' object must have 
#'  the same latitudinal and longitudinal dimensions as parameter 'expL' and a 
#'  single temporal dimension with the maximum number of available observations.
#'@param time_obsL A character string indicating the date of the observations 
#'  in the format "dd-mm-yyyy". Reference time to search for analogs. It must 
#'  have time dimensions.
#'@param time_expL An array of N named dimensions (coinciding with time 
#'  dimensions in expL) of character string(s) indicating the date(s) of the 
#'  experiment in the format "dd-mm-yyyy". Time(s) to find the analogs. If it 
#'  is not an scalar it must have named dimensions.
#'@param lonL A vector containing the longitude of parameter 'expL'.
#'@param latL A vector containing the latitude of parameter 'expL'.
#'@param excludeTime An array of N named dimensions (coinciding with time 
#'  dimensions in expL) of character string(s) indicating the date(s) of the 
#'  observations in the format "dd/mm/yyyy" to be excluded during the search of 
#'  analogs. It can be NULL but if expL is not a forecast (time_expL contained 
#'  in time_obsL), by default time_expL will be removed during the search of 
#'  analogs.
#'@param expVar An array of N named dimensions containing the experimental 
#'  field on the local scale, usually a different variable to the parameter 
#'  'expL'. If it is not NULL (by default, NULL), the returned field by this 
#'  function will be the analog of parameter 'expVar'.
#'@param obsVar An array of N named dimensions containing the field of the 
#'  same variable as the passed in parameter 'expVar' for the same region.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param AnalogsInfo A logical value. If it is TRUE it returns a list
#'  with two elements: 1) the downscaled field and
#'  2) the AnalogsInfo which contains: a) the number of the best 
#'  analogs, b) the corresponding value of the metric used in the selected 
#'  criteria (distance values for Large_dist and Local_dist,correlation values 
#'  for Local_cor), c)dates of the analogs). The analogs are listed in 
#'  decreasing order, the first one is the best analog (i.e if the selected 
#'  criteria is Local_cor the best analog will be the one with highest 
#'  correlation, while for Large_dist criteria the best analog will be the day 
#'  with minimum Euclidean distance). Set to FALSE to get a single analog, the 
#'  best analog, for instance for downscaling. 
#'@param criteria A character string indicating the criteria to be used for the
#' selection of analogs:
#'  \itemize{\item{Large_dist} minimum Euclidean distance in the large scale pattern; 
#'           \item{Local_dist} minimum Euclidean distance in the large scale pattern 
#'            and minimum Euclidean distance in the local scale pattern; and
#'           \item{Local_cor} minimum Euclidean distance in the large scale pattern, 
#'            minimum Euclidean distance in the local scale pattern and highest 
#'            correlation in the local variable to downscale.} 
#'@param lonVar A vector containing the longitude of parameter 'expVar'.
#'@param latVar A vector containing the latitude of parameter 'expVar'.
#'@param region A vector of length four indicating the minimum longitude, 
#'  the maximum longitude, the minimum latitude and the maximum latitude.
#'@param nAnalogs Number of Analogs to be selected to apply the criterias 
#'  'Local_dist' or 'Local_cor'. This is not the necessary the number of analogs 
#'  that the user can get, but the number of events with minimum distance in 
#'  which perform the search of the best Analog. The default value for the 
#'  'Large_dist' criteria is 1, for 'Local_dist' and 'Local_cor' criterias must
#'  be greater than 1 in order to match with the first criteria, if nAnalogs is
#'  NULL for 'Local_dist' and 'Local_cor' the default value will be set at the 
#'  length of 'time_obsL'. If AnalogsInfo is FALSE the function returns just 
#'  the best analog.
#'@param ncores The number of cores to use in parallel computation.
#'
#'@return An array with the dowscaled values of the best analogs for the criteria 
#'selected. If 'AnalogsInfo' is set to TRUE it returns a list with an array 
#'of the dowsncaled field and the analogs information in elements 'analogs', 
#''metric' and 'dates'.
#'
#'@examples
#'# Example 1: Downscaling using criteria 'Large_dist' and a single variable:
#'expSLP <- rnorm(1:20)
#'dim(expSLP) <- c(lat = 4, lon = 5)
#'obsSLP <- c(rnorm(1:180), expSLP * 1.2)
#'dim(obsSLP) <- c(time = 10, lat = 4, lon = 5)
#'time_obsSLP <- paste(rep("01", 10), rep("01", 10), 1994 : 2003, sep = "-")
#'dim(time_obsSLP) <- c(time = 10)
#'downscale_field <- Analogs(expL = expSLP, obsL = obsSLP, 
#'                           time_obsL = time_obsSLP,time_expL = "01-01-1994")
#'
#'# Example 2: Downscaling using criteria 'Large_dist' and 2 variables:
#'obs.pr <- c(rnorm(1:200) * 0.001)
#'dim(obs.pr) <- dim(obsSLP)
#'downscale_field <- Analogs(expL = expSLP, obsL = obsSLP, obsVar = obs.pr,
#'                           time_obsL = time_obsSLP, time_expL = "01-01-1994")
#'
#'# Example 3: Downscaling using criteria 'Local_dist' and 2 variables:
#'# analogs of local scale using criteria 2
#'region = c(lonmin = -1 ,lonmax = 2, latmin = 30, latmax = 33)
#'Local_scale <- Analogs(expL = expSLP, obsL = obsSLP, time_obsL = time_obsSLP,
#'                       obsVar = obs.pr, criteria = "Local_dist", 
#'                       lonL = seq(-1, 5, 1.5),latL = seq(30, 35, 1.5), 
#'                       region = region,time_expL = "01-10-2000", 
#'                       nAnalogs = 10, AnalogsInfo = TRUE)
#'
#'# Example 4: Downscaling using criteria 'Local_cor' and 2 variables:
#'exp.pr <- c(rnorm(1:20) * 0.001)
#'dim(exp.pr) <- dim(expSLP)
#'Local_scalecor <- Analogs(expL = expSLP, obsL = obsSLP, time_obsL = time_obsSLP,
#'                          obsVar = obs.pr, expVar = exp.pr,
#'                          criteria = "Local_cor", lonL = seq(-1, 5, 1.5),
#'                          time_expL = "01-10-2000", latL = seq(30, 35, 1.5),
#'                          lonVar = seq(-1, 5, 1.5), latVar = seq(30, 35, 1.5), 
#'                          nAnalogs = 8, region = region, AnalogsInfo = FALSE)
#'
#'# Example 5: List of best analogs in the three criterias Large_dist, 
#'Large_scale <- Analogs(expL = expSLP, obsL = obsSLP, time_obsL = time_obsSLP,
#'                       criteria = "Large_dist", time_expL = "01-10-2000",
#'                       nAnalogs = 7, AnalogsInfo = TRUE)
#'Local_scale <- Analogs(expL = expSLP, obsL = obsSLP, time_obsL = time_obsSLP,
#'                       time_expL = "01-10-2000", criteria = "Local_dist",
#'                       lonL = seq(-1, 5, 1.5), latL = seq(30, 35, 1.5), 
#'                       nAnalogs = 7, region = region, AnalogsInfo = TRUE)
#'Local_scalecor <- Analogs(expL = expSLP, obsL = obsSLP, time_obsL = time_obsSLP,
#'                          obsVar = obsSLP, expVar = expSLP, 
#'                          time_expL = "01-10-2000", criteria = "Local_cor", 
#'                          lonL = seq(-1, 5, 1.5), latL = seq(30, 35, 1.5),
#'                          lonVar = seq(-1, 5, 1.5), latVar = seq(30, 35, 1.5),
#'                          nAnalogs = 7, region = region, 
#'                          AnalogsInfo = TRUE)
#'@import multiApply
#'@import abind
#'@import s2dv
#'@importFrom ClimProjDiags SelBox Subset
#'@export
Analogs <- function(expL, obsL, time_obsL, time_expL = NULL, 
                    lonL = NULL, latL = NULL, expVar = NULL, obsVar = NULL, 
                    sdate_dim = 'sdate', criteria = "Large_dist", 
                    excludeTime = NULL, lonVar = NULL, latVar = NULL, 
                    region = NULL, nAnalogs = NULL, 
                    AnalogsInfo = FALSE, ncores = NULL) {
  # Check inputs
  # expL, obsL
  if (!is.array(expL) || !is.numeric(expL)) {
    stop("Parameter 'expL' must be a numeric array.")
  }
  if (!is.array(obsL) || !is.numeric(obsL)) {
    stop("Parameter 'obsL' must be a numeric array.")
  }
  obsdims <- names(dim(obsL))
  expdims <- names(dim(expL))
  if (is.null(expdims)) {
    stop("Parameter 'expL' must have dimension names.")
  }
  if (is.null(obsdims)) {
    stop("Parameter 'obsL' must have dimension names.")
  }
  if (any(is.na(expL)))  {
    warning("Parameter 'expL' contains NA values.")
  }
  if (any(is.na(obsL)))  {
    warning("Parameter 'obsL' contains NA values.")
  }
  if (!any(.KnownLonNames() %in% obsdims) | !any(.KnownLonNames() %in% expdims)) {
    stop("Parameter 'expL' and 'obsL' must have longitudinal dimension.")
  }
  if (!any(.KnownLatNames() %in% obsdims) | !any(.KnownLatNames() %in% expdims)) {
    stop("Parameter 'expL' and 'obsL' must have latitudinal dimension.")
  }

  # Know spatial coordinates names
  if (!any(obsdims %in% .KnownLonNames()) | 
      !any(obsdims %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted by ",
         "the package.")
  }

  lon_name <- obsdims[[which(obsdims %in% .KnownLonNames())]]
  lat_name <- obsdims[[which(obsdims %in% .KnownLatNames())]]

  # criteria
  if (!criteria %in% c('Large_dist', 'Local_dist', 'Local_cor')) {
    stop("Parameter 'criteria' can only be: 'Large_dist', 'Local_dist' or 'Local_cor'.")
  }
  if (length(criteria) > 1) {
    warning("Only first element of 'criteria' parameter will be used.")
    criteria <- criteria[1]
  }
  # lonL, latL, lonVar, latVar
  if (criteria == "Local_dist" | criteria == "Local_cor") {
    if (is.null(lonL) | is.null(latL)) {
      stop("Parameters 'lonL' and 'latL' cannot be NULL.")
    }
    if (!is.numeric(lonL) | !is.numeric(latL)) {
      stop("Parameters 'lonL' and 'latL' must be numeric.")
    }
    if (!is.null(dim(lonL)) | !is.null(dim(latL))) {
      if (length(dim(lonL)) == 1 & length(dim(latL)) == 1) {
        lonL <- as.vector(lonL)
        latL <- as.vector(latL)
      } else {
        stop("Parameters 'lonL' and 'latL' need to be a vector.")
      }
    }
  }
  if (criteria == "Local_cor") {
    if (is.null(lonVar) | is.null(latVar)) {
      stop("Parameters 'lonVar' and 'latVar' cannot be NULL.")
    }
    if (!is.numeric(lonVar) | !is.numeric(latVar)) {
      stop("Parameters 'lonVar' and 'latVar' must be numeric.")
    }
    if (!is.null(dim(lonVar)) | !is.null(dim(latVar))) {
      if (length(dim(lonVar)) == 1 & length(dim(latVar)) == 1) {
        lonVar <- as.vector(lonVar)
        latVar <- as.vector(latVar)
      } else {
        stop("Parameters 'lonVar' and 'latVar' need to be a vector.")
      }
    }
  }
  # expVar and obsVar
  if (!is.null(expVar) & is.null(obsVar)) {
    expVar <- NULL
    warning("Parameter 'expVar' is set to NULL as parameter 'obsVar', 
             large scale field will be returned.")
  }
  if (is.null(expVar) & is.null(obsVar)) {
    warning("Parameter 'expVar' and 'obsVar' are NULLs, downscaling/listing 
             same variable as obsL and expL'.")
  }
  if (!is.null(obsVar) & is.null(expVar) & criteria == "Local_cor") {
    stop("Parameter 'expVar' cannot be NULL.")
  }
  # nAnalogs
  if (is.null(nAnalogs) & criteria != "Large_dist") {
    nAnalogs = length(time_obsL)
    warning("Parameter 'nAnalogs' is NULL and is set to the same length of", 
            "'time_obsL' by default")
  }
  if (is.null(nAnalogs) & criteria == "Large_dist") {
    nAnalogs <- 1
  }
  # time_obsL, time_expL
  if (is.null(time_obsL)) {
    stop("Parameter 'time_obsL' cannot be NULL.")
  }
  if (is.null(time_expL)) {
    stop("Parameter 'time_expL' cannot be NULL.")
  }
  if (!inherits(time_obsL, "character")) {
    warning('imposing time_obsL to be a character')
    dims_time_obsL <- dim(time_obsL)
    time_obsL <- format(as.Date(time_obsL), '%d-%m-%Y')
    dim(time_obsL) <- dims_time_obsL
  }
  if (!inherits(time_expL, "character")) {
    warning('imposing time_expL to be a character')
    dims_time_expL <- dim(time_expL)
    time_expL <- format(as.Date(time_expL), '%d-%m-%Y')
    dim(time_expL) <- dims_time_expL
  }
  # time_obsL, time_expL (2)
  if (is.null(names(dim(time_obsL)))) {
    stop("Parameter 'time_obsL' must have named dimensions.")
  }
  if (!is.character(sdate_dim)) {
    stop("Parameter 'sdate_dim' must be a character string.")
  }
  if (!sdate_dim %in% names(dim(time_obsL))) {
    if (length(dim(time_obsL)) == 1) {
      dim(time_obsL) <- c(dim(time_obsL), sdate = 1)
    } else {
      stop("Parameters 'time_obsL' must have 'sdate_dim' dimension name. ",
           "If it has multiple time dimensions.")
    }
  }
  if (length(time_expL) != 1) {
    if (is.null(names(dim(time_expL)))) {
      stop("Parameter 'time_expL' must have named dimensions.")
    }
  } else {
    dim(time_expL) <- 1
  }
  if (!sdate_dim %in% names(dim(time_expL))) {
    if (length(dim(time_expL)) == 1) {
      dim(time_expL) <- c(dim(time_expL), sdate = 1)
    } else {
      stop("Parameters 'time_expL' must have 'sdate_dim' dimension name. ",
           "If it has multiple time dimensions.")
    }
  }
  if (length(dim(time_obsL)) == 2) {
    if (which(sdate_dim %in% names(dim(time_obsL))) == 1) {
      time_obsL <- Reorder(time_obsL, c(2,1))
    }
  } else {
    warning("Parameter 'time_obsL' should have forecast time and start date dimension in this order.")
  }
  if (length(dim(time_expL)) == 2) {
    if (which(sdate_dim %in% names(dim(time_expL))) == 1) {
      time_expL <- Reorder(time_expL, c(2,1))
    }
  } else {
    warning("Parameter 'time_expL' should have forecast time and start date dimension in this order.")
  }

  # excludeTime
  if (!is.null(excludeTime)) {
    if (!inherits(excludeTime, "character")) {
      warning('imposing excludeTime to be a character')
      excludeTime <- format(as.Date(excludeTime),'%d-%m-%Y')
    }
  }
  # obsVar, expVar
  if (!is.null(obsVar)) {
    if (any(names(dim(obsVar)) %in% 'ftime')) {
      if (any(names(dim(obsVar)) %in% 'time')) {
        stop("Multiple temporal dimensions ('ftime' and 'time') found",
             "in parameter 'obsVar'.")
      } else {
        time_pos_obsVar <-  which(names(dim(obsVar)) == 'ftime')
        names(dim(obsVar))[time_pos_obsVar] <- 'time'
        if (any(names(dim(expVar)) %in% 'ftime')) {
          time_pos_expVar <-  which(names(dim(expVar)) == 'ftime')
          names(dim(expVar))[time_pos_expVar] <- 'time'
        }
      }
    }
  }
  # obsL
  if (any(names(dim(obsL)) %in% 'ftime')) {
    if (any(names(dim(obsL)) %in% 'time')) {
      stop("Multiple temporal dimensions ('ftime' and 'time') found",
           "in parameter 'obsL'.")
    } else {
      time_pos_obsL <-  which(names(dim(obsL)) == 'ftime')
      names(dim(obsL))[time_pos_obsL] <- 'time'
      if (any(names(dim(expL)) %in% 'ftime')) {
        time_pos_expL <-  which(names(dim(expL)) == 'ftime')
        names(dim(expL))[time_pos_expL] <- 'time'
      }
    }
  }
  if ((any(names(dim(obsL)) %in% 'sdate')) &&
      (any(names(dim(obsL)) %in% 'time'))) {
    dims_obsL <- dim(obsL)
    pos_sdate <- which(names(dim(obsL)) == 'sdate')
    pos_time <- which(names(dim(obsL)) == 'time')
    pos <- 1 : length(dim(obsL))
    pos <- c(pos_time, pos_sdate, pos[-c(pos_sdate,pos_time)])
    obsL <- aperm(obsL, pos)
    dim(obsL) <- c(time = prod(dims_obsL[c(pos_time, pos_sdate)]),
                   dims_obsL[-c(pos_time, pos_sdate)])
  } else {
    if (any(names(dim(obsL)) %in% 'sdate')) {
      dims_obsL <- dim(obsL)
      pos_sdate <- which(names(dim(obsL)) == 'sdate')
      pos <- 1 : length(dim(obsL))
      pos <- c( pos_sdate, pos[-c(pos_sdate)])
      obsL <- aperm(obsL, pos)
      dim(obsL) <- c(time = prod(dims_obsL[c(pos_sdate)]),
                     dims_obsL[-c( pos_sdate)])
    } else {
      if (any(names(dim(obsL)) %in% 'time')) {
        dims_obsL <- dim(obsL)
        pos_time <- which(names(dim(obsL)) == 'time')
        if (length(time_obsL) != dim(obsL)[pos_time]) {
          stop("'time_obsL' and 'obsL' must have same length in the temporal
                dimension.")
        }
        pos <- 1 : length(dim(obsL))
        pos <- c(pos_time, pos[-c(pos_time)])
        obsL <- aperm(obsL, pos)
        dim(obsL) <- c(time = prod(dims_obsL[pos_time]),
                       dims_obsL[-c(pos_time)])
      } else {
        stop("Parameter 'obsL' must have a temporal dimension named 'time'.")
      }
    }
  }
  # obsVar
  if (!is.null(obsVar)) {
    if (any(names(dim(obsVar)) %in% 'sdate')) {
      if (any(names(dim(obsVar)) %in% 'time')) {
        dims_obsVar <- dim(obsVar)
        pos_sdate <- which(names(dim(obsVar)) == 'sdate')
        pos_time <- which(names(dim(obsVar)) == 'time')
        pos <- 1 : length(dim(obsVar))
        pos <- c(pos_time, pos_sdate, pos[-c(pos_sdate,pos_time)])
        obsVar <- aperm(obsVar, pos)
        dim(obsVar) <- c(time = prod(dims_obsVar[c(pos_time, pos_sdate)]),
                         dims_obsVar[-c(pos_time, pos_sdate)])
      } else {
        dims_obsVar <- dim(obsVar)
        pos_sdate <- which(names(dim(obsVar)) == 'sdate')
        pos <- 1 : length(dim(obsVar))
        pos <- c(pos_sdate, pos[-c(pos_sdate)])
        obsVar <- aperm(obsVar, pos)
        dim(obsVar) <- c(time = prod(dims_obsVar[c(pos_sdate)]),
                         dims_obsVar[-c(pos_sdate)])
      }
    } else {
      if (any(names(dim(obsVar)) %in% 'time')) {
        dims_obsVar <- dim(obsVar)
        pos_time <- which(names(dim(obsVar)) == 'time')
        if (length(time_obsL) != dim(obsVar)[pos_time]) {
          stop(" 'time_obsL' and 'obsVar' must have same length in the temporal
               dimension.")}
        pos <- 1 : length(dim(obsVar))
        pos <- c(pos_time, pos[-c(pos_time)])
        obsVar <- aperm(obsVar, pos)
        dim(obsVar) <- c(time = prod(dims_obsVar[c(pos_time)]),
                         dims_obsVar[-c(pos_time)])
      } else {
        stop("Parameter 'obsVar' must have a temporal dimension named 'time'.")
      }
    }
  }
  if (is.null(region) && criteria != 'Large_dist') {
    if (!is.null(lonVar) & !is.null(latVar)) {
      region <- c(min(lonVar), max(lonVar), min(latVar), max(latVar))
    } else {
      stop("Parameters 'lonVar' and 'latVar' must be given in criteria 
           'Local_dist' and 'Local_cor'")
    }
  }
  if (any(names(dim(expL)) %in% c('ftime', 'leadtime', 'ltime'))) {
    if (length(which(names(dim(expL)) %in% 
                     c('ftime', 'leadtime', 'ltime') == TRUE)) > 1) {
      stop("Parameter 'expL' cannot have multiple forecast time dimensions")
    } else {
    names(dim(expL))[which(names(dim(expL)) %in% c('ftime','leadtime','ltime'))] <- 'time'
    }
  }
  # remove dimension length 1 to simplify outputs:
  if (any(dim(obsL) == 1)) {
    obsL <- adrop(obsL, which(dim(obsL) == 1))
  }
  if (any(dim(expL) == 1)) {
    expL <- adrop(expL, which(dim(expL) == 1))
  }
  if (!is.null(obsVar)) {
    if (any(dim(obsVar) == 1)) {
      obsVar <- adrop(obsVar, which(dim(obsVar) == 1))
    }
  }
  if (!is.null(expVar)) {
    if (any(dim(expVar) == 1)) {
      expVar <- adrop(expVar, which(dim(expVar) == 1))
    }
  }
  names(dim(expL)) <- replace_repeat_dimnames(names(dim(expL)),
                                              names(dim(obsL)), 
                                              lon_name = lon_name, 
                                              lat_name = lat_name)
  if (!is.null(expVar)) {
    names(dim(expVar)) <- replace_repeat_dimnames(names(dim(expVar)),
                                                  names(dim(obsVar)),
                                                  lon_name = lon_name, 
                                                  lat_name = lat_name)
  }
  
  if (is.null(excludeTime)) {
    excludeTime <- vector(mode = "character", length = length(time_expL))

  }
  if (length(time_expL) == length(excludeTime)) {
    if (any(names(dim(expL)) %in% c('sdate_exp'))) {
      dim(time_expL) <- c(dim(expL)['sdate_exp'], dim(expL)['time_exp'])
    } else if (any(names(dim(expL)) %in% c('sdate'))) {
      if (any(names(dim(expL)) %in% c('time_exp'))) {
        dim(time_expL) <- c(dim(expL)['sdate'], dim(expL)['time_exp'])
        dim(excludeTime) <- c(dim(expL)['sdate'], dim(expL)['time_exp'])
      } else if (any(names(dim(expL)) %in% c('time'))) {
        dim(time_expL) <- c(dim(expL)['sdate'], dim(expL)['time'])
        dim(excludeTime) <- c(dim(expL)['sdate'], dim(expL)['time'])
      } else {
        dim(time_expL) <- c(dim(expL)['sdate'])
        dim(excludeTime) <- c(dim(expL)['sdate'])
      } 
    } else if (any(names(dim(expL)) %in% c('time'))) {
      dim(time_expL) <- c(dim(expL)['time'])
      dim(excludeTime) <- c(dim(expL)['time'])
    } else if (any(names(dim(expL)) %in% c('time_exp'))) {
      dim(time_expL) <- c(dim(expL)['time_exp'])
      dim(excludeTime) <- c(dim(expL)['time_exp'])
    }
  }
  if (!AnalogsInfo) {
    if (is.null(obsVar)) {
      res <- Apply(list(expL, obsL),
                   target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name)),
                   fun = .analogs, time_obsL, expVar = expVar,
                   time_expL = time_expL, excludeTime = excludeTime,
                   obsVar = obsVar, criteria = criteria,
                   lonL = lonL, latL = latL,
                   lonVar = lonVar, latVar = latVar, region = region,
                   nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo, 
                   lon_name = lon_name, lat_name = lat_name, 
                   output_dims = c('nAnalogs', lat_name, lon_name), 
                   ncores = ncores)$output1

    } else if (!is.null(obsVar) && is.null(expVar)) {
        res <- Apply(list(expL, obsL, obsVar), 
                     target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name),
                                        c('time', lat_name, lon_name)),
                     fun = .analogs, time_obsL, 
                     time_expL = time_expL, excludeTime = excludeTime,
                     expVar = expVar, criteria = criteria,
                     lonL = lonL, latL = latL,
                     lonVar = lonVar, latVar = latVar, region = region,
                     nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo,
                     lon_name = lon_name, lat_name = lat_name, 
                     output_dims = c('nAnalogs', lat_name, lon_name), 
                     ncores = ncores)$output1

    } else if (!is.null(obsVar) && !is.null(expVar)) {
        res <- Apply(list(expL, obsL, obsVar, expVar),
                     target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name),
                                        c('time', lat_name, lon_name), c(lat_name, lon_name)),
                     fun = .analogs,
                     criteria = criteria, time_obsL,
                     time_expL = time_expL, excludeTime = excludeTime,
                     lonL = lonL, latL = latL,
                     lonVar = lonVar, latVar = latVar, region = region,
                     nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo, 
                     lon_name = lon_name, lat_name = lat_name, 
                     output_dims = c('nAnalogs', lat_name, lon_name), 
                     ncores = ncores)$output1
    }
  } else {
    if (is.null(obsVar)) {
        res <- Apply(list(expL, obsL),
                     target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name)),
                     fun = .analogs, time_obsL, expVar = expVar,
                     time_expL = time_expL, excludeTime = excludeTime,
                     obsVar = obsVar, criteria = criteria,
                     lonL = lonL, latL = latL,
                     lonVar = lonVar, latVar = latVar, region = region,
                     nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo, 
                     lon_name = lon_name, lat_name = lat_name, 
                     output_dims = list(fields = c('nAnalogs', lat_name, lon_name),
                                        analogs = c('nAnalogs'),
                                        metric = c('nAnalogs', 'metric'),
                                        dates = c('nAnalogs')),
                     ncores = ncores)
    } else if (!is.null(obsVar) && is.null(expVar)) {
        res <- Apply(list(expL, obsL, obsVar),
                     target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name),
                                        c('time', lat_name, lon_name)),
                     fun = .analogs, time_obsL,
                     time_expL = time_expL, excludeTime = excludeTime,
                     expVar = expVar, criteria = criteria,
                     lonL = lonL, latL = latL,
                     lonVar = lonVar, latVar = latVar, region = region,
                     nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo, 
                     lon_name = lon_name, lat_name = lat_name, 
                     output_dims = list(fields = c('nAnalogs', lat_name, lon_name),
                                        analogs = c('nAnalogs'),
                                        metric = c('nAnalogs', 'metric'),
                                        dates = c('nAnalogs')),
                     ncores = ncores)

    } else if (!is.null(obsVar) && !is.null(expVar)) {
      res <- Apply(list(expL, obsL, obsVar, expVar),
                   target_dims = list(c(lat_name, lon_name), c('time', lat_name, lon_name),
                                      c('time', lat_name, lon_name), c(lat_name, lon_name)),
                   fun = .analogs, time_obsL, 
                   criteria = criteria,
                   time_expL = time_expL, excludeTime = excludeTime,
                   lonL = lonL, latL = latL,
                   lonVar = lonVar, latVar = latVar, region = region,
                   nAnalogs = nAnalogs, AnalogsInfo = AnalogsInfo, 
                   lon_name = lon_name, lat_name = lat_name, 
                   output_dims = list(fields = c('nAnalogs', lat_name, lon_name),
                                      analogs = c('nAnalogs'),
                                      metric = c('nAnalogs', 'metric'),
                                      dates = c('nAnalogs')),
                   ncores = ncores)
    }
  }
  return(res)
}
.analogs <- function (expL, obsL, time_expL,  excludeTime = NULL,
                      obsVar = NULL, expVar = NULL,
                      time_obsL, criteria = "Large_dist",
                      lonL = NULL, latL = NULL,
                      lonVar = NULL, latVar = NULL, region = NULL, 
                      nAnalogs = NULL, AnalogsInfo = FALSE, lon_name = 'lon',
                      lat_name = 'lat') {

  if (all(excludeTime == "")) {
    excludeTime = NULL
  }

  if (!is.null(obsL)) {
    #obsL <- replace_time_dimnames(obsL)
    if (any(time_expL %in% time_obsL)) {
      if (is.null(excludeTime)) {
        excludeTime <- time_expL
        warning("Parameter 'excludeTime' is NULL, time_obsL contains
                 time_expL, so, by default, the date of 
                 time_expL will be excluded in the search of analogs")
      } else {
        `%!in%` = Negate(`%in%`)
        if(any(time_expL %!in% excludeTime)) {
          excludeTime <- c(excludeTime, time_expL)
          warning("Parameter 'excludeTime' is not NULL, time_obsL contains
                   time_expL, so, by default, the date of 
                   time_expL will be excluded in the search of analogs")
        }
      }
      time_ref <- time_obsL[-c(which(time_obsL %in% excludeTime))]
      posdim <- which(names(dim(obsL)) == 'time')
      posref <- which(time_obsL %in% time_ref)
      obsT <- Subset(obsL, along = posdim, indices = posref)
      if (!is.null(obsVar)) {
        obsTVar <- Subset(obsVar, along = posdim, indices = posref)
      }
      time_obsL <- time_ref
      obsL <- obsT
      if (!is.null(obsVar)) {
        obsVar <- obsTVar
      }
    } else {
      if (is.null(excludeTime)) {
        if (!is.null(obsVar)) {
          warning("Parameter 'excludeTime' is NULL, time_obsL does not contain 
                   time_expL, obsVar not NULL")
        } else {
          warning("Parameter 'excludeTime' is NULL, time_obsL does not contain 
                   time_expL")
        }
      } else {
        time_ref <- time_obsL[-c(which(time_obsL %in% excludeTime))]
        posdim <- which(names(dim(obsL)) == 'time')
        posref <- which(time_obsL %in% time_ref)
        obsT <- Subset(obsL, along = posdim, indices = posref)
        if (!is.null(obsVar)) {
          obsTVar <- Subset(obsVar, along = posdim, indices = posref)
        }
        time_obsL <- time_ref
        obsL <- obsT
        if (!is.null(obsVar)) {
          obsVar <- obsTVar
        }
        if (!is.null(obsVar)) {
          warning("Parameter 'excludeTime' has a value and time_obsL does not 
                   contain time_expL, obsVar not NULL")
        } else {
          warning("Parameter 'excludeTime' has a value and time_obsL does not 
                   contain time_expL")
        }
      }
    }
  } else {
    stop("parameter 'obsL' cannot be NULL")
  }
  if (length(time_obsL) == 0) {
    stop("Parameter 'time_obsL' can not be length 0")
  }
    Analog_result <- FindAnalog(expL = expL, obsL = obsL, time_obsL = time_obsL,
                                expVar = expVar, obsVar = obsVar, 
                                criteria = criteria, 
                                AnalogsInfo = AnalogsInfo,
                                nAnalogs = nAnalogs,
                                lonL = lonL, latL = latL, lonVar = lonVar, 
                                latVar = latVar, region = region, 
                                lon_name = lon_name, lat_name = lat_name)
    if (AnalogsInfo == TRUE) {
      return(list(AnalogsFields = Analog_result$AnalogsFields, 
                  AnalogsInfo = Analog_result$Analog,
                  AnalogsMetric = Analog_result$metric,
                  AnalogsDates = Analog_result$dates))
    } else {
      return(AnalogsFields = Analog_result$AnalogsFields)
    }
}
FindAnalog <- function(expL, obsL, time_obsL, expVar, obsVar, criteria, 
                       lonL, latL, lonVar,
                       latVar, region, nAnalogs = nAnalogs,
                       AnalogsInfo = AnalogsInfo, lon_name = 'lon', lat_name = 'lat') {
  position <- Select(expL = expL, obsL = obsL,  expVar = expVar, 
                     obsVar = obsVar, criteria = criteria, 
                     lonL = lonL, latL = latL, lonVar = lonVar, 
                     latVar = latVar, region = region,
                     lon_name = lon_name, lat_name = lat_name)$position
  metrics <- Select(expL = expL, obsL = obsL,  expVar = expVar, 
                   obsVar = obsVar, criteria = criteria, lonL = lonL,
                   latL = latL, lonVar = lonVar, 
                   latVar = latVar, region = region, 
                   lon_name = lon_name, lat_name = lat_name)$metric.original
  best <- Apply(list(position), target_dims = c('time', 'pos'), 
                fun = BestAnalog, criteria = criteria, 
                AnalogsInfo = AnalogsInfo, nAnalogs = nAnalogs)$output1

  Analogs_dates <- time_obsL[best]
  dim(Analogs_dates) <- dim(best)
  if (all(!is.null(region), !is.null(lonVar), !is.null(latVar))) {
    if (is.null(obsVar)) {
      obsVar <- SelBox(obsL, lon = lonL, lat = latL, region = region, 
                       londim = lon_name, latdim = lat_name)$data
      expVar <- SelBox(expL, lon = lonL, lat = latL, region = region, 
                       londim = lon_name, latdim = lat_name)$data
      Analogs_fields <- Subset(obsVar, 
                               along = which(names(dim(obsVar)) == 'time'),
                               indices = best)
      warning("Parameter 'obsVar' is NULL and the returned field ", 
              "will be computed from 'obsL' (same variable).")
      
    } else {
      obslocal <- SelBox(obsVar, lon = lonVar, lat = latVar, 
                         region = region, londim = lon_name, latdim = lat_name)$data
      Analogs_fields <- Subset(obslocal, 
                               along = which(names(dim(obslocal)) == 'time'),
                               indices = best)
    }
  } else {
    warning("One or more of the parameter 'region', 'lonVar' and 'latVar'",
            " are NULL and the large scale field will be returned.")
    if (is.null(obsVar)) {
      Analogs_fields <- Subset(obsL, along = which(names(dim(obsL)) == 'time'),
                               indices = best)
    } else {
      Analogs_fields <- Subset(obsVar,
                               along = which(names(dim(obsVar)) == 'time'),
                               indices = best)
    }
  }
  
  lon_dim <- which(names(dim(Analogs_fields)) == lon_name)
  lat_dim <- which(names(dim(Analogs_fields)) == lat_name)
  
  Analogs_metrics <- Subset(metrics,
                            along = which(names(dim(metrics)) == 'time'),
                            indices = best)
  analog_number <- as.numeric(1:nrow(Analogs_metrics))
  dim(analog_number) <- c(nAnalog = length(analog_number))
  dim(Analogs_dates) <- c(nAnalog = length(Analogs_dates))
  return(list(AnalogsFields = Analogs_fields, 
              Analog = analog_number, 
              metric = Analogs_metrics,
              dates = Analogs_dates))
} 

BestAnalog <- function(position, nAnalogs = nAnalogs, AnalogsInfo = FALSE, 
                       criteria = 'Large_dist') {
  pos_dim <- which(names(dim(position)) == 'pos')
  if (dim(position)[pos_dim] == 1) {
    pos1 <- Subset(position, along = pos_dim, indices = 1)
    if (criteria != 'Large_dist') {
      warning("Dimension 'pos' in parameter 'position' has length 1,",
              " criteria 'Large_dist' will be used.")
      criteria <- 'Large_dist'
    }
  } else if (dim(position)[pos_dim] == 2) {
    pos1 <- Subset(position, along = pos_dim, indices = 1)
    pos2 <- Subset(position, along = pos_dim, indices = 2)
    if (criteria == 'Local_cor') {
      warning("Dimension 'pos' in parameter 'position' has length 2,",
              " criteria 'Local_dist' will be used.")
      criteria <- 'Local_dist'
    }
  } else if (dim(position)[pos_dim] == 3) {
    pos1 <- Subset(position, along = pos_dim, indices = 1)
    pos2 <- Subset(position, along = pos_dim, indices = 2)
    pos3 <- Subset(position, along = pos_dim, indices = 3)
    if (criteria != 'Local_cor') {
      warning("Parameter 'criteria' is set to", criteria, ".")
    }
  } else {
    stop("Parameter 'position' has dimension 'pos' of different ",
         "length than expected (from 1 to 3).")
  }
  if (criteria == 'Large_dist') {
    if (AnalogsInfo == FALSE) {
      pos <- pos1[1]
    } else {
      pos <- pos1[1 : nAnalogs]
    }
  } else if (criteria == 'Local_dist') {
    pos1 <- pos1[1 : nAnalogs]
    pos2 <- pos2[1 : nAnalogs]
    best <- match(pos1, pos2)
    if (length(best) == 1) {
      warning("Just 1 best analog matching Large_dist and ", 
              "Local_dist criteria")
    } 
    if (length(best) < 1 | is.na(best[1]) == TRUE) {
      stop("no best analogs matching Large_dist and Local_dist criterias, 
            please increase nAnalogs") 
    }
    pos <- pos2[as.logical(best)]
    pos <- pos[which(!is.na(pos))]
    if (AnalogsInfo == FALSE) { 
      pos <- pos[1]
    }else {
      pos <- pos}
  } else if (criteria == 'Local_cor') {
    pos1 <- pos1[1 : nAnalogs]
    pos2 <- pos2[1 : nAnalogs]
    best <- match(pos1, pos2)
    if (length(best) == 1) {
      warning("Just 1 best analog matching Large_dist and ", 
              "Local_dist criteria")
    } 
    if (length(best) < 1 | is.na(best[1]) == TRUE) {
      stop("no best analogs matching Large_dist and Local_dist criterias, 
            please increase nAnalogs") 
    }
    pos <- pos1[as.logical(best)]
    pos <- pos[which(!is.na(pos))]
    pos3 <- pos3[1 : nAnalogs]
    best <- match(pos, pos3)
    if (length(best) == 1) {
      warning("Just 1 best analog matching Large_dist, Local_dist and ", 
              "Local_cor criteria")
    } 
    if (length(best) < 1 | is.na(best[1]) == TRUE) {
      stop("no best analogs matching Large_dist, Local_dist and Local_cor 
            criterias, please increase nAnalogs") 
    }
    pos <- pos[order(best, decreasing = F)]
    pos <- pos[which(!is.na(pos))]
    if (AnalogsInfo == FALSE) { 
      pos <- pos[1]
    } else{
      pos <- pos
    }
    return(pos)
  }
}
Select <- function(expL, obsL,  expVar = NULL, obsVar = NULL, 
                   criteria = "Large_dist", lonL = NULL, latL = NULL,
                   lonVar = NULL, latVar = NULL, region = NULL, 
                   lon_name = 'lon', lat_name = 'lat') {
  names(dim(expL)) <- replace_repeat_dimnames(names(dim(expL)), 
                                              names(dim(obsL)),
                                              lon_name = lon_name,
                                              lat_name = lat_name)
  metric1 <- Apply(list(obsL), target_dims = list(c(lat_name, lon_name)), 
                   fun = .select, expL, metric = "dist",
                   lon_name = lon_name, lat_name = lat_name)$output1
  metric1.original = metric1
  if (length(dim(metric1)) > 1) {
    dim_time_obs <- which(names(dim(metric1)) == 'time' | 
                          names(dim(metric1)) == 'ftime')
    dim(metric1) <- c(dim(metric1), metric=1)
    margins <- c(1 : (length(dim(metric1))))[-dim_time_obs]
    pos1 <- apply(metric1, margins, order)      
    names(dim(pos1))[1] <- 'time'
    metric1.original = metric1
    metric1 <-  apply(metric1, margins, sort)
    names(dim(metric1))[1] <- 'time'
    names(dim(metric1.original)) = names(dim(metric1))
  } else {
    pos1 <- order(metric1)
    dim(pos1) <- c(time = length(pos1))
    metric1 <- sort(metric1)
    dim(metric1) <- c(time = length(metric1))
    dim(metric1.original) = dim(metric1)
    dim_time_obs = 1
  }
  if (criteria == "Large_dist") {
    dim(metric1) <- c(dim(metric1), metric = 1)
    dim(pos1) <- c(dim(pos1), pos = 1)
    dim(metric1.original) = dim(metric1)
    return(list(metric = metric1, metric.original = metric1.original,
                position = pos1))
  }
  if (criteria == "Local_dist" | criteria == "Local_cor") {
    obs <- SelBox(obsL, lon = lonL, lat = latL, region = region, 
                  londim = lon_name, latdim = lat_name)$data
    exp <- SelBox(expL, lon = lonL, lat = latL, region = region, 
                  londim = lon_name, latdim = lat_name)$data
    metric2 <- Apply(list(obs), target_dims = list(c(lat_name, lon_name)), 
                     fun = .select, exp, metric = "dist", 
                     lon_name = lon_name, lat_name = lat_name)$output1
    metric2.original = metric2
    dim(metric2) <- c(dim(metric2), metric=1)
    margins <- c(1 : (length(dim(metric2))))[-dim_time_obs]
    pos2 <- apply(metric2, margins, order)
    dim(pos2) <- dim(pos1)
    names(dim(pos2))[1] <- 'time'
    metric2 <-  apply(metric2, margins, sort)
    names(dim(metric2))[1] <- 'time'
    if (criteria == "Local_dist") {
      metric <- abind(metric1, metric2, along = length(dim(metric1))+1)
      metric.original <- abind(metric1.original,metric2.original,
                               along = length(dim(metric1))+1)
      position <- abind(pos1, pos2, along = length(dim(pos1))+1) 
      names(dim(metric)) <- c(names(dim(pos1)), 'metric')
      names(dim(position)) <- c(names(dim(pos1)), 'pos')
      names(dim(metric.original)) = names(dim(metric)) 
      return(list(metric = metric, metric.original = metric.original,
                  position = position))
    }   
  }
  if (criteria == "Local_cor") {
    obs <- SelBox(obsVar, lon = lonVar, lat = latVar, region = region, 
                  londim = lon_name, latdim = lat_name)$data
    exp <- SelBox(expVar, lon = lonVar, lat = latVar, region = region, 
                  londim = lon_name, latdim = lat_name)$data
    metric3 <- Apply(list(obs), target_dims = list(c(lat_name, lon_name)), 
                     fun = .select, exp, metric = "cor",
                     lon_name = lon_name, lat_name = lat_name)$output1
    metric3.original = metric3
    dim(metric3) <- c(dim(metric3), metric=1)
    margins <- c(1 : (length(dim(metric3))))[-dim_time_obs]
    pos3 <- apply(abs(metric3), margins, order, decreasing = TRUE)
    names(dim(pos3))[1] <- 'time'
    metricsort <- metric3[pos3]
    dim(metricsort) = dim(metric3)
    names(dim(metricsort))[1] <- 'time'
    metric <- abind(metric1, metric2, metricsort, 
                    along = length(dim(metric1)) + 1)
    metric.original <- abind(metric1.original, metric2.original, 
                             metric3.original, 
                             along = length(dim(metric1)) + 1)
    position <- abind(pos1, pos2, pos3, along = length(dim(pos1)) + 1)   
    names(dim(metric)) <- c(names(dim(metric1)), 'metric')
    names(dim(position)) <- c(names(dim(pos1)), 'pos')
    names(dim(metric.original)) = names(dim(metric)) 
    return(list(metric = metric, metric.original=metric.original,
                position = position))
  }
  else {
    stop("Parameter 'criteria' must to be one of the: 'Large_dist', ",
         "'Local_dist','Local_cor'.")
  }
}
.select <- function(exp, obs, metric = "dist", 
                    lon_name = 'lon', lat_name = 'lat') {
  if (metric == "dist") {
    result <- Apply(list(obs), target_dims = list(c(lat_name, lon_name)), 
            fun = function(x) {sqrt(sum((x - exp) ^ 2, na.rm = TRUE))})$output1
  } else if (metric == "cor") {
    result <- Apply(list(obs), target_dims = list(c(lat_name, lon_name)), 
                    fun = function(x) {cor(as.vector(x), 
                                           as.vector(exp),
                                           method = "spearman")})$output1
  } 
  result
}
.time_ref <- function(time_obsL,time_expL,excludeTime) {
  sameTime = which(time_obsL %in% time_expL)
  result<- c(time_obsL[1:(sameTime - excludeTime - 1)],
             time_obsL[(sameTime + excludeTime + 1):length(time_obsL)])
  result
}

replace_repeat_dimnames <- function(names_exp, names_obs, lat_name = 'lat', 
                                    lon_name = 'lon') {
  if (!is.character(names_exp)) {
    stop("Parameter 'names_exp' must be a vector of characters.")
  }
  if (!is.character(names_obs)) {
    stop("Parameter 'names_obs' must be a vector of characters.")
  }
  latlon_dim_exp <- which(names_exp == lat_name | names_exp == lon_name)
  latlon_dim_obs <- which(names_obs == lat_name | names_obs == lon_name)
  if (any(unlist(lapply(names_exp[-latlon_dim_exp],
                        function(x){x == names_obs[-latlon_dim_obs]})))) {
    original_pos <- lapply(names_exp, 
                           function(x) which(x == names_obs[-latlon_dim_obs]))
    original_pos <- lapply(original_pos, length) > 0
    names_exp[original_pos] <- paste0(names_exp[original_pos], "_exp")
  }
  return(names_exp)
}

replace_time_dimnames <- function(dataL, time_name = 'time', 
                                  stdate_name = 'stdate', ftime_name='ftime') {
  names_obs = names(dim(dataL))
  if (!is.character(names_obs)) {
    stop("Parameter 'names_obs' must be a vector of characters.")
  }
  time_dim_obs <- which(names_obs == time_name | 
                        names_obs == stdate_name | names_obs == ftime_name)
  if (length(time_dim_obs) > 1) {
    stop ("more than 1 time dimension, please give just 1")
  }
  if (length(time_dim_obs) == 0) {
    warning ("name of time dimension is not 'ftime' or 'time' or 'stdate' 
              or time dimension is null")
  }
  if (length(time_dim_obs) != 0) {
    names_obs[time_dim_obs]= time_name
  }
  names(dim(dataL)) = names_obs
  return(dataL)
}
