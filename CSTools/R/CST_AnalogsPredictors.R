#' AEMET Downscaling   
#' Precipitation and maximum and minimum temperature downscaling method 
#' based on analogs: synoptic situations and significant predictors. 
#'
#'@author Marta Dominguez Alonso - AEMET, \email{mdomingueza@aemet.es}
#'@author Nuria Perez-Zanon - BSC, \email{nuria.perez@bsc.es}
#'
#'@description This function downscales low resolution precipitation data (e.g. 
#'from Seasonal Forecast Models) through the association with an observational 
#'high resolution (HR) dataset (AEMET 5 km gridded data of daily precipitation 
#'(Peral et al., 2017)) and a collection of predictors and past synoptic 
#'situations similar to estimated day. The method uses three domains: 
#'\itemize{
#'  \item{Peninsular Spain and Balearic Islands domain (5 km resolution): HR precipitation 
#'        and the downscaling result domain.}
#'  \item{Synoptic domain (low resolution, e.g. 1.5º x 1.5º): it should be 
#'        centered over Iberian Peninsula and cover enough extension to detect 
#'        as much synoptic situations as possible.}
#'  \item{Extended domain (low resolution, e.g. 1.5º x 1.5º): it should have the 
#'        same resolution as synoptic domain. It is used for SLP Seasonal 
#'        Forecast Models.}
#' }
#' 
#'@param exp List of arrays with downscaled period seasonal forecast data. The 
#'  list has to contain model atmospheric variables (instantaneous 12h data) 
#'  that must be indentify by parenthesis name. For precipitation:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500_mod) in m/s.}
#'    \item{v component of wind at 500 hPa (v500_mod) in m/s.}
#'    \item{temperature at 500 hPa (t500_mod) in K.}
#'    \item{temperature at 850 hPa (t850_mod) in K.}
#'    \item{specific humidity at 700 hPa (q700_mod) in g/kg. }
#'  }
#'  For temperature:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500_mod) in m/s.}
#'    \item{v component of wind at 500 hPa (v500_mod) in m/s.}
#'    \item{temperature at 500 hPa (t500_mod) in K.}
#'    \item{temperature at 700 hPa (t700_mod) in K. }
#'    \item{temperature at 850 hPa (t850_mod) in K.}
#'    \item{specific humidity at 700 hPa (q700_mod) in g/kg. }
#'    \item{2 meters temperature (tm2m_mod) in K.}
#'  }
#'  The arrays must have at least three dimensions with names 'lon', 'lat' and 
#'  'time'. (lon = gridpoints of longitude, lat = gridpoints of latitude, 
#'  time = number of downscaling days) Seasonal forecast variables must have the 
#'  same resolution and domain as reanalysis variables ('obs' parameter, below).
#'@param slp Array with atmospheric seasonal forecast model sea level pressure
#'  (instantaneous 12h data) that must be indentify as 'slp' (hPa). It has the  
#'  same resolution as 'exp' and 'obs' paremeters but with an extended domain.
#'  This domain contains extra degrees (most in the north and west part) compare  
#'  to synoptic domain. The array must have at least three dimensions with
#'  names 'lon', 'lat' and 'time'.
#'@param obs List of arrays with training period reanalysis data. 
#'  The list has to contain reanalysis atmospheric variables (instantaneous 
#'  12h data) that must be indentify by parenthesis name. For precipitation:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500) in m/s.}
#'    \item{v component of wind at 500 hPa (v500) in m/s.}
#'    \item{temperature at 500 hPa (t500) in K.}
#'    \item{temperature at 850 hPa (t850) in K.}
#'    \item{sea level pressure (slp) in hPa.}
#'    \item{specific humidity at 700 hPa (q700) in g/kg.}
#'  }
#'  For maximum and minimum temperature:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500) in m/s.}
#'    \item{v component of wind at 500 hPa (v500) in m/s.}
#'    \item{temperature at 500 hPa (t500) in K.}
#'    \item{temperature at 700 hPa (t700) in K.}
#'    \item{temperature at 850 hPa (t850) in K.}
#'    \item{sea level pressure (slp) in hPa.}
#'    \item{specific humidity at 700 hPa (q700) in g/kg}
#'    \item{2 meters temperature (tm2m) in K}
#'  }
#'  The arrays must have at least three dimensions with names 'lon', 'lat' and 
#'  'time'.
#'@param lon Vector of the synoptic longitude (from (-180º) to 180º), 
#'  The vector must go from west to east. The same as for the training function. 
#'@param lat Vector of the synoptic latitude. The vector must go from north to 
#'  south. The same as for the training function.
#'@param slp_lon Vector of the extended longitude (from (-180º) to 180º), 
#'  The vector must go from west to east. The same as for the training function. 
#'@param slp_lat Vector of the extended latitude. The vector must go from north 
#'  to south. The same as for the training function.
#'@param var_name Variable name to downscale. There are two options: 'prec' for
#'  precipitation and 'temp' for maximum and minimum temperature.
#'@param hr_obs Local path of HR observational files (maestro and pcp/tmx-tmn). 
#'  For precipitation and temperature can be downloaded from the following link:
#'  \url{https://www.aemet.es/en/serviciosclimaticos/cambio_climat/datos_diarios?w=2} 
#'  respetively. Maestro file (maestro_red_hr_SPAIN.txt) has gridpoint (nptos), 
#'  longitude (lon), latitude (lat) and altitude (alt) in columns (vector 
#'  structure). Data file (pcp/tmx/tmn_red_SPAIN_1951-201903.txt) includes 5km 
#'  resolution spanish daily data (precipitation or maximum and minimum 
#'  temperature from january 1951 to june 2020. See README file for more 
#'  information. IMPORTANT!: HR observational period must be the same as for 
#'  reanalysis variables. It is assumed that the training period is smaller than 
#'  the HR original one (1951-2019), so it is needed to make a new ascii file 
#'  with the new period and the same structure as original, specifying the 
#'  training dates in the name 
#'  (e.g. 'pcp_red_SPAIN_19810101-19961231.txt' for '19810101-19961231' period). 
#'@param tdates Training period dates in format YYYYMMDD(start)-YYYYMMDD(end) 
#'  (e.g. 19810101-20181231).  
#'@param ddates Downscaling period dates in format YYYYMMDD(start)-YYYYMMDD(end) 
#'  (e.g. 20191001-20200331).
#'@param restrain Output (list of matrix) obtained from 'training_analogs' 
#'  function. For precipitation, 'restrain' object must contains um, vm, nger, 
#'  gu92, gv92, gu52, gv52, neni, vdmin, vref, ccm, lab_pred and cor_pred 
#'  variables. For maximum and minimum temperature, 'restrain' object must 
#'  contains um, vm, insol, neni, vdmin y vref. See 'AnalogsPred_train.R' for 
#'  more information.
#'@param dim_name_longitude A character string indicating the name of the 
#'  longitude dimension, by default 'longitude'.
#'@param dim_name_latitude A character string indicating the name of the 
#'  latitude dimension, by default 'latitude'.
#'@param dim_name_time A character string indicating the name of the time 
#'  dimension, by default 'time'.
#'@return Matrix with seasonal forecast precipitation (mm) or maximum and 
#'minimum temperature (dozens of ºC) in a 5km x 5km regular grid over peninsular 
#'Spain and Balearic Islands. The resulted matrices have two dimensions 
#'('ddates' x 'nptos').(ddates = number of downscaling days and nptos = number 
#'of 'hr_obs' gridpoints).  
#'
#'@useDynLib CSTools
#'@export
CST_AnalogsPredictors <- function(exp, slp, obs, lon, lat, slp_lon, slp_lat,
                                  var_name, hr_obs, tdates, ddates, restrain,
                                  dim_name_longitude = "lon",
                                  dim_name_latitude = "lat",        
                                  dim_name_time = "time") {
  if (!is.list(exp)) {
    stop("Parameter 'exp' must be a list of 'array' objects")
  }

  if (!(all(sapply(exp, inherits, 'array')))) {
    stop("Elements of the list in parameter 'exp' must be of the class ",
         "'array'.")
  }

  if (!is.array(slp)) {
    stop("Parameter 'slp' must be of the class 'array'.")
  }

  if (!is.list(obs)) {
    stop("Parameter 'obs' must be a list of 'array' objects")
  }

  if (!(all(sapply(obs, inherits, 'array')))) {
    stop("Elements of the list in parameter 'obs' must be of the class ",
          "'array'.")
  }

  if (var_name == "prec") {
    if (length(exp) != 5) {
      stop("Parameter 'exp' must be a length of 5.")
    } else {
      if (!(any(names(exp) %in% "u500_mod"))) {
        stop("Variable 'u500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "v500_mod"))) {
        stop("Variable 'v500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "t500_mod"))) {
        stop("Variable 't500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "t850_mod"))) {
        stop("Variable 't850_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "q700_mod"))) {
        stop("Variable 'q700_mod' in 'exp' parameter is missed.")
      }
    }
    if (length(obs) != 6) {
      stop("Parameter 'obs' must be a length of 6.")
    } else {
      if (!(any(names(obs) %in% "u500"))) {
        stop("Variable 'u500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "v500"))) {
        stop("Variable 'v500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "t500"))) {
        stop("Variable 't500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "t850"))) {
        stop("Variable 't850' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "slp"))) {
        stop("Variable 'slp' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "q700"))) {
        stop("Variable 'q700' in 'obs' parameter is missed.")
      }
    }
  } else {
    if (length(exp) != 7) {
        stop("Parameter 'exp' must be a length of 7.")
    } else {
      if (!(any(names(exp) %in% "u500_mod"))) {
        stop("Variable 'u500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "v500_mod"))) {
        stop("Variable 'v500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "t500_mod"))) {
        stop("Variable 't500_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "t700_mod"))) {
        stop("Variable 't700_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "t850_mod"))) {
        stop("Variable 't850_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "q700_mod"))) {
        stop("Variable 'q700_mod' in 'exp' parameter is missed.")
      } else if (!(any(names(exp) %in% "tm2m_mod"))) {
        stop("Variable 'tm2m_mod' in 'exp' parameter is missed.")
      }
    }
    if (length(obs) != 8) {
      stop("Parameter 'obs' must be a length of 8.")
    } else {
      if (!(any(names(obs) %in% "u500"))) {
        stop("Variable 'u500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "v500"))) {
        stop("Variable 'v500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "t500"))) {
        stop("Variable 't500' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "t700"))) {
        stop("Variable 't700' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "t850"))) {
        stop("Variable 't850' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "slp"))) {
        stop("Variable 'slp' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "q700"))) {
        stop("Variable 'q700' in 'obs' parameter is missed.")
      } else if (!(any(names(obs) %in% "tm2m"))) {
        stop("Variable 'tm2m' in 'obs' parameter is missed.")
      }
    }
  }

  if (all((sapply(exp,dim)) == dim(exp[[1]]))) {
    dim_exp <- dim(exp[[1]])
    if (!(any(names(dim_exp) %in% dim_name_longitude))) { 
      stop("Dimension 'lon' in exp parameter is missed.")
    }
    if (!(any(names(dim_exp) %in% dim_name_latitude))) { 
      stop("Dimension 'lat' in exp parameter is missed.")
    }
    if (!(any(names(dim_exp) %in% dim_name_time))) { 
      stop("Dimension 'time' in exp parameter is missed.")
    }
  } else {
    stop("All 'exp' variables must have the same dimensions.")
  }

  dim_slp <- dim(slp)
  if (!(any(names(dim_slp) %in% dim_name_longitude))) {   
    stop("Dimension 'lon' in slp parameter is missed.")
  }
  if (!(any(names(dim_slp) %in% dim_name_latitude))) {        
    stop("Dimension 'lat' in slp parameter is missed.")
  }
  if (!(any(names(dim_slp) %in% dim_name_time))) {
    stop("Dimension 'time' in slp parameter is missed.")
  }

  if (all((sapply(obs,dim))==dim(obs[[1]]))) {
    dim_obs <- dim(obs[[1]])
    if (!(any(names(dim_obs) %in% dim_name_longitude))) {
      stop("Dimension 'lon' in obs parameter is missed.")
    }
    if (!(any(names(dim_obs) %in% dim_name_latitude))) {
      stop("Dimension 'lat' in obs parameter is missed.")
    } 
    if (!(any(names(dim_obs) %in% dim_name_time))) {
      stop("Dimension 'time' in obs parameter is missed.")
    }
  } else {
    stop("All 'obs' variables must have the same dimensions.")
  }

  if (!is.vector(lon) || !is.numeric(lon)) {
    stop("Parameter 'lon' must be a numeric vector")
  } else {
    if (is.unsorted(lon)) {
      lon <- sort(lon)
      warning("'lon' vector has been sorted in increasing order")
    }
  }

  if (!is.vector(lat) || !is.numeric(lat)) {
    stop("Parameter 'lat' must be a numeric vector")
  } else {
    if (!is.unsorted(lat)) {
      lat <- sort(lat, decreasing = TRUE)
      warning("'lat' vector has been sorted in decreasing order")
    }
  }

  if (!is.vector(slp_lon) || !is.numeric(slp_lon)) {
    stop("Parameter 'slp_lon' must be a numeric vector")
  } else {
    if (is.unsorted(slp_lon)) {
      lon <- sort(slp_lon)
      warning("'slp_lon' vector has been sorted in increasing order")
    }
  }

  if (!is.vector(slp_lat) || !is.numeric(slp_lat)) {
    stop("Parameter 'slp_lat' must be a numeric vector")
  } else {
    if (!is.unsorted(slp_lat)) {
      lat <- sort(slp_lat, decreasing = TRUE)
      warning("'slp_lat' vector has been sorted in decreasing order")
    }
  }

  if (!is.character(hr_obs)){
    stop("Parameter 'hr_obs' must be a character.")
  } else {
    if (!dir.exists(hr_obs)) {
      stop("'hr_obs' directory does not exist")
    }
  }

  if (!is.character(tdates)) {
    stop("Parameter 'tdates' must be a character.")
  } else {
    if (nchar(tdates) != "17") {
      stop("Parameter 'tdates' must be a string with 17 charecters.")
    } else {
      dateini <- as.Date(substr(tdates,start = 1, stop = 8), format = "%Y%m%d")
      dateend <- as.Date(substr(tdates,start = 10, stop = 18), format = "%Y%m%d")
      if (dateend <= dateini) {
        stop("Parameter 'tdates' must be at least of one day")
      }
    }
  }

  if (!is.character(ddates)) {
    stop("Parameter 'ddates' must be a character.")
  } else {
    if (nchar(ddates) != "17") {
      stop("Parameter 'ddates' must be a string with 17 charecters.")
    } else {
      dateini <- as.Date(substr(ddates, start = 1, stop = 8), format = "%Y%m%d")
      dateend <- as.Date(substr(ddates, start = 10, stop = 18), format = "%Y%m%d")
      if (dateend <= dateini) {
        stop("Parameter 'ddates' must be at least of one day")
      }
    }
  }

  if (names(dim(exp[[1]]))[1] == "lon" & names(dim(exp[[1]]))[2] == "lat" 
      || names(dim(exp[[1]]))[2] == "lon" & names(dim(exp[[1]]))[3] == "lat") {
      texp2D <- lapply(exp, MergeDims, merge_dims = c('lon', 'lat'), 
                rename_dim = 'gridpoint')
  } else if (names(dim(exp[[1]]))[1] == "lat" & names(dim(exp[[1]]))[2] == "lon"
      || names(dim(exp[[1]]))[2] == "lat" & names(dim(exp[[1]]))[3] == "lon") {
      texp2D <- lapply(exp, MergeDims, merge_dims = c('lat', 'lon'), 
                rename_dim = 'gridpoint')
  }

  if (names(dim(slp))[1] == "lon" & names(dim(slp))[2] == "lat"
      || names(dim(slp))[2] == "lon" & names(dim(slp))[3] == "lat") {
      tslp2D <- MergeDims(slp,merge_dims = c('lon', 'lat'),
                rename_dim = 'gridpoint')
  } else if (names(dim(slp))[1] == "lat" & names(dim(slp))[2] == "lon"
      || names(dim(slp))[2] == "lat" & names(dim(slp))[3] == "lon") {
      tslp2D <- MergeDims(slp,merge_dims = c('lat', 'lon'),
                rename_dim = 'gridpoint')
  }

  if (names(dim(obs[[1]]))[1] == "lon" & names(dim(obs[[1]]))[2] == "lat"
      || names(dim(obs[[1]]))[2] == "lon" & names(dim(obs[[1]]))[3] == "lat") {
      tobs2D <- lapply(obs, MergeDims, merge_dims = c('lon', 'lat'), 
                    rename_dim = 'gridpoint')
  } else if (names(dim(obs[[1]]))[1] == "lat" & names(dim(obs[[1]]))[2] == "lon"
      || names(dim(obs[[1]]))[2] == "lat" & names(dim(obs[[1]]))[3] == "lon") {
      tobs2D <- lapply(obs, MergeDims, merge_dims = c('lat', 'lon'),    
                    rename_dim = 'gridpoint')
  }

  if (names(dim(texp2D[[1]]))[1] == "gridpoint") {
      exp2D <- lapply(texp2D,aperm)
  } else {
      exp2D <- texp2D
  } 

  if (names(dim(tslp2D))[1] == "gridpoint") {
      slp2D <- aperm(tslp2D)
  } else {
      slp2D <- tslp2D
  }

  if (names(dim(tobs2D[[1]]))[1] == "gridpoint") {
      obs2D <- lapply(tobs2D,aperm)
  } else {
      obs2D <- tobs2D
  }

  downres <- .analogspred(exp2D, slp2D, obs2D, lon, lat, slp_lon, slp_lat,
                          var_name, hr_obs, tdates, ddates, restrain)

}

#' Atomic .analogspred function
#'
#'@author Marta Dom\'inguez Alonso - AEMET, \email{mdomingueza@aemet.es}
#'This function works with lists of matrix from reanalysis and seasonal
#'forecast data and uses a Fortran interface (.Fortran) to run an
#'analogs method developed in AEMET. 
#'@param pred_mod List of matrix with downscaled period seasonal forecast data. The list 
#'  has to contain model atmospheric variables (instantaneous 12h data) that must 
#'  be indentify by parenthesis name. For precipitation:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500_mod) in m/s.}
#'    \item{v component of wind at 500 hPa (v500_mod) in m/s.}
#'    \item{temperature at 500 hPa (t500_mod) in K.}
#'    \item{temperature at 850 hPa (t850_mod) in K.}
#'    \item{specific humidity at 700 hPa (q700_mod) in g/kg.}
#'  }
#'  For temperature:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500_mod) in m/s.}
#'    \item{v component of wind at 500 hPa (v500_mod) in m/s.}
#'    \item{temperature at 500 hPa (t500_mod) in K.}
#'    \item{temperature at 700 hPa (t500_mod) in K.}
#'    \item{temperature at 850 hPa (t850_mod) in K.}
#'    \item{specific humidity at 700 hPa (q700_mod) in g/kg.}
#'    \item{2 meters temperature (tm2m_mod) in K.}
#'  }
#'  Seasonal forecast variables must have the same resolution and  
#'  domain as 'pred_rea' parameter. All matrices must have two dimensions with 
#'  names 'time' and 'gridpoint'.
#'@param pred_slp Matrix with atmospheric seasonal forecast model sea level 
#'  pressure (instantaneous 12h data) that must be indentify as 'slp'. It has 
#'  the same resolution as 'pred_mod' paremeter but with an extended domain. 
#'  This domain contains extra degrees (most in the north and west part) compare 
#'  to synoptic domain. The matrix must have two dimensions with names 'time' 
#'  and 'gridpoint'.
#'@param pred_rea List of matrix with training period reanalysis data. The 
#'  list has to contain reanalysis atmospheric variables (instantaneous 12h 
#'  data) that must be indentify by parenthesis name. For precipitation:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500) in m/s.}
#'    \item{v component of wind at 500 hPa (v500) in m/s.}
#'    \item{temperature at 500 hPa (t500) in K.}
#'    \item{temperature at 850 hPa (t850) in K.}
#'    \item{sea level pressure (slp) in hPa.}
#'    \item{specific humidity at 700 hPa (q700) in g/kg.}
#'  }
#'  For maximum and minimum temperature:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500) in m/s.}
#'    \item{v component of wind at 500 hPa (v500) in m/s.}
#'    \item{temperature at 500 hPa (t500) in K.}
#'    \item{temperature at 700 hPa (t500) in K.}
#'    \item{temperature at 850 hPa (t850) in K.}
#'    \item{sea level pressure (slp) in hPa.}
#'    \item{specific humidity at 700 hPa (q700) in g/kg.}
#'    \item{2 meters temperature (tm2m) in K}
#'  } 
#'  All matrices must have two dimensions with names 'ddates' and 'gridpoint'.
#'@param lon Vector of the synoptic longitude (from (-180º) to 180º), 
#'  The vector must go from west to east. 
#'@param lat Vector of the synoptic latitude. The vector must go from north to 
#'  south.
#'@param slp_lon Vector of the extended longitude (from (-180º) to 180º), 
#'  The vector must go from west to east. 
#'@param slp_lat Vector of the extended latitude. The vector must go from north 
#'  to south.
#'@param var Variable name to downscale. There are two options: 'prec' for
#'  precipitation and 'temp' for maximum and minimum temperature.
#'@param HR_path Local path of HR observational files (maestro and pcp/tmx-tmn). 
#'  For precipitation and temperature can be downloaded from the following link:
#'  \url{https://www.aemet.es/en/serviciosclimaticos/cambio_climat/datos_diarios?w=2}  
#'  respetively. Maestro file (maestro_red_hr_SPAIN.txt) has gridpoint (nptos), 
#'  longitude (lon), latitude (lat) and altitude (alt) in columns (vector 
#'  structure). Data file (pcp/tmx/tmn_red_SPAIN_1951-201903.txt) includes 5km 
#'  resolution spanish daily data (precipitation or maximum and minimum 
#'  temperature from january 1951 to march 2019. See README file for more 
#'  information. IMPORTANT!: HR observational period must be the same as for 
#'  reanalysis variables ('pred_rea' parameter). It is assumed that the training 
#'  period is smaller than the HR original one (1951-2019), so it is needed to 
#'  make a new ascii file with the new period and the same structure as original, 
#'  specifying the training dates in the name (e.g. 
#'  'pcp_red_SPAIN_19810101-19961231.txt' for '19810101-19961231' period).  
#'@param tdates Training period dates in format YYYYMMDD(start)-YYYYMMDD(end) 
#'  (e.g. 19810101-20181231). The same as for the training function. 
#'@param ddates Downscaling period dates in format YYYYMMDD(start)-YYYYMMDD(end) 
#'  (e.g. 20191001-20200331).
#'@param restrain Output (list of matrix) obtained from 'training_analogs' 
#'  function. For precipitation, 'restrain' object must contains um, vm, nger, 
#'  gu92, gv92, gu52, gv52, neni, vdmin, vref, ccm, lab_pred and cor_pred 
#'  variables. For maximum and minimum temperature, 'restrain' object must 
#'  contains um, vm, insol, neni, vdmin y vref. See 'AnalogsPred_train.R' for 
#'  more information.
#'@return .analogspred Returns seasonal forecast precipitation (mm) or maximum
#'and minimum temperature (dozens of ºC) in a 5km x 5km regular grid over 
#'peninsular Spain and Balearic Islands. Each matrix of the list has two 
#'dimensions ('ddates' x 'nptos').  
#'
#'@importFrom utils read.table
#'@useDynLib CSTools
#'@noRd
.analogspred <- function(pred_mod, pred_slp, pred_rea, lon, lat, slp_lon, 
                         slp_lat, var, HR_path, tdates, ddates, restrain) {

  if (!is.list(pred_mod)) {
      stop("Parameter 'pred_mod' must be a list of 'matrix' objects")
    }

  if (!(all(sapply(pred_mod, inherits, 'matrix')))) {
      stop("Elements of the list in parameter 'pred_mod' must be of the class ",
            "'matrix'.")
  }

  if (!is.matrix(pred_slp)) {
      stop("Parameter 'pred_slp' must be of the class 'matrix'.")
    }

  if (!is.list(pred_rea)) {
      stop("Parameter 'pred_rea' must be a list of 'matrix' objects")
    }

  if (!(all(sapply(pred_rea, inherits, 'matrix')))) {
      stop("Elements of the list in parameter 'pred_rea' must be of the class ",
            "'matrix'.")
    }

  if (var == "prec") {
    if (length(pred_rea) != 6) {
        stop("Parameter 'pred_rea' must be a length of 6.")
      }
    if (length(pred_mod) != 5) {
        stop("Parameter 'pred_mod' must be a length of 5.")
      }
  } else {
    if (length(pred_rea) != 8) {
        stop("Parameter 'pred_rea' must be a length of 8.")
      }
    if (length(pred_mod) != 7) {
        stop("Parameter 'pred_mod' must be a length of 7.")
      }
  }

  if (!is.vector(lon) || !is.numeric(lon)) {
      stop("Parameter 'lon' must be a numeric vector")
    }

  if (!is.vector(lat) || !is.numeric(lat)) {
      stop("Parameter 'lat' must be a numeric vector")
    }

  if (!is.vector(slp_lon) || !is.numeric(slp_lon)) {
      stop("Parameter 'slp_lon' must be a numeric vector")
    }

  if (!is.vector(slp_lat) || !is.numeric(slp_lat)) {
      stop("Parameter 'slp_lat' must be a numeric vector")
    }

  if (!is.character(HR_path)){
      stop("Parameter 'HR_path' must be a character.")
    }

  if (!is.character(tdates)) {
    stop("Parameter 'tdates' must be a character.")
    }

  if (!is.character(ddates)) {
    stop("Parameter 'ddates' must be a character.")
    }

  if (!is.list(restrain)) {
      stop("Parameter 'restrain' must be a list of 'matrix' and 'parameter' objects")
    }

  #! REANALYSIS GRID PARAMETERS

    rlon <- c(lon, NA) - c(NA, lon)
    rlon <- rlon[!is.na(rlon)]
    if (!all(rlon == rlon[1])) {
      stop("Parameter 'lon' must be in regular grid.")
    } else {
      rlon <- rlon[1]
    }

    rlat <- c(lat, NA) - c(NA, lat)
    rlat <- rlat[!is.na(rlat)]
    if (!all(rlat == rlat[1])) {
      stop("Parameter 'lat' must be in regular grid.")
    } else {
      rlat <- rlat[1]
    }

    if (rlon != (-rlat)) {
      stop("Parameters 'lon' and 'lat' must have the same resolution.")
    } else {
      res <- rlon
    }

    nlat <- ((lat[length(lat)] - lat[1]) / rlat) + 1
    nlon <- ((lon[length(lon)] - lon[1]) / rlon) + 1

    ic <- nlat * nlon
  #
    slp_rlon <- c(slp_lon, NA) - c(NA, slp_lon)
    slp_rlon <- slp_rlon[!is.na(slp_rlon)]
    if (!all(slp_rlon == slp_rlon[1])) {
      stop("Parameter 'slp_lon' must be in regular grid.")
    } else {
      slp_rlon <- slp_rlon[1]
    }

    slp_rlat <- c(slp_lat, NA) - c(NA, slp_lat)
    slp_rlat <- slp_rlat[!is.na(slp_rlat)]
    if (!all(slp_rlat == slp_rlat[1])) {
      stop("Parameter 'slp_lat' must be in regular grid.")
    } else {
      slp_rlat <- slp_rlat[1]
    }

    if (slp_rlon != (-slp_rlat)) {
      stop("Parameters 'slp_lon' and 'slp_lat' must have the same resolution.")
    } else {
      slp_res <- slp_rlon
    }

    nlatt <- ((slp_lat[length(slp_lat)] - slp_lat[1]) / slp_rlat) + 1  
    nlont <- ((slp_lon[length(slp_lon)] - slp_lon[1]) / slp_rlon) + 1

    id <- nlatt * nlont    

    slat <- max(lat)
    slon <- min(c(lon[which(lon > 180)] - 360,
                  lon[which(lon <= 180)]))

    slatt <- max(slp_lat)
    slont <- min(c(slp_lon[which(slp_lon > 180)] - 360,
                  slp_lon[which(slp_lon <= 180)]))

    ngridd <- ((2*nlatt)-1)*((2*nlont)-1)

    if (all((sapply(pred_rea,nrow))==nrow(pred_rea[[1]]))){
      nd <- nrow(pred_rea[[1]])
    } else {
      stop("All 'pred_rea' variables must have the same period.")
    }

      if (all((sapply(pred_mod,nrow))==nrow(pred_mod[[1]]))){
      nm <- nrow(pred_mod[[1]])
    } else {
      stop("All 'pred_mod' variables must have the same period.")
    }

      seqdates <- seq(as.Date(substr(ddates,start=1,stop=8),format="%Y%m%d"),as.Date(substr(ddates,start=10,stop=18),format="%Y%m%d"),by="days")
      month <- format(seqdates,format="%m")
      day <- format(seqdates,format="%d")

  #! TRAINING REANALYSIS VARIABLES
  u500 <- pred_rea[['u500']]
  v500 <- pred_rea[['v500']]
  t500 <- pred_rea[['t500']]
  t850 <- pred_rea[['t850']]
  msl_si <- pred_rea[['slp']]
  q700 <- pred_rea[['q700']]

  if (var == "temp") {
  t700 <- pred_rea[['t700']]
  tm2m <- pred_rea[['tm2m']]
  }

  #! SEASONAL FORECAST MODEL VARIABLES
  u500_mod <- pred_mod[['u500_mod']]
  v500_mod <- pred_mod[['v500_mod']]
  t500_mod <- pred_mod[['t500_mod']]
  t850_mod <- pred_mod[['t850_mod']]
  msl_lr_mod <- pred_slp
  q700_mod <- pred_mod[['q700_mod']]

  if (var == "temp") {
  t700_mod <- pred_mod[['t700_mod']]
  tm2m_mod <- pred_mod[['tm2m_mod']]
  }

  #! HIGH-RESOLUTION (HR) OBSERVATIONAL DATASET
  maestro_hr_file <- paste(HR_path, "maestro_red_hr_SPAIN.txt",sep="")
  if (!file.exists(maestro_hr_file)) {
      stop("'maestro_red_hr_SPAIN.txt' does not exist.")
  } else {
      maestro <- read.table(maestro_hr_file)
      lon_hr <- unlist(maestro[2])
      lat_hr <- unlist(maestro[3])
      nptos <- length(readLines(maestro_hr_file))
  }

  if (var == "prec") {
    prec_hr_file <- paste(HR_path, "pcp_red_SPAIN_",tdates,".txt",sep="")
    if (!file.exists(prec_hr_file)) {
      stop(sprintf("precipitation HR file for %s does not exist.",tdates))
    } else {
      nd_hr <- length(readLines(prec_hr_file))
      preprec_hr <- matrix(scan(prec_hr_file), nrow=nd_hr ,ncol= nptos+1, byrow=TRUE)
      prec_hr <- preprec_hr[1:nd_hr,-c(1)]
    }
  } else {
    tmx_hr_file <- paste(HR_path, "tmx_red_SPAIN_",tdates,".txt",sep="")
    tmn_hr_file <- paste(HR_path, "tmn_red_SPAIN_",tdates,".txt",sep="")
    if (!file.exists(tmx_hr_file)) {
      stop(sprintf("maximum temperature HR file for %s does not exist.",tdates))
    } else if (!file.exists(tmn_hr_file)) {
      stop(sprintf("minimum temperature HR file for %s does not exist.",tdates))
    } else if (length(readLines(tmx_hr_file)) != length(readLines(tmn_hr_file))) {
      stop("maximum and minimum temperature HR observation files must have the same period.")
    } else {
      nd_hr <- length(readLines(tmx_hr_file))
      pretmx_hr <- matrix(scan(tmx_hr_file), nrow=nd_hr ,ncol= nptos+1, byrow=TRUE)
      tmx_hr <- pretmx_hr[1:nd_hr,-c(1)]
      pretmn_hr <- matrix(scan(tmn_hr_file), nrow=nd_hr ,ncol= nptos+1, byrow=TRUE)
      tmn_hr <- pretmn_hr[1:nd_hr,-c(1)]
    }
  }

    if (nd_hr != nd) {
      stop("Reanalysis variables and HR observations must have the same period.")
    }

  #! OTHER PARAMETERS that should not be changed
  #! Number of analog situations to consider 
  nanx <- 155
  #! Number of temperature predictors
  nvar <- 7

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (var == "prec") {

      downs <- .Fortran("down_prec",
                        ic = as.integer(ic),
                        id = as.integer(id),
                        nd = as.integer(nd),
                        nm = as.integer(nm),
                        nlat = as.integer(nlat),
                        nlon = as.integer(nlon),
                        nlatt = as.integer(nlatt),
                        nlont = as.integer(nlont),
                        slat = as.numeric(slat),
                        slon = as.numeric(slon),
                        rlat = as.numeric(rlat),
                        rlon = as.numeric(rlon),
                        slatt = as.numeric(slatt),
                        slont = as.numeric(slont),
                        ngridd = as.integer(ngridd),
                        u500 = as.numeric(u500),
                        v500 = as.numeric(v500),
                        t500 = as.numeric(t500),
                        t850 = as.numeric(t850),
                        msl_si = as.numeric(msl_si),
                        q700 = as.numeric(q700),
                        prec_hr = as.numeric(prec_hr),
                        nanx = as.integer(nanx),
                        restrain$um,
                        restrain$vm,
                        restrain$nger,
                        restrain$gu92,
                        restrain$gv92,
                        restrain$gu52,
                        restrain$gv52,
                        restrain$neni,
                        restrain$vdmin,
                        restrain$vref,
                        restrain$ccm,
                        restrain$indices[,,,1],#lab_pred
                        restrain$indices[,,,2],#cor_pred 
                        u500_mod = as.numeric(u500_mod),
                        v500_mod = as.numeric(v500_mod),
                        t500_mod = as.numeric(t500_mod),
                        t850_mod = as.numeric(t850_mod),
                        msl_lr_mod = as.numeric(msl_lr_mod),
                        q700_mod = as.numeric(q700_mod),
                        pp=matrix(as.double(seq(1,nm*nptos)),c(nm,nptos)),
                        PACKAGE = 'CSTools')

              output <- downs$pp

  } else {

      downs <- .Fortran("down_temp",
                        ic = as.integer(ic),
                        id = as.integer(id),
                        nd = as.integer(nd),
                        nm = as.integer(nm),
                        nlat = as.integer(nlat),
                        nlon = as.integer(nlon),
                        nlatt = as.integer(nlatt),
                        nlont = as.integer(nlont),
                        slat = as.numeric(slat),
                        slon = as.numeric(slon),
                        rlat = as.numeric(rlat),
                        rlon = as.numeric(rlon),
                        slatt = as.numeric(slatt),
                        slont = as.numeric(slont),
                        ngridd = as.integer(ngridd),
                        u500 = as.numeric(u500),
                        v500 = as.numeric(v500),
                        t500 = as.numeric(t500),
                        t850 = as.numeric(t850),
                        msl_si = as.numeric(msl_si),
                        q700 = as.numeric(q700),
                        t700 = as.numeric(t700),
                        tm2m = as.numeric(tm2m),
                        tmx_hr = as.numeric(tmx_hr),
                        tmn_hr = as.numeric(tmn_hr),
                        nanx = as.integer(nanx),
                        nvar = as.integer(nvar),
                        day = as.integer(day),
                        month = as.integer(month),
                        restrain$um,
                        restrain$vm,
                        restrain$insol,
                        restrain$neni,
                        restrain$vdmin,
                        restrain$vref,
                        u500_mod = as.numeric(u500_mod),
                        v500_mod = as.numeric(v500_mod),
                        t500_mod = as.numeric(t500_mod),
                        t850_mod = as.numeric(t850_mod),
                        msl_lr_mod = as.numeric(msl_lr_mod),
                        q700_mod = as.numeric(q700_mod),
                        t700_mod = as.numeric(t700_mod),
                        tm2m_mod = as.numeric(tm2m_mod),
                        tmx=matrix(as.double(seq(1,nm*nptos)),c(nm,nptos)),
                        tmn=matrix(as.double(seq(1,nm*nptos)),c(nm,nptos)),
                        PACKAGE = 'CSTools')

              output <- list("tmax" = downs$tmx,
                              "tmin" = downs$tmn)

}
             return(output)
}


