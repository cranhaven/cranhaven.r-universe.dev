#' AEMET Training   
#' Training method (pre-downscaling) based on analogs: 
#' synoptic situations and significant predictors. 
#'
#'@author Marta Dominguez Alonso - AEMET, \email{mdomingueza@aemet.es}
#'@author Nuria Perez-Zanon - BSC, \email{nuria.perez@bsc.es}
#'
#'@description This function caracterizes the synoptic situations in a past 
#'period based on low resolution reanalysis data (e.g, ERAInterim 1.5º x 1.5º) 
#'and an observational high resolution (HR) dataset (AEMET 5 km gridded daily 
#'precipitation and maximum and minimum temperature) (Peral et al., 2017)). 
#'The method uses three domains:
#'\itemize{
#'  \item{peninsular Spain and Balearic Islands domain (5 km resolution): HR domain}
#'  \item{synoptic domain (low resolution): it should be centered over Iberian   
#'        Peninsula and cover enough extension to detect as much synoptic 
#'        situations as possible.}
#'  \item{extended domain (low resolution): it is an extension of the synoptic 
#'        domain. It is used for 'slp_ext' parameter (see 'slp_lon' and 'slp_lat' 
#'        below).}
#'}
#'@param pred List of matrix reanalysis data in a synoptic domain. The list has 
#'  to contain reanalysis atmospheric variables (instantaneous 12h data) that 
#'  must be indentify by parenthesis name. For precipitation:
#'  \itemize{
#'    \item{u component of wind at 500 hPa (u500) in m/s}
#'    \item{v component of wind at 500 hPa (v500) in m/s}
#'    \item{temperature at 500 hPa (t500) in K}
#'    \item{temperature at 850 hPa (t850) in K}
#'    \item{temperature at 1000 hPa (t1000) in K}
#'    \item{geopotential height at 500 hPa (z500) in m}
#'    \item{geopotential height at 1000 hPa (z1000) in m}
#'    \item{sea level pressure (slp) in hPa}
#'    \item{specific humidity at 700 hPa (q700) in g/kg}
#'  }
#'  For maximum and minimum temperature:
#'  \itemize{
#'    \item{temperature at 1000 hPa (t1000) in K}
#'    \item{sea level pressure (slp) in hPa}
#'  }
#'  All matrix must have [time,gridpoint] dimensions.
#'  (time = number of training days, gridpoint = number of synoptic gridpoints). 
#'@param slp_ext Matrix with atmospheric reanalysis sea level pressure
#'  (instantaneous 12h data)(hPa). It has the same resolution as 'pred' parameter 
#'  but with an extended domain. This domain contains extra degrees (most in the 
#'  north and west part) compare to synoptic domain. The matrix must have 
#'  [time,gridpoint] dimensions. (time = number of training days, 
#'  gridpoint = number of extended gridpoints). 
#'@param lon Vector of the synoptic longitude (from (-180º) to 180º), 
#'  The vector must go from west to east. 
#'@param lat Vector of the synoptic latitude. The vector must go from north to 
#'  south.
#'@param slp_lon Vector of the extended longitude (from (-180º) to 180º).
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
#'  temperature from january 1951 to june 2020. See README file for more 
#'  information. IMPORTANT!: HR observational period must be the same as for 
#'  reanalysis variables. It is assumed that the training period is smaller than 
#'  the HR original one (1951-2020), so it is needed to make a new ascii file 
#'  with the new period and the same structure as original, specifying the 
#'  training dates ('tdates' parameter) in the name (e.g. 
#'  'pcp_red_SPAIN_19810101-19961231.txt' for '19810101-19961231' period).  
#'@param tdates Training period dates in format YYYYMMDD(start)-YYYYMMDD(end) 
#'  (e.g. 19810101-19961231).
#'@return A matrix list (e.g. restrain) as a result of characterize the past 
#'synoptic situations and the significant predictors needed to downscale 
#'seasonal forecast variables. For precipitation the output includes:
#'\itemize{
#'   \item{'um': u component of geostrophic wind in all period (numeric matrix 
#'         with [time, gridpoint] dimensions).}
#'   \item{'vm': v component of geostrophic wind in all period (numeric matrix 
#'         with [time,gridpoint] dimensions).}
#'   \item{'nger': number of synoptic situations (integer).}
#'   \item{'gu92': u component of geostrophic wind for each synoptic situation 
#'         (numeric matrix with [nger,gridpoint] dimensions).}
#'   \item{'gv92': v component of geostrophic wind for each synoptic situation 
#'         (numeric matrix with [nger, gridpoint] dimensions).}
#'   \item{'gu52': u component of wind at 500 hPa for each synotic situation 
#'         (numeric matrix with [nger, gridpoint] dimensions).}
#'   \item{'gv52': v component of wind at 500 hPa for each synotic situation 
#'         (numeric matrix with [nger, gridpoint] dimensions).}
#'   \item{'neni': number of reference centers where predictors are calculated 
#'         (integer).}
#'   \item{'vdmin': minimum distances between each HR gridpoint and the four 
#'         nearest synoptic gridpoints (numeric matrix with [nptos,4] dimensions) 
#'         (nptos = number of HR gridpoints).}
#'   \item{'vref': four nearest synoptic gridpoints to each HR gridpoint (integer 
#'         matrix with [nptos, 4] dimensions).}
#'   \item{'ccm': multiple correlation coeficients (numeric matrix with [nger, nptos] 
#'         dimensions) indices:
#'     \itemize{
#'       \item{'lab_pred': numeric labels of selected predictors (integer matrix 
#'             with [nger,nptos,11,1] dimensions).}
#'       \item{'cor_pred': partial correlation of selected predictors (numeric 
#'             matrix with [nger,nptos,11,2] dimensions).}
#'     }
#'   }
#' }
#'For maximum and minimum temperature the output includes:
#'\itemize{
#'  \item{'um': u component of geostrophic wind in all training period (numeric 
#'        matrix with [time,gridpoint] dimensions).}
#'  \item{'vm': v component of geostrophic wind in all training period (numeric 
#'        matrix with [time,gridpoint] dimensions).}
#'  \item{'insol': insolation in all training period (numeric vector with [time] 
#'        dimension).}
#'  \item{'neni': number of reference centers where predictors are calculated 
#'        (integer).}
#'  \item{'vdmin': minimum distances between each HR gridpoint and the four 
#'        nearest synoptic gridpoints (numeric matrix with [nptos,4] dimensions) 
#'        (nptos = number of HR gridpoints).}
#'  \item{'vref': four nearest synoptic gridpoints to each HR gridpoint (integer 
#'        matrix with [nptos,4] dimensions).}
#'}
#'The output can directly use as argument to 'CST_AnalogsPredictors' function 
#'(e.g. resdowns <- CST_AnalogsPredictors(...,restrain)).
#'@importFrom utils read.table
#'@useDynLib CSTools
#'@export 
training_analogs <- function(pred, slp_ext, lon, lat, slp_lon, slp_lat, var, 
                             HR_path, tdates) {

  if (!is.list(pred)) {
    stop("Parameter 'pred' must be a list of 'matrix' objects")
  }

  if (!(all(sapply(pred, inherits, 'matrix')))) {
     stop("Elements of the list in parameter 'pred' must be of the class ",
          "'matrix'.")
  }

  if (var == "prec") {
    if (length(pred) != 9) {
      stop("Parameter 'pred' must be a length of 9.")
    } else {
      if (is.null(names(dim(pred[[1]]))) ||
          is.null(names(dim(pred[[2]]))) ||
          is.null(names(dim(pred[[3]]))) ||
          is.null(names(dim(pred[[4]]))) ||
          is.null(names(dim(pred[[5]]))) ||
          is.null(names(dim(pred[[6]]))) ||
          is.null(names(dim(pred[[7]]))) ||
          is.null(names(dim(pred[[8]]))) ||
          is.null(names(dim(pred[[9]])))) {
        stop("Parameter 'pred' should have dimmension names.")
      }
      if (!(any(names(pred) %in% "u500"))) {
        stop("Variable 'u500' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "v500"))) {
        stop("Variable 'v500' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "t500"))) {
        stop("Variable 't500' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "t850"))) {
        stop("Variable 't850' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "t1000"))) {
        stop("Variable 't1000' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "z500"))) {
        stop("Variable 'z500' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "z1000"))) {
        stop("Variable 'z1000' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "slp"))) {
        stop("Variable 'slp' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "q700"))) {
        stop("Variable 'q700' in pred parameter is missed.")
      }
    }
  } else { 
    if (length(pred) != 2) {
      stop("Parameter 'pred' must be a length of 2.")
    } else {
      if (is.null(names(dim(pred[[1]]))) ||
          is.null(names(dim(pred[[2]])))) {
        stop("Parameter 'pred' should have dimmension names.")
      }
      if (!(any(names(pred) %in% "t1000"))) {
        stop("Variable 't1000' in pred parameter is missed.")
      } else if (!(any(names(pred) %in% "slp"))) {
        stop("Variable 'slp' in pred parameter is missed.")
      }
    }
  }

  if (all((sapply(pred,dim)) == dim(pred[[1]])) & 
      all((sapply(pred, function(pred){names(dim(pred))})) == names(dim(pred[[1]])))) {
      dim_pred <- dim(pred[[1]])
    if (!(any(names(dim_pred) %in% "time"))) {
      stop("Dimension 'time' in pred parameter is missed.")
    }
    if (!(any(names(dim_pred) %in% "gridpoint"))) {
      stop("Dimension 'gridpoint' in pred parameter is missed.")
    }
    if (names(dim_pred)[1] == "gridpoint") {
      pred <- lapply(pred,aperm)
    } else {
      pred <- pred
    }
  } else {
    stop("All 'pred' variables must have the same dimensions and name dimensions.")
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

  if (!is.character(HR_path)) {
    stop("Parameter 'HR_path' must be a character.")
  } else {
    if (!dir.exists(HR_path)) {
      stop("'HR_path' directory does not exist")
    }
  }

  if (!is.character(tdates)) {
    stop("Parameter 'tdates' must be a character.")
  } else {
    if (nchar(tdates) != "17") {
      stop("Parameter 'tdates' must be a string with 17 charecters.")
    } else {
      dateini <- as.Date(substr(tdates,start=1,stop=8),format="%Y%m%d") 
      dateend <- as.Date(substr(tdates,start=10,stop=18),format="%Y%m%d") 
      if (dateend <= dateini) {
        stop("Parameter 'tdates' must be at least of one day")
      }
    }
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

  if (all((sapply(pred,nrow))==nrow(pred[[1]]))){
    nd <- nrow(pred[[1]]) 
  } else {
    stop("All 'pred' variables must be in the same period.")
  }

  #!!!!! COMPROBAR QUE SLP TAMBIEN TIENE EL MISMO NROW

    seqdates <- seq(as.Date(substr(tdates,start=1,stop=8),format="%Y%m%d"),
                    as.Date(substr(tdates,start=10,stop=18),format="%Y%m%d"),by="days")
    month <- format(seqdates,format="%m")
    day <- format(seqdates,format="%d")

  #! TRAINING REANALYSIS VARIABLES
  t1000 <- pred[['t1000']]
  msl_si <- pred[['slp']]
  msl_lr <- slp_ext

  if (var == "prec") {
    u500 <- pred[['u500']]
    v500 <- pred[['v500']]
    t500 <- pred[['t500']]
    t850 <- pred[['t850']]
    z500 <- pred[['z500']]
    z1000 <- pred[['z1000']]
    q700 <- pred[['q700']]
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
  #! Number of predictors
  npx <- 11

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (var == "prec") {

    prePro <- .Fortran("training_part1_prec", 
                       u500 = as.numeric(u500),
                       v500 = as.numeric(v500),
                       t1000 = as.numeric(t1000),
                       z500 = as.numeric(z500),
                       z1000 = as.numeric(z1000),
                       msl_si = as.numeric(msl_si),
                       msl_lr = as.numeric(msl_lr),
                       ngridd = as.integer(ngridd),
                       nlat = as.integer(nlat),
                       nlon = as.integer(nlon),
                       ic = as.integer(ic),
                       nlatt = as.integer(nlatt),
                       nlont = as.integer(nlont),
                       id = as.integer(id),
                       slat = as.numeric(slat),
                       slon = as.numeric(slon),
                       rlat = as.numeric(rlat),
                       rlon = as.numeric(rlon),
                       slatt = as.numeric(slatt),
                       slont = as.numeric(slont),
                       nd = as.integer(nd),
                       um = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       vm = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       gu92 = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       gv92 = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       gu52 = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       gv52 = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       nger = as.integer(1),
                       PACKAGE = 'CSTools') 

    a <- prePro$um
    b <- prePro$vm
    c <- prePro$gu92[1:prePro$nger,]
    d <- prePro$gv92[1:prePro$nger,]
    e <- prePro$gu52[1:prePro$nger,]
    f <- prePro$gv52[1:prePro$nger,]

    g <- prePro$nger

    predSig <- .Fortran("training_part2_prec",
                        u500 = as.numeric(u500),
                        v500 = as.numeric(v500),
                        t500 = as.numeric(t500),
                        t850 = as.numeric(t850),
                        msl_si = as.numeric(msl_si),
                        q700 = as.numeric(q700),
                        lon_hr = as.numeric(lon_hr),
                        lat_hr = as.numeric(lat_hr),
                        prec_hr = as.numeric(prec_hr),
                        nanx = as.integer(nanx),
                        nlat = as.integer(nlat),
                        nlon = as.integer(nlon),
                        ic = as.integer(ic),
                        nlatt = as.integer(nlatt),
                        nlont = as.integer(nlont),
                        id = as.integer(id),
                        slat = as.numeric(slat),
                        slon = as.numeric(slon),
                        rlat = as.numeric(rlat),
                        rlon = as.numeric(rlon),
                        slatt = as.numeric(slatt),
                        slont = as.numeric(slont),
                        nd = as.integer(nd),
                        um = as.double(a),
                        vm = as.double(b),
                        gu92 = as.double(c),
                        gv92 = as.double(d),
                        gu52 = as.double(e),
                        gv52 = as.double(f),
                        nger = as.integer(g),
                        vdmin = matrix(as.double(seq(1,nptos*4)),c(nptos,4)),
                        vref = matrix(as.integer(seq(1,nptos*4)),c(nptos,4)),  
                        neni = as.integer(1),
                        mi = matrix(as.integer(seq(1,prePro$nger*nptos)),c(prePro$nger,nptos)),
                        ccm = matrix(as.double(seq(1,prePro$nger*nptos)),c(prePro$nger,nptos)),
                        lab_pred = matrix(as.integer(seq(1,prePro$nger*nptos*npx)),c(prePro$nger,nptos,npx)),
                        cor_pred = matrix(as.double(seq(1,prePro$nger*nptos*npx)),c(prePro$nger,nptos,npx)), 
                        PACKAGE = 'CSTools')

    h <- predSig$mi
    i <- predSig$ccm
    j <- predSig$lab_pred
    k <- predSig$cor_pred
    l <- predSig$vdmin
    m <- predSig$vref  

    indices <- array(c(j,k),c(g,nptos,npx,2))
    dimnames(indices)[[4]] <- c("lab_pred","cor_pred")

    output <- list("um" = a,
                   "vm" = b,
                   "nger" = g,
                   "gu92" = c,
                   "gv92" = d,
                   "gu52" = e,
                   "gv52" = f,
                   "neni" = predSig$neni, 
                   "vdmin" = l,
                   "vref" = m, 
                   "ccm" = i,
                   "indices" = indices)
  } else {

    prePro <- .Fortran("training_temp", 
                       t1000 = as.numeric(t1000),
                       msl_si = as.numeric(msl_si),
                       msl_lr = as.numeric(msl_lr),
                       lon_hr = as.numeric(lon_hr),
                       lat_hr = as.numeric(lat_hr),
                       ngridd = as.integer(ngridd),
                       nlat = as.integer(nlat),
                       nlon = as.integer(nlon),
                       ic = as.integer(ic),
                       nlatt = as.integer(nlatt),
                       nlont = as.integer(nlont),
                       id = as.integer(id),
                       slat = as.numeric(slat),
                       slon = as.numeric(slon),
                       rlat = as.numeric(rlat),
                       rlon = as.numeric(rlon),
                       slatt = as.numeric(slatt),
                       slont = as.numeric(slont),
                       nd = as.integer(nd),
                       day = as.integer(day),
                       month = as.integer(month),
                       um = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       vm = matrix(as.double(seq(1,nd*ic)),c(nd,ic)),
                       insol = vector(mode="double",length=nd),
                       vdmin = matrix(as.double(seq(1,nptos*4)),c(nptos,4)),
                       vref = matrix(as.integer(seq(1,nptos*4)),c(nptos,4)),  
                       neni = as.integer(1),
                       PACKAGE = 'CSTools') 

    a <- prePro$um
    b <- prePro$vm
    c <- prePro$insol
    d <- prePro$vdmin
    e <- prePro$vref
    f <- prePro$neni

    output <- list("um" = a,
                   "vm" = b,
                   "insol" = c,
                   "vdmin" = d,
                   "vref" = e, 
                   "neni" = f) 
            
  }

  return(output)

}
                                 
