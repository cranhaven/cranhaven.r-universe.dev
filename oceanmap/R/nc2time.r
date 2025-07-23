nc2time <- function(nc,varname){
  # script adapted from netCDFtoRasterCD.R
  # returns netcdf-time entries as strings
  if(extends(class(nc), 'character')){
   nc <- nc_open(nc) 
  }
  dodays <- TRUE
  dohours <- FALSE
  doseconds <- FALSE
  
  if(missing(varname)) varname <- "time"
  
  time <- ncvar_get(nc,varname)
  un <- ncatt_get(nc, varname)$units
  
  if (substr(un, 1, 10) == "days since") { 
    startDate = as.Date(substr(un, 12, 22))
  } else {
    dodays <- FALSE
    if (substr(un, 1, 11) == "hours since") { 
      dohours <- TRUE
    } else { 
      if (substr(un, 1, 13) == "seconds since") { 
        doseconds <- T
      }
    }
  }
  if (doseconds) {
    startTime0 <- substr(un, 15, 33)
    startTime <- strptime(startTime0, "%Y-%m-%d %H:%M:%OS")
    if(is.na(startTime)) startTime <- strptime(startTime0, "%Y-%m-%dT%H:%M:%S")
    time <- startTime + as.numeric(time)
#     time <- as.character(time)
  }
  if (dohours) {
    startTime <- substr(un, 13, 30)
    startTime <- strptime(startTime, "%Y-%m-%d %H:%M:%OS")
    time <- startTime + as.numeric(time) * 3600
#     time <- as.character(time)
  }
  if (dodays) {
    cal <- ncatt_get(nc, varname)$calendar
    if(is.null(cal)){
      greg <- TRUE
    }else{
      if (cal =='gregorian' | cal =='proleptic_gregorian' | cal =='standard') {
        greg <- TRUE
      } else if (cal == 'noleap' | cal == '365 day' | cal == '365_day') { 
        greg <- FALSE
      } else {
        greg <- TRUE
        #warning('assuming a standard calender')
      }
    }
    
    if (greg) {
      time <- as.Date(time, origin=startDate)
    } else {
      a <- as.numeric(time)/365
      year <- trunc(a)
      doy <- (time - (year * 365))
      time <- as.Date(doy, origin=paste(year, "-1-1", sep='')) - 1
    }
  }
  return(time)
}