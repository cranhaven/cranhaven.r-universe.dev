nc2raster <- function(nc,varname,t=layer,layer,verbose=FALSE){
  #   #inst.pkg('ncdf4')
  #   #inst.pkg('raster')
  #   cat(paste("editing file:",ncfile\n))
  if(!is(nc, 'ncdf4')){
    nc <- nc_open(nc) # open netcdf file
  }
  if(verbose) print(nc) # print netcdf information, like ncdump
  
  if(missing(varname)) varname <- c() 
  #   if(missing(varname)) varname <- "Conc"
  if(!is(varname, 'character')){
    if(length(names(nc$var)) == 1){ 
      varname <- names(nc$var)[1]
      warning(paste('varname is missing or not of type character! \nSelected:', varname))
    }else{
      stop(paste('error in nc2raster: varname is missing or not of type character! \nPlease choose one of the available variables:\n',
                 paste(names(nc$var),collapse=", ")))
    }
  }
  z <- ncvar_get(nc,varname)
  date <- T
  if(length(dim(z)) > 2){
    if(missing(layer)){
      if(!missing(t)){
        layer <- t
      }else{
        layer <- 1:dim(z)[3]
      }
    }else{
      z <- z[,,layer]
    }
  }else{
    layer <- 1
    if(!any(grepl('time', nc))) date <- F
  }
  
  
  if(is.logical(date)){
    if(date & any(grepl('time', nc))){
      time <- nc2time(nc)[layer] # autoload time information, accoording on netcdf-standards
      dates <- format(time, '%Y%m%d%H')
    }else{
      dates <- rep(NA,max(c(1,dim(z)[3]),na.rm=T))
    }
  }  
  
  
  dimnames <- unlist(lapply(nc$var[[varname]]$dim, function(x) { return(x$name)}))

  lon <- as.vector(ncvar_get(nc,dimnames[1])) # fillvalues are automatically replaced by NA
  lat <- as.vector(ncvar_get(nc,dimnames[2])) # fillvalues are automatically replaced by NA

  z.raster <- matrix2raster(z,x=lon,y=lat)
  if(!is.na(dates)[1]) names(z.raster) <- dates
  #   z.raster <- brick(z.raster)
  return(z.raster)
}
