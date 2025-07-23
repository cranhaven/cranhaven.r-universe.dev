get.avg <- function(x, radius, unit="km", raster){
  get.avg.bathy(x=x, radius=radius, unit=unit, raster=raster)
}

get.avg.bathy <- function(x, radius, unit="km", raster, bathy, v_area="medw4"){
  
  note <- 'bathymetry'
  if(!missing(raster)) {bathy <- raster
  note <- "value"}
  if(missing(bathy)) bathy <- get.bathy(v_area,resolution = 1, folder.bathy = "/home/robert/Dropbox/Ifremer/data/batymetry/",terrain = F,visualize = F)
  if(min(bathy[,],na.rm=T) < 0 & max(bathy[,]) > 0){
    bathy[which(bathy[,] > 0)] <- NA  
    bathy[,] <- -1*bathy[,]
  }
  oradius <- radius
  if(unit == "km"){
    cat("radius provided in 'km', assuming raster field/bathymetry is in degrees.\n")
    radius <- radius/111.32
  }
  
  out <- c()
  if(!is.null(dim(x))){
    n <- nrow(x)
    for(i in 1:n){
      # perc.done(i,n,suffix = paste())
      spc <- SpatialCircle(x[i,1],x[i,2],r = radius)
      bathy.avg <- mean(extract(bathy,spc)[[1]],na.rm=T)
      out <- c(out, bathy.avg)
    }
  }else{
    spc <- SpatialCircle(x[1],x[2],r = radius)
    bathy.avg <- mean(extract(bathy,spc)[[1]],na.rm=T)
    out <- c(out, bathy.avg)
  }
  cat('returning average ',note, ' in a radius of ', round(oradius,1), unit,' around provided point(s)!\n')
  return(out)
}
