v.bathy <- get.bathy <- function(v_area,lon,lat,resolution=4, keep=F, savename.bathy, folder.bathy,visualize=T,terrain=F,...#steps,levels,
){
  if(missing(lon) | missing(lat)){
    
    if(!missing(v_area)){
      if(grepl('Raster', class(v_area)) | grepl('Extent', class(v_area))){
        ext <- as.vector(t(sp::bbox(raster::extent(v_area))))
        lon <- ext[1:2]
        lat <- ext[3:4]
        v_area <- bathy.area <- paste0('lon',lon[1],'-',lon[2],'.lat',lat[1],'-',lat[2])
      }else{
        r <- regions(v_area)
        lon <- r$xlim
        lat <- r$ylim
        bathy.area <- v_area
      }
    }
  }
  if(missing(lon) | missing(lat)) stop('geographical reference missing! please revise!')
  if(missing(v_area)) {
    bathy.area <- paste0('lon',lon[1],'-',lon[2],'.lat',lat[1],'-',lat[2])
  }else{
    bathy.area <- v_area
  }
  if(missing(folder.bathy)) folder.bathy <- getwd()
  
  if(missing(savename.bathy)){
    savename.bathy <- paste0('bathy_',bathy.area,'_res.',resolution,'min.dat')
  }else{
    if(!grepl(".dat", savename.bathy)) savename.bathy <- paste0(savename.bathy,'.dat')
  }
  folder.bathy <- .check.folder(folder.bathy)
  savename <- paste0(folder.bathy,savename.bathy)
  
  if(file.exists(savename)){
    load(savename)
    if(!terrain){
      ii <- which(h[,] < 0)
      if(length(ii) > 0){
        h[h[,] > 0] <- NA
        h[,] <- -h[,]
      }
    }
    
  }else{
    cat('loading bathymetry data at a resolution of',resolution ,"minutes\n")
#     bathy <- getNOAA.bathy(lon1 = min(lon), lon2 = max(lon), lat1 = min(lat), lat2 = max(lat),
#                            resolution = resolution)   
    
    pos <- .getNOAA.bathy(lon1 = min(lon), 
                          lon2 = max(lon), 
                          lat1 = min(lat), 
                          lat2 = max(lat),
                          resolution = resolution)

    ext <- raster::extent(c(lon),lat)
    dd <- raster::raster(ext, ncol=length(unique(pos[,1])), nrow=length(unique(pos[,2])))
    if(!terrain){
      pos$V3[pos$V3 > 0] <- NA
      pos$V3 <- -1*pos$V3
    }
    h <- raster::rasterize(pos[,1:2], dd, pos[,3], fun=mean)
#     h <- bathy
#     h <- t(h[])[ncol(h):1,]
#     if(!terrain){
#       h[h > 0] <- NA
#       h <- -1*h
#     }
#     h <- raster(h)
#     raster::extent(h) <- raster::extent(c(lon,lat))
    raster::projection(h) <- "+proj=longlat"
  }
  if(keep) {
    save(h, file=savename)
    cat(paste0("\nsaving bathymetry as: '",savename.bathy, "' in folder: '", folder.bathy, "'\n")) # display files to print
  }
  
  if(visualize) v(h,param='bathy',...)
  return(h)
}
