#' @title get_rec
#'
#' @author Johannes De Groeve
#' 
#' @description naming of remaining reconstructed polygons based on when they appear for the first time
#' 
#' @param x prepared object including standarized input datasets
#' @param iso vector of altitudes to distinguish, default 0 (coastlines)
#' @param noise maximum number of pixels considered as noise 
#' @param noiserm boolean, whether noise should be removed
#' @param fillholes boolean, fill the holes in polygons, independent from noise (e.g. lakes)
#' @param reclabs if NULL the island labeling dataset is used, otherwise another column from the region object could be used, or a feature from the geonames feature list could be specified 
#' @param aggregate boolean, aggregate polygons if TRUE
#' @param verbose boolean, print messages if true
#' 
#' @return SpatVector
#' 
#' @noRd
#' @keywords internal
#'
#'
get_rec<-function(x=NULL, iso=0, reclabs=NULL, noise=5, noiserm=TRUE, fillholes=NULL,aggregate=FALSE, verbose=FALSE){
  
  # datasets
  topo <- x$topo
  names(topo) <- 'topo'
  labs <- x$labs
  curve<- x$curve
  correction <- x$correction   
  
  #### 2. create reconstructed vectors and rasters ####
  paleov<-list()
  paleor<-list()
  if(as.numeric(verbose) == 1) message('2. prepare recvect recrast')
  if(as.numeric(verbose) == 1) message('2.1. create polygons')
  if(as.numeric(verbose) == 1) {pb <- pbar(max=length(names(curve)))}
  
  #### periods ####
  periods <- names(curve)
  periods_name <- ifelse(grepl('-',periods),
                         paste0('BP',sprintf("%07d", abs(as.numeric(periods)))),
                         paste0('AP',sprintf("%07d", abs(as.numeric(periods)))))

  for(i in 1:length(names(curve))){
    # select period
    #period <- periods[i]
    
    # sea curve filter  
    if( class(curve)[1] == "SpatRaster"){
      sea_level <- curve[[i]]
    } else {
      sea_level <- as.vector(unlist(curve[i]))
    }
    # correction grid filter
    if( class(correction)[1] == "SpatRaster"){
      correctionm <- correction[[i]]
    } else {
      if(length(names(correction)) == length(names(curve))){
      correctionm <- as.vector(unlist(correction[i]))
      } else {
      correctionm <- as.vector(unlist(correction))
      }
    }
    
    
    # identify pixels above the curve accounting for correction, per iso
    if(is.list(iso)){
      sign <- unlist(iso[!unlist(lapply(iso,is.numeric))])
      iso <- unlist(iso[unlist(lapply(iso,is.numeric))])
    } else {
      sign <- '>='
    }
    iso <- sort(iso, decreasing =TRUE)
    aslr <- lapply(1:length(iso), function(x) {
          #aslr2 <- eval(parse(paste0('topo < (sea_level + correctionm + 10000) & topo ',sign,' (sea_level + correctionm + ',iso[x],')')))
          if(sign==">=") aslr2 <- topo < (sea_level + correctionm + 10000) & topo >= (sea_level + correctionm + iso[x]) # 10000m because no altitude is higher than 10000m
          if(sign==">") aslr2 <- topo < (sea_level + correctionm + 10000) & topo > (sea_level + correctionm + iso[x]) #
          return(aslr2)
    })
    
    aslr <- terra::rast(aslr)
    aslr <- terra::app(aslr,sum)
    names(aslr) <- 'iso'
    terra::NAflag(aslr) <- 0
    # define levels for different iso's
    if(length(iso) > 1){
      aslr <- (aslr - 1)
      cls <- data.frame(id=1:length(iso)-1, iso=rev(iso))
    } else {
      cls <- data.frame(id=1:length(iso), iso=rev(iso))
    } 
    levels(aslr) <- cls
    # terra::NAflag(aslr)<-0
    
    # identify island peaks and add back to aslv
    aslv <- add_peaks(topo,vec=aslr_to_aslv(aslr,fillholes=fillholes),reference=FALSE,verbose=verbose)
    aslv <- suppressMessages(terra::makeValid(aslv))
    
    # check if points intersect with the labs land. If not we assume they are noise and can be removed
    aslv$intersects <- terra::relate(terra::vect(as.data.frame(aslv[,c('x','y')]),geom=c('x','y'),crs=crs(aslv)),
                                     aggregate(labs),
                                     'intersects')
    
    # filter small polygon features to remove noise
    if(noiserm){ # check in get_recnames if small islands are added correctly when they are kept
      aslv <- subset(aslv,aslv$n > noise | aslv$intersects) # & aslv$intersects
    }
    
    if(aggregate){ # check if names is not null
      # since everything is aggregated we want to calculate area and n for each iso 
      # but we want to use the same coordinates throughout the reconstruction for each class 
      # this allows to keep the names constant throughout the reconstruction dataset
      aslv_aggregate <- terra::aggregate(aslv,by='iso')[,'iso']
      aslv_aggregate$unique_id <- 1
      suman <- terra::aggregate(aslv,by='iso',fun='sum')
      aslv_aggregate$area <- suman$sum_area
      z_max <- max(aslv$z)
      aslv_aggregate$x <- aslv[which(aslv$z==z_max),]$x[1]
      aslv_aggregate$y <- aslv[which(aslv$z==z_max),]$y[1]
      aslv_aggregate$z <- z_max
      aslv_aggregate$n <- suman$sum_n
      aslv <- aslv_aggregate
      aslv$intersects <- T # I think we need true but I am not sure
      
      #aslv$recname <- paste0('S-',.data$period[1],'-',.data$unique_id[1])
      #aslv$recid <- paste0(strrep("9", 12 - nchar(paste0(gsub('[A-Z][A-Z]','',.data$period[1]),.data$unique_id[1]))), paste0(gsub('[A-Z][A-Z]','',.data$period[1]),.data$unique_id[1]))  
      #,'recid','recname','recnames','refnames'
      
      
    }
    paleov[[i]] <- aslv
    
    # add period and mean sea level position 
    paleov[[i]]$period <- periods_name[i]
    if(class(sea_level)[1] == 'SpatRaster'){
      paleov[[i]]$curve <- mean(values(curve[[i]]))
    } else {
      paleov[[i]]$curve <- as.vector(unlist(curve[i]))
    }
    if(class(correctionm)[1] == 'SpatRaster'){
      paleov[[i]]$correction <- mean(values(correctionm),na.rm=TRUE)
    } else {
      paleov[[i]]$correction <- as.vector(unlist(correctionm))
    }
    # after filling holes and treating the noise it is necessary to rasterize again so the paleor and paleov are exactly the same
    paleor[[i]] <- terra::rasterize(merge(paleov[[i]],cls, all.x=TRUE, by='iso'),aslr,field='id') 
    levels(paleor[[i]]) <- cls
    if(as.numeric(verbose) == 1) {update_pbar(pb, i)}
    
  }
  paleor <- rast(paleor)
  names(paleor) <- periods_name
  names(paleov) <- periods_name
  if(as.numeric(verbose) == 1) {close(pb)}
  rec <- list(recvect=paleov,recrast=paleor)
  return(rec)
}

