get_geopos <- function(x, xlim, ylim, date_format, lang_format="en", tz="UTC", proj4string, prob_lim=.5, verbose=TRUE){
  file <- x
  if(!file.exists(x)) stop(paste("The file", file, "does not exist in the current working directory. Please revise!"))
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  if (missing(proj4string)) proj4string <- sp::CRS(as.character(NA))
  if(verbose) cat(paste0("Loading tagging tracks from file: ",file,"\n"))
  if(substr(file,nchar(file)-2,nchar(file)) == "csv"){
    #### check for header line:
    skip <- -1
    header_found <- F
    while(!header_found){
      skip <- skip +1
      header0 <- as.character(unlist(read.delim(file,sep=",",header=F,nrows=1,skip=skip,stringsAsFactors=F)))
      header_found <- any(grepl("Most.Likely",header0))
    }
    
    info_raw <- readLines(file,n = 3)
    taginfo <- data.frame(file=file, Serial=NA, speed=NA, score=NA, stringsAsFactors = F)
    # if(grepl('PTT ',info_raw))){
    #   taginfo$DeployID <- taginfo$Ptt <- strsplit(strsplit(info_raw,"PTT ")[[1]][2],",")[[1]][1]
    # }
    if(any(grepl('Serial ',info_raw))){
      taginfo$Serial <- strsplit(strsplit(info_raw,"Serial ")[[1]][2],",")[[1]][1]
    }
    if(any(grepl('@ ',info_raw))){
      taginfo$speed <- strsplit(strsplit(info_raw,"@ ")[[2]][2],",")[[1]][1]
    }
    if(any(grepl(' Model Score ',info_raw))){
      taginfo$score <- strsplit(strsplit(info_raw," Model Score ")[[2]][2],"\\. ")[[1]][1]
      taginfo$score <- gsub('\"',"",taginfo$score)
    }
    
    pos <- read.csv(file, header=T,sep=',', skip=skip,stringsAsFactors = F)
    pos <- cbind(pos,taginfo)
    # head(pos)
    names(pos) <- gsub('Most.Likely.', '', names(pos))
    names(pos) <- gsub('gitude', '', names(pos))
    names(pos) <- gsub('itude', '', names(pos))
    pos$datetime <- .fact2datetime(pos$Date, date_format = date_format, 
                                   lang_format = lang_format, tz = tz)
    pos$date <- as.Date(pos$datetime)
    pos$Date <- c()
    out <- pos
  }
  
  if(substr(file,nchar(file)-1,nchar(file)) == "nc"){
    
    nc <- ncdf4::nc_open(file)
    #     print(nc)
    datetime <- .date2datetime("1970-01-01",tz="UTC",midday=F)+ncdf4::ncvar_get(nc,"twelve_hour_timestamps") # seconds since 1970-1-1
    lons <- ncdf4::ncvar_get(nc, "longitude")
    lats <- ncdf4::ncvar_get(nc, "latitude")
    
    # if(add) {
    #   xlim <- par()$usr[1:2]
    #   ylim <- par()$usr[3:4]
    # }else{
    #   #         if(!missing(v_area)){
    #   #           r <- regions(v_area)
    #   #           xlim <- r$xlim
    #   #           ylim <- r$ylim
    #   #         }else{
    #   if(missing(xlim)) xlim <- range(lons)
    #   if(missing(ylim)) ylim <- range(lats)
    #   #         }
    # }
    if(missing(xlim)) xlim <- range(lons)
    if(missing(ylim)) ylim <- range(lats)
    
    Boundaries <- raster::extent(c(xlim, ylim)) #creates a bounding box to include all of the different gridded area sizes
    #       if(!add) oceanmap::plotmap(Boundaries) 
    
    #### load polygons:
    i <- j <- 1
    pols <- c()
    for(i in 1:length(datetime)){
      if(verbose) print(datetime[i])
      tmp <- invisible(capture.output(Raster.LR0 <- raster::raster(file,varname = "twelve_hour_likelihoods",band = i)))
      Raster.LR <- raster::extend(Raster.LR0, Boundaries) #then extends any of your surfaces with the set boundaries
      #You can then use stack() to stack multiple tags, and overlay() to merge them together into a single probability surface.
      #To interpolate a surface (resample it at a higher resolution):
      Raster.big <- raster::raster(ncol=1200,nrow=1200,ext=Boundaries) #creates the higher resolution grid
      Raster.HR <- raster::resample(x=Raster.LR,y=Raster.big,method="bilinear") #will resample the data object onto the higher resolution grid
      Raster.HR@data@values <- Raster.HR@data@values/sum(Raster.HR@data@values,na.rm = T) #normalize the grid values so they sum to 1
      
      RasterVals <- sort(Raster.HR@data@values) #sort the probability values
      Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=(1-prob_lim)))])
      cl <- try(raster::rasterToContour(Raster.HR,levels = Raster.breaks),silent = T)
      
      cl0 <- cl
      if(!is(cl, 'try-error')){
        
        ## new approach
        if(length(cl@lines) > 1 | length(cl@lines[[1]]@Lines) > 1) warning("polygon conversion incomplete, please contact package author")
        spolys <- SpatialPolygons(list(Polygons(list(Polygon(cl@lines[[1]]@Lines[[1]])),ID =  as.character(datetime[i]))),
                                  proj4string = CRS(proj4string(cl)))
        
        ## old approach with maptools
        # p <- maptools::SpatialLines2PolySet(cl)
        # spolys <- maptools::PolySet2SpatialPolygons(p)
        # spolys@polygons[[1]]@ID <- as.character(datetime[i])
        
        if(is.null(pols)){
          pols <- spolys
        }else{
          pols@polygons[[j]] <- spolys@polygons[[1]]
        }
        j <- j +1
      }
    }
    
    # pols@plotOrder <- spolys@plotOrder
    # rgeos::gBuffer(spTransform(pols, CRS(paste(proj4string(pols)))),0,byid = F)
    # which(!.clgeo_CollectionReport(pols)$valid)
    # 
    pols_joined <- .check_and_fill_holes(pols)
    sp::proj4string(pols_joined) <- sp::proj4string(pols)
    
    info_raw <- ncdf4::ncatt_get(nc,0)$comments
    taginfo <- data.frame(file=file, DeployID=NA, Ptt=NA, Serial=NA, speed=NA, score=NA, stringsAsFactors = F)
    if(grepl('PTT ',info_raw)){
      taginfo$DeployID <- taginfo$Ptt <- strsplit(strsplit(info_raw,"PTT ")[[1]][2],",")[[1]][1]
    }
    if(grepl('Serial ',info_raw)){
      taginfo$Serial <- strsplit(strsplit(info_raw,"Serial ")[[1]][2],",")[[1]][1]
    }
    if(grepl('@ ',info_raw)){
      taginfo$speed <- strsplit(strsplit(info_raw,"@ ")[[1]][3],",")[[1]][1]
    }
    if(grepl(' Model Score ',info_raw)){
      taginfo$score <- strsplit(strsplit(info_raw," Model Score ")[[1]][2],"\\. ")[[1]][1]
    }
    
    add <- data.frame(file=file,prob_lim=prob_lim,datetime=datetime,
                      xmin=xlim[1],xmax=xlim[2],ymin=ylim[1],ymax=ylim[2],stringsAsFactors = F)
    df <- cbind(taginfo, add)
    out <- pols_df <- SpatialPolygonsDataFrame(Sr=pols_joined, data=df, match.ID = FALSE)
    out@plotOrder <- 1:nrow(out)
  }
  
  if(substr(file,nchar(file)-2,nchar(file)) %in% c("kml","kmz")){
    pl <- .getKMLpols(kmlfile=file,verbose = verbose)
    LikelihoodArea <- prob_lim*100
    if(!(prob_lim %in% c(.99, .95, .5))) stop("Invalid 'porb_lim' value. Please select one of the following values: 0.99, 0.95, 0.50")
    out <- .merge_pols(pl, LikelihoodArea=LikelihoodArea, date_format=date_format, lang_format=lang_format, tz=tz, proj4string = proj4string, xlim=xlim, ylim=ylim)
    out@plotOrder <- 1:nrow(out)
  }
  
  return(out)
}

.check_and_fill_holes <- function(x){
  ## fill potential holes:
  report <- cleangeo::clgeo_CollectionReport(x)
  issues <- length(which(report$valid == FALSE))
  npols <- length(x)
  if(issues >= 1){
    for(i in 1:issues){
      # stop("holes found")
      new_report <- cleangeo::clgeo_CollectionReport(x)
      j <- which(new_report$valid == FALSE)[1]
      add <- try(buffer(x[j,],0),silent = TRUE)
      proj4string(add) <- proj4string(x)
      x0 <- rbind(x[1:(j-1),], spChFIDs(add,names(x)[j]))
      if(j+1 <= npols) x0 <- rbind(x0, x[(j+1):npols,])
      x <- x0
    }
  }
  return(x)
}
# 
# .clgeo_CollectionReport	 <- function(spo){
#   clgeo_report <- as.data.frame(do.call("rbind", lapply(1:length(spo), 
#                                                         function(x) {
#                                                           report <- unlist(.clgeo_GeometryReport(spo[x, ]))
#                                                         })), stringsAsFactors = FALSE)
#   clgeo_report$valid <- as(clgeo_report$valid, "logical")
#   clgeo_report$type <- as.factor(clgeo_report$type)
#   clgeo_report$issue_type <- as.factor(clgeo_report$issue_type)
#   return(clgeo_report)
# }
# 
# .clgeo_GeometryReport <- function(spgeom){ 
#   
#   clgeo_report <- list(type = NA, valid = FALSE, issue_type = NA, 
#                        error_msg = NA, warning_msg = NA)
#   report <- tryCatch({
#     isvalid <- rgeos::gIsValid(spgeom)
#     if (isvalid) 
#       clgeo_report$valid <- TRUE
#     return(clgeo_report)
#   }, warning = function(w) {
#     clgeo_report$type <- "rgeos_validity"
#     clgeo_report$valid <- FALSE
#     if (regexpr("at or near point", conditionMessage(w), 
#                 "match.length", ignore.case = TRUE) > 1) 
#       clgeo_report$issue_type = "GEOM_VALIDITY"
#     clgeo_report$warning_msg <- conditionMessage(w)
#     return(clgeo_report)
#   }, error = function(e) {
#     clgeo_report$type <- "rgeos_error"
#     clgeo_report$valid <- FALSE
#     if (regexpr("orphaned hole", conditionMessage(e), "match.length", 
#                 ignore.case = TRUE) > 1) 
#       clgeo_report$issue_type = "ORPHANED_HOLE"
#     clgeo_report$error_msg = conditionMessage(e)
#     return(clgeo_report)
#   })
#   return(report)
# }


.getKMLpols <- function(kmlfile, ignoreAltitude=TRUE,verbose){
  if (missing(kmlfile))  stop("kmlfile is missing")
  
  
  if(substr(start = (nchar(kmlfile)-3),stop = nchar(kmlfile),kmlfile) ==".kmz"){
    kmzfile <- kmlfile
    if (grepl(" ", kmzfile) & Sys.info()['sysname'] != "Windows") {
      kmzfile <- gsub(" ","\\\\ ",kmlfile)
    }
    
    kmzfile <- kmlfile
    zipfile <- gsub(".kmz",".zip",kmzfile)
    exdir <- gsub(".kmz","",kmzfile)
    
    file.copy(kmzfile,zipfile)
    unzip(zipfile,exdir=exdir)
    tmpfile <- Sys.glob(paste0(exdir,"/*.kml"))
    kmlfile <- gsub(".kmz",".kml",kmzfile)
    
    file.copy(tmpfile, kmlfile)
    unlink(exdir, recursive = TRUE)
    file.remove(zipfile)
    # system(paste("mv",gsub(" ","\\\\ ",tmpfile),gsub(" ","\\\\ ",kmlfile)))
    # system(paste("rm -r", gsub(" ","\\\\ ",exdir)))
    # system(paste("rm -r", gsub(" ","\\\\ ",zipfile)))
    # 
    if(verbose) cat("extracted kml-file from provided kmz-file\n")
  }
  
  kml0 <- readLines(kmlfile,warn=F)
  istart <- grep("Time Lapse",kml0)
  iend <- grep("Maximum Likelihood",kml0)
  iend <- iend[which(iend > istart)][1]
  kml <- kml0[istart:iend]
  idates <- grep('Data name="time"',kml)
  
  n <- length(idates)
  idates <- c(idates,length(kml))
  out <- list()
  for(ii in 1:n){
    ptype <- as.numeric(strsplit(gsub("</styleUrl>","",kml[(idates[ii]-2)]),"#contour-")[[1]][[2]])+1
    ltype <- paste(c("99%","95%","50%")[ptype], "Likelihood Areas")
    
    dd <- strsplit(gsub("</value>","",kml[idates[ii]+1]),"<value>")[[1]][2]
    sub <- kml[idates[ii]:(idates[ii+1]-1)]
    pols_start <- grep("<coordinates>",sub)
    pols_end <- grep("</coordinates>",sub)
    
    
    nj <- length(pols_start); j <- 1
    while(j <= nj){
      poltype <- c("inner","outer")[grepl("outer",sub[(pols_start[j]-2)])+1]
      
      sub2 <- sub[pols_start[j]:(pols_end[j]-1)]
      sub2[1] <- strsplit(sub2[1],"<coordinates>")[[1]][2]
      coords <- read.table(textConnection(sub2),sep = ",")
      if(ignoreAltitude) coords[[3]] <- c()
      add <- coords
      out[[ltype]][[dd]][[poltype]] <- add
      j <- j+1
    }
  }
  
  itaginfo <- grep('<description><!',kml0)
  info_raw <- kml0[itaginfo]
  out$info <- data.frame(file=kmlfile,DeployID=NA, Ptt=NA, Serial=NA, speed=NA, score=NA,stringsAsFactors = F)
  if(grepl('PTT ',info_raw)){
    out$info$DeployID <- out$info$Ptt <- strsplit(strsplit(info_raw,"PTT ")[[1]][2],",")[[1]][1]
  }
  if(grepl('Serial ',info_raw)){
    out$info$Serial <- strsplit(strsplit(info_raw,"Serial ")[[1]][2],",")[[1]][1]
  }
  if(grepl('@ ',info_raw)){
    out$info$speed <- strsplit(strsplit(info_raw,"@ ")[[1]][3],",")[[1]][1]
  }
  if(grepl(' Model Score ',info_raw)){
    out$info$score <- strsplit(strsplit(info_raw," Model Score ")[[1]][2],"<")[[1]][1]
  }
  
  return(out)
}



.merge_pols <- function(pl, LikelihoodArea=95, date_format = "%d-%b-%Y %H:%M:%S",lang_format="en",tz="UTC", proj4string, xlim, ylim){
  out <- pl
  file <- out$info$file; out$file <- c()
  ltype <- paste0(LikelihoodArea,"% Likelihood Areas")
  valid_ltypes <- gsub("% Likelihood Areas","",names(out))
  if(!ltype %in% names(out)) stop("Please select one of the following valid Likelihood Areas: ",paste(valid_ltypes,collapse=", "))
  n <- length(out[[ltype]])
  
  if (missing(proj4string)) proj4string <- sp::CRS(as.character(NA))
  pols <- c()
  if(missing(xlim)) xlim <- c()
  if(missing(ylim)) ylim <- c()
  
  for(i in 1:n){
    dd <- names(out[[ltype]])[i]
    coords <- out[[ltype]][[dd]][["outer"]]
    spolys <- SpatialPolygons(list(Polygons(list(Polygon(coords, hole=as.logical(NA))), ID=paste("outer",dd))), proj4string=proj4string)
    
    if(is.null(pols)){
      pols <- spolys
    }else{
      pols@polygons[[i]] <- spolys@polygons[[1]]
    }
    
    xlim <- range(c(xlim,coords[,1]))
    ylim <- range(c(ylim,coords[,2]))
  }
  pols_joined <- .check_and_fill_holes(pols)

  datetime <- .fact2datetime(names(out[[ltype]]),date_format = date_format, lang_format = lang_format, tz = tz)
  
  df <- out$info
  add <- data.frame(prob_lim=LikelihoodArea/100,datetime=datetime,
                    xmin=xlim[1],xmax=xlim[2],ymin=ylim[1],ymax=ylim[2],stringsAsFactors = F)
  df <- cbind(df,add)
  out2 <- pols_df <- SpatialPolygonsDataFrame(Sr=pols_joined, data=df,FALSE)
  return(out2)
}
