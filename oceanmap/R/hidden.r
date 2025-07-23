.getNOAA.bathy <- function(lon1, lon2, lat1, lat2, resolution = 4, keep=FALSE, antimeridian=FALSE)   {
  x1=x2=y1=y2 = NULL
  if (lon1 == lon2) 
    stop("The longitudinal range defined by lon1 and lon2 is incorrect")
  if (lat1 == lat2) 
    stop("The latitudinal range defined by lat1 and lat2 is incorrect")
  if (lat1 > 90 | lat1 < -90 | lat2 > 90 | lat2 < -90) 
    stop("Latitudes should have values between -90 and +90")
  if (lon1 < -180 | lon1 > 180 | lon2 < -180 | lon2 > 180) 
    stop("Longitudes should have values between -180 and +180")
  if (resolution < 1) 
    stop("The resolution must be equal to or greater than 1")
  
  if(lon1 < lon2) {
    lon1 -> x1
    lon2 -> x2
  }else{
    lon1 -> x2
    lon2 -> x1
  }
  
  if(lat1 < lat2){
    lat1 -> y1
    lat2 -> y2
  }else{
    lat1 -> y2
    lat2 -> y1
    }
  
  x1 <- round(x1, 1)
  x2 <- round(x2, 1)
  y1 <- round(y1, 1)
  y2 <- round(y2, 1)
  ncell.lon <- (x2 - x1) * 60/resolution
  ncell.lat <- (y2 - y1) * 60/resolution
  
  if (ncell.lon < 2 & ncell.lat < 2) 
    stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal and longitudinal ranges or the resolution (i.e. use a smaller res value)")
  if (ncell.lon < 2) 
    stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal range or the resolution (i.e. use a smaller resolution value)")
  if (ncell.lat < 2) 
    stop("It's impossible to fetch an area with less than one cell. Either increase the latitudinal range or the resolution (i.e. use a smaller resolution value)")
    
  fetch <- function(x1,y1,x2,y2,dbresolution) {
    if (dbresolution < 0.5) {
      dbresolution <- 0.25
    } else {
      if (dbresolution < 1) {
        dbresolution <- 0.5
      }
    }
    if (dbresolution == 0.25) database <- "27ETOPO_2022_v1_15s_bed_elev"
    if (dbresolution == 0.50) database <- "27ETOPO_2022_v1_30s_bed"
    if (dbresolution  > 0.50) database <- "27ETOPO_2022_v1_60s_bed"
    
    ncell.lon <- floor(ncell.lon)
    ncell.lat <- floor(ncell.lat)
    WEB.REQUEST <- paste0("https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/ImageServer/exportImage?bbox=", x1, ",", y1, ",", x2, ",", y2, 
                          "&bboxSR=4326&size=", ncell.lon, ",", ncell.lat,
                          "&imageSR=4326&format=tiff&pixelType=F32&interpolation=+RSP_NearestNeighbor&compression=LZ77&renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%", 
                          database, "%27%22}&f=image")
    download.file(url = WEB.REQUEST, destfile = "tmp.tif", mode = "wb")
    dat <- suppressWarnings(try(raster::raster("tmp.tif"), silent = TRUE))
    
    dat <- .as.xyz(.as.bathy(dat))
    return(dat)
  }
  
  # Naming the file
  # if (antimeridian) {
  #   FILE <- paste("marmap_coord_",x1,";",y1,";",x2,";",y2,"_res_",resolution,"_anti",".csv", sep="")
  # } else {
  #   FILE <- paste("marmap_coord_",x1,";",y1,";",x2,";",y2,"_res_",resolution,".csv", sep="")
  # }
  
#   # If file exists in the working directory, load it,
#   if(FILE %in% list.files() ) {
#     cat("File already exists ; loading \'", FILE,"\'", sep="")
#     marmap::read.bathy(FILE, header=T) -> exisiting.bathy
#     return(exisiting.bathy)
#   } else { # otherwise, fetch it on NOAA server
    
#     if (antimeridian) {
#       
#       l1 <- x2 ; l2 <- 180 ; l3 <- -180 ; l4 <- x1
#       
#       cat("Querying NOAA database ...\n")
#       cat("This may take seconds to minutes, depending on grid size\n")
#       left <- fetch(l1,y1,l2,y2,res)
#       right <- fetch(l3,y1,l4,y2,res)
#       
#       if (is(left,"try-error")|is(right,"try-error")) {
#         stop("The NOAA server cannot be reached\n")
#       } else {
#         cat("Building bathy matrix ...\n")  
#         left <- marmap::as.bathy(left) ; left <- left[-nrow(left),]
#         right <- marmap::as.bathy(right)
#         rownames(right) <- as.numeric(rownames(right)) + 360
#         bath2 <- rbind(left,right)
#         class(bath2) <- "bathy"
#         bath <- marmap::as.xyz(bath2)
#       }
#       
#     } else {
      
      cat("Querying NOAA database ...\n")
      cat("This may take seconds to minutes, depending on grid size\n")
      bath <- fetch(x1,y1,x2,y2,dbresolution=resolution)
      
      if (is(bath,"try-error")) {
        stop("The NOAA server cannot be reached\n")
#       } else {
#         cat("Building bathy matrix ...\n")  
#         bath2 <- try(marmap::as.bathy(bath),silent = T)
#         if(class(bath2) == "try-error") stop("The conversion of bathymetry data failed. Please try a different resolution!")
      }
#     }
    
#     if (keep) {
#       write.table(bath, file=FILE, sep=",", quote=FALSE, row.names=FALSE)
#     }
#     
    return(bath)
#   }
}


.as.xyz <- function(bathy){
  if(!is(bathy, 'bathy')) stop("Objet is not of class bathy")
  lon <- as.numeric(rownames(bathy))
  lat <- as.numeric(colnames(bathy))
  xyz <- data.frame(expand.grid(lon, lat), as.vector(bathy))
  xyz <- xyz[order(xyz[, 2], decreasing = TRUE), ]
  names(xyz) <- c("V1", "V2", "V3")
  rownames(xyz) <- 1:nrow(xyz)
  return(xyz)
}

.as.bathy <- function(x){
  if (is(x, "bathy")) 
    stop("Object is already of class 'bathy'")
  if (is(x, "SpatialGridDataFrame")) 
    x <- raster::raster(x)
  if (is(x, "RasterLayer")) {
    lat.min <- x@extent@xmin
    lat.max <- x@extent@xmax
    lon.min <- x@extent@ymin
    lon.max <- x@extent@ymax
    nlat <- x@ncols
    nlon <- x@nrows
    lon <- seq(lon.min, lon.max, length.out = nlon)
    lat <- seq(lat.min, lat.max, length.out = nlat)
    bathy <- t(raster::as.matrix(raster::flip(x, direction = "y")))
    colnames(bathy) <- lon
    rownames(bathy) <- lat
  }
  if (ncol(x) == 3 & !exists("bathy", inherits = FALSE)) {
    bath <- x
    bath <- bath[order(bath[, 2], bath[, 1], decreasing = FALSE), 
    ]
    lat <- unique(bath[, 2])
    bcol <- length(lat)
    lon <- unique(bath[, 1])
    brow <- length(lon)
    if ((bcol * brow) == nrow(bath)) {
      bathy <- matrix(bath[, 3], nrow = brow, ncol = bcol, 
                      byrow = FALSE, dimnames = list(lon, lat))
    }
    else {
      colnames(bath) <- paste("V", 1:3, sep = "")
      bathy <- acast(bath, V1 ~ V2, value.var = "V3")
    }
  }
  if (!exists("bathy", inherits = FALSE)) 
    stop("as.bathy requires a 3-column table, or an object of class RasterLayer or SpatialDataFrame")
  ordered.mat <- .check.bathy(bathy)
  class(ordered.mat) <- "bathy"
  return(ordered.mat)
}

.check.bathy <- function(x){
  xc <- order(as.numeric(colnames(x)))
  xr <- order(as.numeric(rownames(x)))
  sorted.x <- x[xr, xc]
  return(sorted.x)
}