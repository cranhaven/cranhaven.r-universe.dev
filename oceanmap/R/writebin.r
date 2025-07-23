writebin <- function(satdata,filename,folder,param){
  region_definitions <- NULL
  rm(region_definitions)
  data('region_definitions',envir=environment())
  size=1
  A <- satdata
  if(!is(A, 'matrix')) A <- raster2matrix(A)
  area <- name_split(filename)$area
  if(any(is.na(region_definitions[region_definitions$label == area,7:9]))){    
    add.px <- region_definitions[region_definitions$label == area,c(1,7:9)]
    add.px$ncol <- ncol(satdata)
    add.px$nrow <- nrow(satdata)
    add.px$px <- add.px$ncol*add.px$nrow
    add.region(add.px=add.px)
  }
#   if(class(A) != "matrix")  A <- t(as.matrix(A)[dim(A)[1]:1,])
#A <- as.matrix(A)
  if(!missing(param)){A <- param_unconvert(A,param)}
  A[is.nan(A)] <- 254 # set landmask
  A[is.na(A)] <- 255 # set clouds
  A <- as.vector(A[,ncol(A):1],mode="integer")
  
  if(!missing(folder)){
    folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
    filename <- paste0(folder,filename)
  }
  confile <- gzfile(filename,"wb") #  Open for writing in binary mode
  writeBin(A,confile,size)
  close(confile)
}

