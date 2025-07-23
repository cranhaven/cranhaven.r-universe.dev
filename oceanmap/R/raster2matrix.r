raster2matrix <- function(RasterLayer){
  if(dim(RasterLayer)[3] <= 1){
    A <- raster::as.matrix(RasterLayer)
    m <- t(A[nrow(A):1,])
  }else{
    m <- aperm(raster::as.array(flip(RasterLayer,direction=2)),c(2,1,3))
  }
  return(m)
}

raster2array <- function(RasterLayer){
  m <- raster2matrix(RasterLayer)
  return(m)
}