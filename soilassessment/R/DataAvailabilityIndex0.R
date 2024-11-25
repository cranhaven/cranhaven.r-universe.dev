DataAvailabilityIndex=function(Boundary, Scale, CP,Data){
  if(is(Boundary, 'SpatialPolygonsDataFrame')){
    Boundary=as(Boundary,"SpatialPolygons")
  }

  else if (is(Boundary,"data.frame")){
    if(nrow(Boundary)>3){
    p = Polygon(Boundary)
    ps = Polygons(list(p),1)
    Boundary = SpatialPolygons(list(ps))
    Boundary=st_as_sfc(Boundary)
    Boundary= st_sfc(Boundary, crs=CP)
    }
    else{
      pol = st_polygon(
        list(
          cbind(
            Boundary[,1][c(1,2,2,1,1)],
            Boundary[,2][c(1,1,2,2,1)])
        )
      )
      Boundary = st_sfc(pol, crs=CP)
    }
  }
  else {Boundary=Boundary
  if(is.na(crs(Boundary))) {
    crs(Boundary) = CP}
  }
  Boundary=st_transform(Boundary,4326)
  if(Scale<=0.1){stop("Selected assessment area is too small and must be greater than zero")}

  Boundary=as(Boundary,"Spatial")
  crs(Boundary)=CP
  ext <-  extent(Boundary)
  xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
  if(round(xy[1]/(Scale/111),0)<=1){stop("selected assessment area is too large")}
  w <- raster(ext, ncol=round(xy[1]/(Scale/111),0), nrow=round(xy[2]/(Scale/111),0))
  ww <-rasterize(Boundary, w)
  Data=st_as_sfc(Data,crs=CP)
  Data = st_transform(Data,4326)
  Data=as(Data,"Spatial")
  #Data=spTransform(Data,CRS("+init=epsg:4326"))
  pts_in<-Data[!is.na(over(Data,Boundary)),]

  pointcount = function(r, pts){
    b2 = r
    b2[] = 0
    counts = table(cellFromXY(r,pts))
    b2[as.numeric(names(counts))] = counts
    return(b2)
  }

  w2=pointcount(ww,pts_in)
  proj4string(w2)=CRS("+init=epsg:4326")
  return(w2)
}
