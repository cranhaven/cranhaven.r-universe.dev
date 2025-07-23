region2SpatialPolygon <- function(region,v_area=region){
  r <- regions(v_area)
  x <- r$xlim
  y <- r$ylim
  plot(x,y)
  p <- sp::Polygon(cbind(
    c(x,rev(x),x[1]),
    c(y[1],y,rev(y))))
  
  sp <-  sp::SpatialPolygons(list(sp::Polygons(list(p), 1)))
  return(sp)
}


extent2SpatialPolygon <- function(ext){
  coords <- rbind(c(ext[1],ext[3]),
                  c(ext[2],ext[3]),
                  c(ext[2],ext[4]),
                  c(ext[1],ext[4]),
                  c(ext[1],ext[3])
  )
  
  pol <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)),ID="1")))
  return(pol)
}
