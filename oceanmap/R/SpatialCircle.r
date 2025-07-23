# x = 10 # center x
# y = 10 # center y
# n = 100 # nr of pts
# r = 5 # radius
SpatialCircle <- function(x,y,r,n=100,proj4str){
  pts <- seq(0, 2 * pi, length.out = n)
#   plot(sin(pts), cos(pts), type = 'l', asp = 1) # test
  
  xy <- cbind(x + r * sin(pts), y + r * cos(pts))
  sl <- SpatialLines(list(Lines(list(Line(xy)), "line")))
if(!missing(proj4str)) sp::proj4string(sl) <- proj4str
  #   plot(sl, add=FALSE, col = 'red', axes=T )
  return(sl)
}