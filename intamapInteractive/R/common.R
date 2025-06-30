


commonArea = function(objecti,objectj) {
  bi = bbox(objecti)
  bj = bbox(objectj)
  iarea = bbArea(bi)
  jarea = bbArea(bj)
  sdim = sqrt((iarea+jarea)/2)
  bl = list()
  for (i in 1:2) bl[[i]] =  max(bi[[i]],bj[[i]])-sdim/1000
  for (i in 3:4) bl[[i]] =  min(bi[[i]],bj[[i]])+sdim/1000
  if (bl[[3]] >= bl[[1]] & bl[[4]] >= bl[[2]]) {
    larea = bbArea(bl)
    if (larea < 0.0001*max(iarea,jarea)) larea = 0.0001*max(iarea,jarea)
  } else {
    larea = 0
  }
#
  if (jarea< (0.0001*iarea)) jarea = 0.0001*iarea
  if (iarea< (0.0001*jarea)) iarea = 0.0001*jarea
  ilarea = larea/iarea
  jlarea = larea/jarea


  return(list(ilarea,jlarea))
}


bbArea = function(bb) {
  xd = bb[[3]]-bb[[1]]
  yd = bb[[4]]-bb[[2]]
  abs(xd) * abs(yd)
}





 spdf2list2 = function (data) {
# Modified function to convert a SpatialPolygonsDataFrame into a list of polygons
# This is a new version, also checking if there are several polygons in each Polygons
# Original function taken from GeoXp
    if (class(data)[1] != "SpatialPolygonsDataFrame") 
        stop("Must be a SpatialPolygonsDataFrame object")
    poly <- data@polygons
    n <- length(poly)
    for (i in 1:n) {
      np = length(poly[[i]]@Polygons)
      for (j in 1:np) {
        if (i ==1 & j ==1) {
          X <- poly[1][[1]]@Polygons[[1]]@labpt[1]
          Y <- poly[1][[1]]@Polygons[[1]]@labpt[2]
          contours <- rbind(NA, NA, NA, poly[1][[1]]@Polygons[[1]]@coords)
        } else {
          X <- rbind(X, poly[i][[1]]@Polygons[[j]]@labpt[1])
          Y <- rbind(Y, poly[i][[1]]@Polygons[[j]]@labpt[2])
          contours = rbind(contours, NA, NA, NA, poly[i][[1]]@Polygons[[j]]@coords)
        }
      }
    }
    contours = rbind(contours, NA, NA, NA)
    return(list(X = X, Y = Y, poly = contours))
}




dSolve = function(diffs) {
  D = as.data.frame(diffs$D)
  xnam <- paste("x", 1:dim(D)[2], sep="")
  names(D) = xnam
  fmla <- as.formula(paste("Q ~ ", paste(xnam, collapse= "+"),"-1"))
  Q = diffs$Q
  V = diffs$V
  dat = cbind(D,Q=Q)
  ols = as.data.frame(summary(lm(fmla,dat))$coefficients)
  wls = as.data.frame(summary(lm(fmla,dat,weights = 1/V))$coefficients)
  names(ols) = c("ols","ols.std","ols t value", "ols.p")
  names(wls) = c("wls","wls.std","wls t value", "wls.p")
  return(cbind(ols,wls))
}
