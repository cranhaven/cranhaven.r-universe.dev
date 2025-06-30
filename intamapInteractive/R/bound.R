
###################################
#
# Bound.R includes different functions for finding boundaries between polygons (countries)
#
###################################

findBoundaryLines = function(polygons, projOrig, projNew,regCode = "regCode") {
  
  if (!missing(projOrig)) {
    proj4string(polygons) = CRS(projOrig)
    if (!missing(projNew) & projOrig != projNew) {
      polygons = as(st_transform(polygons, crs = st_crs(projNew)), "Spatial")
    }
  }
  boundaryLines = findBoundaries(polygons,regCode)

  if (!extends(class(boundaryLines),"Spatial")) coordinates(boundaryLines) = ~x+y
  if (!missing(projNew) & (is.na(is.projected(boundaryLines)) | 
                          !is.projected(boundaryLines)))
     proj4string(boundaryLines) = CRS(projNew)

  return(boundaryLines)
}





findBoundaries = function(polygons,regCode) {
# Function which takes a shape file of regional boundaries as input,
# and gives back a list of the points that define each single border
uRegCode = unique(polygons[[regCode]])
nRegCode = length(uRegCode)
if (exists("cabound")) rm(cabound)
cList = list()
polyList = list()
for (ic in 1:nRegCode) {
  rci = as.character(uRegCode[ic])
  c1 = polygons[regCode %in% names(polygons) & polygons[[regCode]] == rci,]
  c1p = spdf2list2(c1)
  c1kor = c1p$poly
  c1df = as.data.frame(c1kor)
  c1n = c1df[!is.na(c1df[,1]),]
  c1k = unique(c1n)
  cList[[ic]] = c1k
  polyList[[ic]] = c1

}
for (i in 1:(nRegCode-1)) {
  rci = as.character(uRegCode[i])
  c1k = cList[[i]]
  c1 = polyList[[i]]
  for (j in (i+1):nRegCode) {
    rcj = as.character(uRegCode[j])
    c2k = cList[[j]]
    c2 = polyList[[j]]
    if ( commonArea(c1,c2)[[1]] > 0.001) {
      c21 = rbind(c1k,c2k)
      c21 = signif(c21, 14)
      lbound = c21[duplicated(c21),]
      ldim = dim(lbound)[1]
      if (ldim > 0) {
        caz = data.frame(c1 = rci,c2 = rcj, lbound)
        if (exists("cabound")) {
          cabound = rbind(cabound,caz)
        } else {
          cabound = caz
        }
      }
    }
  }
}
names(cabound) = c("c1","c2","x","y")
return (cabound)
}


