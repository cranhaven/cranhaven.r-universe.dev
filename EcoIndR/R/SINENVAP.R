SINENVAP<-function(data=NULL, var=NULL, dataLat=NULL, dataLon=NULL, polyLat=NULL, polyLon=NULL, zonedata=NULL, zonepoly=NULL,  convex=FALSE, alpha=0.07, ASC=NULL, shape=NULL, shapenames=NULL, Area=NULL, validation=30,
type.krige="OK", trend.d = "cte", trend.l = "cte", model="AUTO",  minimisation="optim", weights="npairs", maxdist=NULL, nugget=NULL,
sill=NULL, range=NULL, kappa=NULL, beta=NULL, jitter="jitter", maxjitter=0.00001, direction=c(0,45,90,135), inside=TRUE, error=FALSE,
ResetPAR=TRUE, PAR=NULL, BOXPLOT=NULL, OUTLINE=FALSE, XLABP=NULL, YLABP=NULL, XLAB=NULL, YLAB=NULL, XLABB="Model",
YLABB="Accuracy measures", MAIN="", XLIM=NULL, YLIM=NULL,  ZLIM=NULL, COLOR="rev(heat.colors(100))", COLORC="black" ,
COLORB=NULL , COLORM="transparent", NLEVELS=10, LABCEX=0.6, contour=TRUE, breaks=10, ndigits=0, xl=0, xr=0,  pro=TRUE,
cell=NULL, file1="Predictions data.csv", file2="Predictions polygon.csv", file3="Accuracy measures.csv", file4="Semivariogram.csv",
file5="Standard errors.csv", file6="Model selected.txt", na="NA", dec=",", row.names=FALSE){



if(!is.null(data)){
polygon<-data
}

if(is.null(zonedata)) cordata="dec" else cordata="UTM"

if(is.null(zonepoly)) corpolygon="dec" else corpolygon="UTM"

ZZ<-matrix(c("","","",""), nrow=2)


###Function for removing NA at the end of a column
endNA <- function (object){
nona <-complete.cases(object)
val<- (cumsum(nona) > 0) & rev(cumsum(rev(nona)) > 0)
object[val,, drop = FALSE]
}

rescale<-function(x,newrange) {
 if(missing(x) | missing(newrange)) {
  usage.string<-paste("Usage: rescale(x,newrange)\n",
   "\twhere x is a numeric object and newrange is the new min and max\n",
   sep="",collapse="")
  stop(usage.string)
 }
 if(is.numeric(x) && is.numeric(newrange)) {
  xna<-is.na(x)
  if(all(xna)) return(x)
  if(any(xna)) xrange<-range(x[!xna])
  else xrange<-range(x)
  # if x is constant, just return it
  if(xrange[1] == xrange[2]) return(x)
  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  return(newrange[1]+(x-xrange[1])*mfac)
 }
 else {
  warning("Only numeric objects can be rescaled")
  return(x)
 }
}

color.scale<-function(x,cs1=c(0,1),cs2=c(0,1),cs3=c(0,1),alpha=1,
 extremes=NA,na.color=NA,xrange=NULL,color.spec="rgb") {
 
 xdim<-dim(x)
 if (diff(range(x, na.rm = TRUE)) == 0) x<-x/max(x,na.rm=TRUE)
 naxs<-is.na(x)
 if(!is.na(extremes[1])){
  # calculate the color ranges from the extremes - only for rgb
  colmat<-col2rgb(extremes)
  cs1<-colmat[1,]/255
  cs2<-colmat[2,]/255
  cs3<-colmat[3,]/255
  color_spec<-"rgb"
 }
 maxcs1<-ifelse(color.spec=="hcl",360,1)
 maxcs2<-ifelse(color.spec=="hcl",100,1)
 maxcs3<-ifelse(color.spec=="hcl",100,1)
 ncolors<-length(x)
 if(is.null(xrange)) {
  xrange<-range(x,na.rm=TRUE)
  drop.extremes<-FALSE
 }
 else {
  if(xrange[1] > min(x,na.rm=TRUE) || xrange[2] < max(x,na.rm=TRUE))
   stop("An explicit range for x must include the range of x values.")
  x<-c(xrange,x)
  drop.extremes=TRUE
 }
 ncs1<-length(cs1)
 if(ncs1>1) {
  cs1s<-rep(cs1[ncs1],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(ncs1-1)
  for(seg in 1:(ncs1-1)){
   segindex<-which((x >= xstart) & (x <= (xstart+xinc)))
   cs1s[segindex]<-rescale(x[segindex],cs1[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(cs1s,na.rm=TRUE) < 0 || max(cs1s,na.rm=TRUE) > maxcs1)
   cs1s<-rescale(cs1s,c(0,maxcs1))
 }
 else cs1s<-rep(cs1,ncolors)
 ncs2<-length(cs2)
 if(ncs2>1) {
  cs2s<-rep(cs2[ncs2],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(ncs2-1)
  for(seg in 1:(ncs2-1)){
   segindex<-which((x >= xstart) & (x <= (xstart+xinc)))
   cs2s[segindex]<-rescale(x[segindex],cs2[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(cs2s,na.rm=TRUE) < 0 || max(cs2s,na.rm=TRUE) > maxcs2)
   cs2s<-rescale(cs2s,c(0,maxcs2))
 }
 else cs2s<-rep(cs2,ncolors)
 ncs3<-length(cs3)
 if(ncs3>1) {
  cs3s<-rep(cs3[ncs3],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(ncs3-1)
  for(seg in 1:(ncs3-1)){
   segindex<-which((x >= xstart) & (x <= (xstart+xinc)))
   cs3s[segindex]<-rescale(x[segindex],cs3[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(cs3s,na.rm=TRUE) < 0 || max(cs3s,na.rm=TRUE) > maxcs3)
   cs3s<-rescale(cs3s,c(0,maxcs3))
 }
 else cs3s<-rep(cs3,ncolors)
 if(drop.extremes) {
  cs1s<-cs1s[-(1:2)]
  cs2s<-cs2s[-(1:2)]
  cs3s<-cs3s[-(1:2)]
 }
 colors<-do.call(color.spec,list(cs1s,cs2s,cs3s,alpha=alpha))
 if(!is.null(xdim)) colors<-matrix(colors,nrow=xdim[1])
 if(length(naxs)) colors[naxs]<-na.color
 return(colors)
}

color.gradient<-function(reds,greens,blues,nslices=50) {
 return(color.scale(1:nslices,reds,greens,blues))
}

gradient.rect<-function(xleft,ybottom,xright,ytop,reds,greens,blues, 
 col=NULL,nslices=50,gradient="x",border=par("fg")) {

 if(is.null(col)) col<-color.gradient(reds, greens, blues, nslices)
 else nslices<-length(col)
 nrect<-max(unlist(lapply(list(xleft,ybottom,xright,ytop),length)))
 oldxpd<-par(xpd=NA)
 if(nrect > 1) {
  if(length(xleft) < nrect) xleft<-rep(xleft,length.out=nrect)
  if(length(ybottom) < nrect) ybottom<-rep(ybottom,length.out=nrect)
  if(length(xright) < nrect) xright<-rep(xright,length.out=nrect)
  if(length(ytop) < nrect) ytop<-rep(ytop,length.out=nrect)
  for(i in 1:nrect)
   gradient.rect(xleft[i],ybottom[i],xright[i],ytop[i],
    reds,greens,blues,col,nslices,gradient,border=border)
 }
 else {
  if (gradient == "x") {
   xinc <- (xright - xleft)/nslices
   xlefts <- seq(xleft, xright - xinc, length = nslices)
   xrights <- xlefts + xinc
   rect(xlefts,ybottom,xrights,ytop,col=col,lty=0)
   rect(xlefts[1],ybottom,xrights[nslices],ytop,border=border)
  }
  else {
   yinc <- (ytop - ybottom)/nslices
   ybottoms <- seq(ybottom, ytop - yinc, length = nslices)
   ytops <- ybottoms + yinc
   rect(xleft,ybottoms,xright,ytops,col=col,lty=0)
   rect(xleft,ybottoms[1],xright,ytops[nslices],border=border)
  }
 }
 par(oldxpd)
 invisible(col)
}

color.legend<-function (xl,yb,xr,yt,legend,rect.col,cex=1,align="lt",
 gradient="x",...) {

 oldcex<-par("cex")
 par(xpd=TRUE,cex=cex)
 gradient.rect(xl,yb,xr,yt,col=rect.col,nslices=length(rect.col),
  gradient=gradient)
 if(gradient == "x") {
  xsqueeze<-(xr-xl)/(2*length(rect.col))
  textx<-seq(xl+xsqueeze,xr-xsqueeze,length.out=length(legend))
  if(match(align,"rb",0)) {
   texty<-yb-0.2*strheight("O")
   textadj<-c(0.5,1)
  }
  else {
   # assume it's the default
   texty<-yt+0.2*strheight("O")
   textadj<-c(0.5,0)
  }
 }
 else {
  ysqueeze<-(yt-yb)/(2*length(rect.col))
  texty<-seq(yb+ysqueeze,yt-ysqueeze,length.out=length(legend))
  if(match(align,"rb",0)) {
   textx<-xr+0.2*strwidth("O")
   textadj<-c(0,0.5)
  }
  else {
   # assume it's the default
   textx<-xl-0.2*strwidth("O")
   textadj<-c(1,0.5)
  }
 }
 text(textx,texty,labels=legend,adj=textadj,...)
 par(xpd=FALSE,cex=oldcex)
}





####Function bold lab

bold<-function(PAR=PAR){
if(!is.null(PAR)){
parexe<-paste("par(new,",toString(x=PAR), ")")
eval(parse(text=parexe))
}
else{
par(font.lab=2, mar=c(6,5,3,4),cex.lab=1.5)
}
}

if(inherits(ASC, "RasterLayer")==TRUE){

ZZ[1,1]<-"ORGANIZING ASC FILE"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

data<-ASC
var<-"variable"
dataLat<-"Lat"
dataLon<-"Lon"

###Format of matrix

if(round(raster::xmin(data))==-180 & round(raster::ymin(data))==-90 & round(raster::xmax(data))==180 & round(raster::ymax(data))==90){
m1<-raster::as.matrix(data)
dimm<-dim(m1)
long<-seq(from=(-180+360/dimm[2]), to = 180 , by = 360/dimm[2])
m1<-rbind(long,m1)

lat<-seq(from=(90-180/dimm[1]), to = -90 , by = -180/dimm[1])
lat<-c(0,lat)
data<-cbind(lat,m1, deparse.level=0)
}
else{

reso<-raster::res(data)

r1<-raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=reso)

data<-raster::resample(data,r1)

m1<-raster::as.matrix(data)
dimm<-dim(m1)

long<-seq(from=(raster::xmin(data)+(raster::xmax(data)-raster::xmin(data))/dimm[2]), to = raster::xmax(data) , by = (raster::xmax(data)-raster::xmin(data))/dimm[2])
m1<-rbind(long,m1)

lat<-seq(from=(raster::ymax(data)+(raster::ymin(data)-raster::ymax(data))/dimm[1]), to = raster::ymin(data) , by = (raster::ymin(data)-raster::ymax(data))/dimm[1])
lat<-c(0,lat)
data<-cbind(lat,m1, deparse.level=0)

}
##Format Lon, Lat and variable

Lon<-data[1,-1]
lenLon<-length(Lon)

Lat<-data[-1,1]
lenLat<-length(Lat)

Longitude<-matrix(t(replicate(lenLat,Lon)),ncol=1)
Latitude<-rep(Lat,lenLon)
vari<-matrix(data[-1,-1],ncol=1)


data<-data.frame(Longitude,Latitude, vari)
names(data)<-c(dataLon, dataLat, var)
remove(Longitude)
remove(Latitude)
remove(vari)

}
else{
if(!is.null(zonedata)){
data<-na.exclude(data.frame(subset(data, select=dataLon), subset(data, select=dataLat),  subset(data, select=zonedata), subset(data, select=var)))
}
else{
data<-na.exclude(data.frame(subset(data, select=dataLon), subset(data, select=dataLat), subset(data, select=var)))
}
}###End ASC


if(!is.null(Area)){
polyLat<-"Lat"
polyLon<-"Lon"

if(!is.null(Area) & exists("adworld1")==TRUE){
polygon<-adworld1
}
else{
polygon<-adworld
}

}

if(!is.null(polyLat) & !is.null(polyLon)){
if(!is.null(zonepoly)){
polygonB<-data.frame(subset(polygon, select=polyLon), subset(polygon, select=polyLat), subset(polygon, select=zonepoly))
polygonB<-endNA(polygonB)
polygon<-na.exclude(data.frame(subset(polygon, select=polyLon), subset(polygon, select=polyLat), subset(polygon, select=zonepoly)))
}
else{
polygonB<-data.frame(subset(polygon, select=polyLon), subset(polygon, select=polyLat))
polygonB<-endNA(polygonB)
polygon<-na.exclude(data.frame(subset(polygon, select=polyLon), subset(polygon, select=polyLat)))
}
}

if(!is.null(shape) & is.null(shapenames)){
stop("It is necessary to specify in the argment 'shapenames' the variable with the names of the polygons in the shape")
}


if(!is.null(shape)){

ZZ[1,1]<-"ORGANIZING SHAPE FILE"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(inherits(shape,"list")==TRUE){
polygon<-shape[[1]]
lsh<-length(shape)
if(lsh>1){
ss<-seq(2,lsh)
hh<-as.character(shape[ss])
polygon<-eval(parse(text=paste("subset(polygon,",noquote(shapenames), " %in% hh)", sep="")))
}
}
else{
polygon<-shape
if(inherits(polygon,"character")==TRUE){
polygon<-eval(parse(text=paste(".GlobalEnv$", polygon, sep="")))
}
}
numero<-length(polygon)
polygonB<-as.data.frame(polygon@polygons[[1]]@Polygons[[1]]@coords)
if(numero>1){
polygonB<-rbind(polygonB,c(NA,NA))
for(kk in 2:numero){
ppt<-as.data.frame(polygon@polygons[[kk]]@Polygons[[1]]@coords)
if(kk<numero){
ppt<-rbind(ppt,c(NA,NA))
}
polygonB<-rbind(polygonB,ppt)
}
}
polyLat<-"Lat"
polyLon<-"Lon"
names(polygonB)<-c("Lon","Lat")
polygon<-na.exclude(polygonB)

}####End shape

######Convert UTM to dec

if(corpolygon=="UTM"){

ZZ[1,1]<-"CONVERTING POLYGONS FROM UTM TO DECIMAL DEGREES COORDINATES"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

dim<-dim(polygonB)

for(z in 1:dim[1]){
if(!is.na(polygonB[z,polyLon])){
zone<-polygonB[z,zonepoly]
texto<-paste("+proj=longlat +zone=", zone," +datum=WGS84", sep="")
xy<-polygonB[z, c(polyLon, polyLat)]
names(xy)<-c("x","y")
sp::coordinates(xy)<-c("x", "y") 
sp::proj4string(xy) <- sp::CRS("+proj=utm + ellps=WGS84") 
data1<-sp::spTransform(xy,CRS=texto) 
polygonB[z,c(polyLon, polyLat)]<- as.data.frame(sp::coordinates(data1)) 
}
}

polygon<-na.exclude(polygonB)


}##End if corpoly


if(cordata=="UTM"){

ZZ[1,1]<-"CONVERTING DATA FROM UTM TO DECIMAL DEGREES COORDINATES"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

xy<-data[,1:2]
names(xy)<-c("x","y")
dimxy<-dim(xy)
sp::coordinates(xy)<-c("x", "y") 
sp::proj4string(xy) <- sp::CRS("+proj=utm + ellps=WGS84") 

for(z in 1:dimxy[1]){
zone<-data[z,zonedata]
texto<-paste("+proj=longlat +zone=", zone," +datum=WGS84", sep="")
data1<-sp::spTransform(xy[z],CRS=texto) 
data[z,1:2]<- as.data.frame(sp::coordinates(data1)) 
}

}##End if cordata



###Jitter

if(any(duplicated(data[,1:2])==TRUE)==FALSE){
jitter<-"NO"
}
 

if(jitter=="jitter"){
data1<-geoR::jitterDupCoords(x=data[,1:2],max=maxjitter)
if(is.null(zonedata)){
data<-cbind(data1,data[,3])
names(data)<-c(dataLon, dataLat, var)
}
else{
data<-cbind(data1,data[,3:4])
names(data)<-c(dataLon, dataLat, zonedata, var)
}
}

if(jitter=="mean"){
if(is.null(zonedata)){
data<-aggregate(x=data[,var], by = list(data[ , dataLon], data[,dataLat]), mean, na.rm=TRUE)
names(data)<-c(dataLon, dataLat, var)
}
else{
aggregate(x=data[,c(zonedata,var)], by = list(data[ , dataLon], data[,dataLat]), mean, na.rm=TRUE)
names(data)<-c(dataLon, dataLat, zonedata, var)
}
}



###Alpha shape

if(convex==TRUE){

ZZ[1,1]<-"ESTIMATING CONVEX HULL"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

ashape_to_SPLDF <- function(x, proj4string=NA)
	{
	if(inherits(x, 'ashape')==FALSE)
		stop('this function only works with `ashape` class objects')
	
	# convert ashape edges to DF
	x.as.df <- as.data.frame(x$edges)
	
	# convert each edge to a line segment
	l.list <- list()
	for(i in 1:nrow(x.as.df))
		{
		# extract line start and end points as 1x2 matrices
		p1 <- cbind(x.as.df$x1[i], x.as.df$y1[i])
		p2 <- cbind(x.as.df$x2[i], x.as.df$y2[i])
		# row-bind into 2x3 matrix
		l.list[[i]] <- sp::Line(rbind(p1, p2))
		}
		
	# promote to Lines class, then to SpatialLines class
	l <- sp::Lines(l.list, ID=1)
	
	# copy over CRS data from original point data
	l.spl <- sp::SpatialLines(list(l), proj4string=sp::CRS(as.character(NA)))
	
	# promote to SpatialLinesDataFrame, required for export to GRASS / OGR
	l.spldf <- sp::SpatialLinesDataFrame(l.spl, data=data.frame(id=1), match.ID=FALSE)
	
	return(l.spldf)
	}


prueba<-alphahull::ashape(cbind(data[,dataLon],data[,dataLat]), alpha=alpha)
spp<-ashape_to_SPLDF(prueba)

spp_sf <- sf::st_as_sf(spp) 
sf_polygons <- sf::st_polygonize(spp_sf)
sf_polys= sf::st_collection_extract(sf_polygons, type = c("POLYGON"), warn = FALSE)  
spatial_ps=methods::as(sf_polys , "Spatial")
polygon <- raster::aggregate(spatial_ps,dissolve=T)
numero<-length(polygon@polygons[[1]]@Polygons)
polygonB<-as.data.frame(polygon@polygons[[1]]@Polygons[[1]]@coords)
if(numero>1){
polygonB<-rbind(polygonB,c(NA,NA))
for(kk in 2:numero){
ppt<-as.data.frame(polygon@polygons[[1]]@Polygons[[kk]]@coords)
if(kk<numero){
ppt<-rbind(ppt,c(NA,NA))
}
polygonB<-rbind(polygonB,ppt)
}
}

polyLat<-"Lat"
polyLon<-"Lon"
names(polygonB)<-c("Lon","Lat")
polygon<-na.exclude(polygonB)
}###End convex




data<-replace(data, data==-9999,NA)
polygon<-replace(polygon, polygon==-9999,NA)
polygonB<-replace(polygonB, polygonB==-9999,NA)

###Selection of points inside the polygon


if(inside==TRUE){

ZZ[1,1]<-"SELECTION OF DATA POINTS INSIDE THE POLYGONS"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

maxpoLon<-max(polygon[, polyLon])
minpoLon<-min(polygon[, polyLon])
maxpoLat<-max(polygon[, polyLat])
minpoLat<-min(polygon[, polyLat])
fila<-which((data[,dataLon] >= minpoLon) &  (data[,dataLon] <= maxpoLon) & (data[,dataLat] >= minpoLat) &  (data[,dataLat] <= maxpoLat))
borrar<-data[fila,]
log<-mgcv::in.out(as.matrix(polygon[,c(polyLon, polyLat)]),as.matrix(borrar[, c(dataLon, dataLat)]))
mm<-cbind(borrar,log)
dhh<-dim(mm)
mm<-mm[mm[,dhh[2]],]
mm<-mm[,c(-dhh[2])]
data<-mm
remove(borrar)
}


####Warnings
if(exists("adworld")==FALSE){
adworld<-1
}

if(!is.null(Area) & exists("adworld1")==FALSE){
stop("It is necessary to use RWizard and replace data(adworld) by @_Build_AdWorld_, for using administative areas")
}

if(exists("adworld1")==FALSE){
adworld1<-1
}

if(exists("adworld2")==FALSE){
adworld2<-1
}

if(cordata=="dec"){
if(max(data[, dataLon], na.rm=TRUE)>180 | max(data[, dataLat], na.rm=TRUE)>90) {
warning("It seems data are in UTM coordinates. If it is so, the argument cordata should be UTM")
}
}

if(corpolygon=="dec"){
if(max(polygon[, polyLon], na.rm=TRUE)>180 | max(polygon[, polyLat], na.rm=TRUE)>90) {
warning("It seems polygons are in UTM coordinates. If it is so, the argument corpolygon should be UTM")
}
}









###Selection of random rows
if(validation>0){
dimdata<-dim(data)
tot<-floor(dimdata[1]*validation/100)
selra<-sample(1:dimdata[1],tot)
random<-data[selra,]
data<-data[-selra,]
}
else{
random=0
}



if(ResetPAR==TRUE){
#Resetear par() a las opciones por defecto
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar()) }
else{
}


####Predictions

ZZ[1,1]<-"PLOTTING PREDICTIONS"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(type.krige=="SK" & is.null(beta)){
beta<-mean(data[,var], na.rm=TRUE)
}

accu<-matrix(rep(0,49),7,7)


if(is.null(XLIM)){
maxLon<-max(polygon[,polyLon], na.rm=TRUE)
minLon<-min(polygon[,polyLon], na.rm=TRUE)
}
else{
minLon<-XLIM[1]
maxLon<-XLIM[2]
}

if(is.null(YLIM)){
minLat<-min(polygon[,polyLat], na.rm=TRUE)
maxLat<-max(polygon[,polyLat], na.rm=TRUE)
}
else{
minLat<-YLIM[1]
maxLat<-YLIM[2]
}

bold(PAR=PAR)



########### function written by Greg Snow
squishplot <- function(xlim,ylim,asp=1){
   if(length(xlim) < 2) stop('xlim must be a vector of length 2')
   if(length(ylim) < 2) stop('ylim must be a vector of length 2')

  tmp <- par(c('plt','pin','xaxs','yaxs'))

  if( tmp$xaxs == 'i' ){ # not extended axis range

        xlim <- range(xlim)
  } else { # extended range

	tmp.r <- diff(range(xlim))
	xlim <- range(xlim) + c(-1,1)*0.04*tmp.r

  }

  if( tmp$yaxs == 'i' ){ # not extended axis range

        ylim <- range(ylim)
  } else { # extended range

	tmp.r <- diff(range(ylim))
	ylim <- range(ylim) + c(-1,1)*0.04*tmp.r

  }

  tmp2 <- (ylim[2]-ylim[1])/(xlim[2]-xlim[1])

  tmp.y <- tmp$pin[1] * tmp2 * asp

  if(tmp.y < tmp$pin[2]){ # squish vertically
	par(pin=c(tmp$pin[1], tmp.y))
	par(plt=c(tmp$plt[1:2], par('plt')[3:4]))
  } else { # squish horizontally
	tmp.x <- tmp$pin[2]/tmp2/asp
	par(pin=c(tmp.x, tmp$pin[2]))
	par(plt=c(par('plt')[1:2], tmp$plt[3:4]))

  }

  return(invisible(tmp['plt']))
} ####end of function


if(length(COLOR)>1){
lenC<-length(COLOR)
colt<-paste("c(", "'",COLOR[1],"'", sep="")
for(z in 2:lenC){
if(z==lenC){
colt<-paste(colt, ",", "'", COLOR[z],"'", ")",sep="")
}
else{
colt<-paste(colt, ",", "'", COLOR[z],"'", sep="")
}
}
color<-noquote(colt)
}
else{
color<-noquote(COLOR)
}

if(any(is.na(polygonB))==TRUE){
dim<-dim(polygonB)
L<-polygonB[,polyLon]
nas<-which(is.na(L))
lenA<-length(nas)
}
else{
polygontemp<-polygon
lenA<-0
}

zh<-(-1)

#####Estimating cell size

Lat<-polygon[,polyLat]
Lon<-polygon[,polyLon]

if(is.null(cell)){

dimB<-dim(polygonB)
cna<-which(is.na(polygonB[,1]))
tna<-length(cna)
if(tna==0){
rLon<-abs(max(Lon, na.rm=TRUE)-min(Lon, na.rm=TRUE))
rLat<-abs(max(Lat, na.rm=TRUE)-min(Lat, na.rm=TRUE))
}
else{
repp<-rep(paste("A",1, sep=""), (cna[1]))
kl<-cna[1]+1

for(ww in 2:(tna+1)){
if(ww==(tna+1)){
reppt<-rep(paste("A",ww, sep=""), (dimB[1]-kl))
repp<-append(repp,reppt)
}
else{
reppt<-rep(paste("A",ww, sep=""), (cna[ww]-kl))
kl<-cna[ww]+1
repp<-append(repp,reppt)
}
}
borrar<-cbind(polygon,repp)


minbLon<-aggregate(borrar[,1],by=list(borrar[,"repp"]),min,na.rm=TRUE)
maxbLon<-aggregate(borrar[,1],by=list(borrar[,"repp"]),max,na.rm=TRUE)
rLon<-max(abs(maxbLon[,2]-minbLon[,2]), na.rm=TRUE)

minbLat<-aggregate(borrar[,2],by=list(borrar[,"repp"]),min,na.rm=TRUE)
maxbLat<-aggregate(borrar[,2],by=list(borrar[,"repp"]),max,na.rm=TRUE)
rLat<-max(abs(maxbLat[,2]-minbLat[,2]), na.rm=TRUE)
remove(borrar)
}

cell<-round(min(c(rLon,rLat))/100, digits=4)


}#End cell NULL




seq1<-(seq(min(Lon, na.rm=TRUE)-abs(0.3*min(Lon, na.rm=TRUE)/100), max(Lon, na.rm=TRUE)+abs(0.3*max(Lon, na.rm=TRUE)/100),cell))

seq2<-(seq(min(Lat, na.rm=TRUE)-abs(0.2*min(Lat, na.rm=TRUE)/100), max(Lat, na.rm=TRUE)+abs(0.2*max(Lat, na.rm=TRUE)/100),cell))


data.grid<-expand.grid(Lon=seq1, Lat=seq2)


####Selection of grid points inisde the polygons



ZZ[1,1]<-"1. Adjusting the grid into the polygon"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

maxpoLon<-max(polygon[, polyLon])
minpoLon<-min(polygon[, polyLon])
maxpoLat<-max(polygon[, polyLat])
minpoLat<-min(polygon[, polyLat])
fila<-which((data.grid[,"Lon"] >= minpoLon) &  (data.grid[,"Lon"] <= maxpoLon) & (data.grid[,"Lat"] >= minpoLat) &  (data.grid[,"Lat"] <= maxpoLat))
borrar<-data.grid[fila,]
log<-mgcv::in.out(as.matrix(polygon[,c(polyLon, polyLat)]),as.matrix(borrar[, c("Lon", "Lat")]))
mm<-cbind(borrar,log)
dhh<-dim(mm)
mm<-mm[mm[,dhh[2]],]
mm<-mm[,c(-dhh[2])]
pos<-0
lend<-dim(data)
remove(borrar)




ZZ[1,1]<-"2. Selection of the grid coordinates nearest to data coordinates"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(validation>0){
dimr<-dim(random)
for(z in 1:dimr[1]){
valor<-(random[z,dataLon]-mm[,1])^2+(random[z,dataLat]-mm[,2])^2
pos1<-which(valor==min(valor,na.rm=TRUE))
pos<-append(pos,pos1)
}
pos<-pos[-1]
}
else{
for(z in 1:lend[1]){
valor<-(data[z,dataLon]-mm[,1])^2+(data[z,dataLat]-mm[,2])^2
pos1<-which(valor==min(valor,na.rm=TRUE))
pos<-append(pos,pos1)
}
pos<-pos[-1]
}

###Selection of columns
nombres<-names(data)
Lat<-which(nombres==dataLat)
Lon<-which(nombres==dataLon)
Variable<-which(nombres==var)
dataGeo<-geoR::as.geodata(data, coords.col =c(Lon, Lat), data.col = Variable)

if(is.null(maxdist)){
maxdist<-max(dist(data[,c(dataLon,dataLat)]),na.rm=TRUE)/2
}
variog<-geoR::variog(dataGeo,max.dist=maxdist)
varig<-data.frame(variog$u,variog$v)
names(varig)<-c("Distance","Semivariance")

if(is.null(nugget)){
fixnugget=FALSE
nugget=0
}
else{
fixnugget=TRUE
nugget=nugget
}

if(is.null(kappa)){
fixkappa<-FALSE
kappa<-0.5
}
else{
fixkappa<-TRUE
}



ZZ[1,1]<-"3. Estimation of predictions and accuracy measures"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


if(any(model=="AUTO")==TRUE){
modelos<-c("cauchy","circular", "cubic", "exponential", "gaussian", "gneiting", "linear",  "power",
"powered.exponential", "pure.nugget", "spherical",  "wave")
}
else{
modelos<-model
}

lenmo<-length(modelos)

index<-c("r-squared", "NMAE", "NRMSE","Nash-Sutcliffe coefficient", "Index of agreement", "NRMAE", "NRRMSE")

valorr<-(-100)

if(validation>0){
TaylorD<-random[,var]
}
else{
TaylorD<-data[,var]
}

for(hh in 1:lenmo){

modelo<-modelos[hh]

if(modelo=="powered.exponential"){
m<-rep("p.exponential",7)
}
else{
m<-rep(modelo,7)
}



ZZ[1,1]<-paste("Testing model (", hh, " of ", lenmo, "): ", modelo, sep="")
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


variofit<-try(geoR::variofit(variog, fix.nugget=fixnugget, nugget=nugget, cov.model=modelo, max.dist=maxdist, fix.kappa=fixkappa,
minimisation.function=minimisation, weights=weights), silent=TRUE)


if(as.character(substr(variofit, start=1, stop=5))!="Error"){

if(!is.null(sill)){
variofit$cov.pars<-c(sill, variofit$cov.pars[2])
}

if(!is.null(range)){
variofit$cov.pars<-c(variofit$cov.pars[1], range)
}


kc<-try(geoR::krige.conv(dataGeo, loc=data.grid, bord=polygon[,c(polyLon,polyLat)], krige= geoR::krige.control(beta=beta, type.krige=type.krige, trend.d = trend.d, trend.l = trend.l,
kappa=variofit$kappa, nugget=variofit$nugget, cov.model=modelo, cov.pars=variofit$cov.pars)), silent=TRUE)

if(as.character(substr(kc, start=1, stop=5))!="Error"){

if(validation>0){
reg<-lm(kc$predict[pos]~random[,var])
Re<-summary(reg)
NMAE<-1-(sum(abs(kc$predict[pos]-random[,var]), na.rm=TRUE)/sum(random[,var], na.rm=TRUE))

NRMSE<-1-sqrt(sum((kc$predict[pos]-random[,var])^2, na.rm=TRUE)/sum(random[,var], na.rm=TRUE))

E<-1-(sum((random[,var]-kc$predict[pos])^2, na.rm=TRUE)/sum((random[,var]-mean(random[,var],na.rm=TRUE))^2, na.rm=TRUE))

d<-1-(sum((random[,var]-kc$predict[pos])^2, na.rm=TRUE)/(sum((abs(kc$predict[pos]-mean(random[,var], na.rm=TRUE))+abs(random[,var]-mean(random[,var], na.rm=TRUE)))^2, na.rm=TRUE)))

NRMAE<-1-(sum(abs((kc$predict[pos]-random[,var])/random[,var])*100,na.rm=TRUE)/sum(random[,var], na.rm=TRUE))

NRRMSE<-1-(sum(abs((((kc$predict[pos]-random[,var])/random[,var])^2)^1/2)*100, na.rm=TRUE)/sum(random[,var], na.rm=TRUE))

}
else{
reg<-lm(kc$predict[pos]~data[,var])
Re<-summary(reg)
NMAE<-1-(sum(abs(kc$predict[pos]-data[,var]), na.rm=TRUE)/sum(data[,var], na.rm=TRUE))

NRMSE<-1-sqrt(sum((kc$predict[pos]-data[,var])^2, na.rm=TRUE)/sum(data[,var], na.rm=TRUE))

E<-1-(sum((data[,var]-kc$predict[pos])^2, na.rm=TRUE)/sum((data[,var]-mean(data[,var],na.rm=TRUE))^2, na.rm=TRUE))

d<-1-(sum((data[,var]-kc$predict[pos])^2, na.rm=TRUE)/(sum((abs(kc$predict[pos]-mean(data[,var], na.rm=TRUE))+abs(data[,var]-mean(data[,var], na.rm=TRUE)))^2, na.rm=TRUE)))

NRMAE<-1-(sum(abs((kc$predict[pos]-data[,var])/data[,var])*100, na.rm=TRUE)/sum(data[,var], na.rm=TRUE))

NRRMSE<-1-(sum(abs((((kc$predict[pos]-data[,var])/data[,var])^2)^1/2)*100, na.rm=TRUE)/sum(data[,var], na.rm=TRUE))

}



col<- kc$predict[pos]

TaylorD<-cbind(TaylorD, col)
dimT<-dim(TaylorD)
if(dimT[2]==2){
TaylorD<-as.data.frame(TaylorD)
names(TaylorD)<-c(var,modelo)
nam<-names(TaylorD)
}
else{
names(TaylorD)<-c(nam,modelo)
nam<-names(TaylorD)
}

paramedia<-c(Re$r.squared,NMAE,NRMSE,E,d, NRMAE, NRRMSE)

paramedia[is.infinite(paramedia) | is.nan(paramedia)] <- NA 

ammean<-mean(paramedia,na.rm=TRUE)


if(ammean>valorr){
valorr<-ammean
variofitT<-variofit
}

if(modelo=="matern" | modelo=="powered.exponential" | modelo=="cauchy"){
kappaT<-variofit$kappa
}
else{
kappaT<-NA
}



if(inherits(accu,"matrix")==TRUE){
accu<-as.data.frame(accu)
names(accu)<-c("Model", "range", "sill", "nugget", "kappa", "Accuracy.Index", "Measure")
accu[,1]<-m
accu[,2]<-rep(variofit$cov.pars[2],7)
accu[,3]<-rep(variofit$cov.pars[1],7)
accu[,4]<-rep(variofit$nugget,7)
accu[,5]<-rep(kappaT,7)
accu[,6]<-index
accu[,7]<-c(Re$r.squared,NMAE, NRMSE,E,d, NRMAE, NRRMSE)
}
else{
pegar<-as.data.frame(matrix(rep(0,49),7,7))
names(pegar)<-c("Model", "range", "sill", "nugget", "kappa", "Accuracy.Index", "Measure")
pegar[,1]<-m
pegar[,2]<-rep(variofit$cov.pars[2],7)
pegar[,3]<-rep(variofit$cov.pars[1],7)
pegar[,4]<-rep(variofit$nugget,7)
pegar[,5]<-rep(kappaT,7)
pegar[,6]<-index
pegar[,7]<-c(Re$r.squared,NMAE, NRMSE,E,d, NRMAE, NRRMSE)
accu<-rbind(accu,pegar)
}


}#End error kc

}#End error variofit

}###End for

if(sum(accu[,2])==0){
stop("It was not possible to fit any model to the semivariogram. Try to modify the value of the argument validation.")
}

medias<-aggregate(x=accu[,c("Measure")], by = list(accu[ , c("Model")]), mean, na.rm=TRUE)
fila<-which(medias[,2]==max(medias[,2],na.rm=TRUE))
sel<-subset(accu,(accu[,"Model"] == as.character(medias[fila[1],1])))
modelo<-unique(as.character(sel[,1]))

if(modelo=="p.exponential"){
modelo<-"powered.exponential"
}
range<-unique(as.numeric(sel[,2]))
sill<-unique(as.numeric(sel[,3]))
nugget<-unique(as.numeric(sel[,4]))

if(modelo=="matern" | modelo=="powered.exponential" | modelo=="cauchy"){
kappa<-unique(as.numeric(sel[,5]))
}



kc<-geoR::krige.conv(dataGeo, loc=data.grid, bord=polygon[,c(polyLon,polyLat)], krige= geoR::krige.control(beta=beta, type.krige=type.krige, trend.d = trend.d, trend.l = trend.l,
kappa=kappa, nugget=nugget, cov.model=modelo, cov.pars=c(sigmasq=sill, phi=range))) 

if(validation>0) te<-random else te<-data

reg<-lm(kc$predict[pos]~te[,var])
Re<-summary(reg)
NMAE<-1-(sum(abs(kc$predict[pos]-te[,var]), na.rm=TRUE)/sum(te[,var], na.rm=TRUE))

NRMSE<-1-sqrt(sum((kc$predict[pos]-te[,var])^2, na.rm=TRUE)/sum(te[,var], na.rm=TRUE))

E<-1-(sum((te[,var]-kc$predict[pos])^2, na.rm=TRUE)/sum((te[,var]-mean(te[,var],na.rm=TRUE))^2, na.rm=TRUE))

d<-1-(sum((te[,var]-kc$predict[pos])^2, na.rm=TRUE)/(sum((abs(kc$predict[pos]-mean(te[,var], na.rm=TRUE))+abs(te[,var]-mean(te[,var], na.rm=TRUE)))^2, na.rm=TRUE)))

NRMAE<-1-(sum(abs((te[,var]-kc$predict[pos])/te[,var])*100, na.rm=TRUE)/sum(te[,var], na.rm=TRUE))

NRRMSE<-1-(sum(abs((((te[,var]-kc$predict[pos])/te[,var])^2)^1/2)*100, na.rm=TRUE)/sum(te[,var], na.rm=TRUE))




AM<-list("Cell size", cell, "Validation", validation, "Model", modelo, "sill", sill, "range", range, "nugget", nugget, "kappa", kappa, "Probability of the regression between observed and predicted values", Re$coefficients[2,4],
"r-squared", Re$r.squared,  "Normalized mean absolute error (NMAE)", NMAE,
"Normalized root mean square error (NRMSE)", NRMSE, "Nash-Sutcliffe coefficient (E)", E, "Index of agreement", d, "NRMAE", NRMAE, "NRRMSE", NRRMSE)

if(!is.null(file6)){
sink(file6)
print(AM)
sink()
}


if(is.null(XLABP)){
xlab<-paste("Observed values of", var)
}
else{
xlab<-XLABP
}

if(is.null(YLABP)){
ylab<-paste("Predicted values of", var)
}
else{
ylab<-YLABP
}


####Variogram
variog<-geoR::variog(dataGeo)
plot(variog$u, variog$v, xlim=c(0, max(variog$u, na.rm=TRUE)), ylim=c(0, max(variog$v, na.rm=TRUE)), xlab="Distance", ylab="Semivariance", pch=19, col="red")
points(varig[,1], varig[,2], col="green", pch=19, xlab="",ylab="")
if(exists("variofitT")==TRUE){
lines(variofitT, col="blue", lwd=2)
}


dev.new()

bold(PAR=PAR)

####Directional variogram

plot(geoR::variog4(dataGeo,max.dist=maxdist, trend=trend.d, direction=direction, unit.angle="degrees"), xlab="Distance", ylab="Semivariance")


ZZ[1,1]<-"4. Plotting predictions"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

dev.new()

bold(PAR=PAR)

if(validation>0){
plot(x=random[,var], y=kc$predict[pos], xlab=xlab, ylab=ylab, pch=16)
}
else{
plot(x=data[,var], y=kc$predict[pos], xlab=xlab, ylab=ylab, pch=16)
}

legend(x="topleft", legend=bquote(italic(r)^2 == .(format(Re$r.squared, digits = 2, decimal.mark=dec))), bty="n", cex=1.4)

Predictions<-data.frame(mm,kc$predict)
names(Predictions)<-c(dataLon,dataLat,var)
Serror<-sqrt(kc$krige.var)
SErrors<-data.frame(mm,Serror)
names(SErrors)<-c(dataLon,dataLat,"Standard.errors")


if(validation>0){
Pre<-data.frame(random, kc$predict[pos])
names(Pre)<-c(names(random), "Prediction")
}
else{
Pre<-data.frame(data, kc$predict[pos])
names(Pre)<-c(names(data), "Prediction")
}

dimPr<-dim(Pre)



Pre<-cbind(Pre[,c(-(dimPr[2]-1), -dimPr[2])], TaylorD)



#####Boxplot


if(lenmo>1){

dev.new()

if(!is.null(COLORB)){
color1<-COLOR
}
else{
color1<-terrain.colors(lenmo)
}


bold(PAR=PAR)

if(!is.null(BOXPLOT)){
boxplotexe<-paste("boxplot(","accu[,'Measure']~accu[,'Model'],",toString(x=BOXPLOT), ")")
eval(parse(text=boxplotexe))
}
else{
boxplotexe<-paste("boxplot(","accu[,'Measure']~accu[,'Model'],",
"xlab=XLABB,", "ylab=YLABB,","col=color1,","outline=OUTLINE", ")")
eval(parse(text=boxplotexe))
}
}


ZZ[1,1]<-"PLOTTING CONTOUR MAP"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


dev.new()

bold(PAR=PAR)


Lati<-(maxLat+minLat)/2
if (pro==TRUE) aspe=(1/cos(Lati*pi/180)) else aspe=1

if(is.null(XLAB)){
xlab<-"Longitude"
}
else{
xlab<-XLAB
}

if(is.null(YLAB)){
ylab<-"Latitude"
}
else{
ylab<-YLAB
}



if(!is.null(Area) & convex==TRUE){

if (Area=="World") {
AA<-data.frame(adworld$Lon,adworld$Lat)
}
else{
AA<-data.frame(adworld1$Lon,adworld1$Lat)
}

names(AA)<-c("Lon", "Lat")

if(is.null(XLIM)){
maxLon<-max(AA[,1], na.rm=TRUE)
minLon<-min(AA[,1], na.rm=TRUE)
}
else{
minLon<-XLIM[1]
maxLon<-XLIM[2]
}

if(is.null(YLIM)){
minLat<-min(AA[,2], na.rm=TRUE)
maxLat<-max(AA[,2], na.rm=TRUE)
}
else{
minLat<-YLIM[1]
maxLat<-YLIM[2]
}

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)
plot(0,0, xlim=c(minLon,maxLon), ylim=c(minLat, maxLat), xlab=xlab, ylab=ylab, type="n", main=MAIN)

}
else{

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)
plot(0,0, xlim=c(minLon,maxLon), ylim=c(minLat, maxLat), xlab=xlab, ylab=ylab, type="n", main=MAIN)

}


if(is.null(ZLIM)){
zlim<-c(min(kc$predict, na.rm=TRUE),max(kc$predict, na.rm=TRUE))
}
else{
zlim<-ZLIM
}

zt<-paste("c(",zlim[1], ",", zlim[2],"),", sep="")
zt<-noquote(zt)

sumgrid<-1

if(!is.null(Area) & convex==TRUE){
polygon(AA$Lon,AA$Lat,col=COLORM, border="black")
}


while(zh<lenA){

if(any(is.na(polygonB))==TRUE){

if(zh==-1){
polygontemp<-polygonB[1:(nas[zh+2]-1),]
}
if(zh==(lenA-1)){
polygontemp<-polygonB[(nas[zh+1]+1):dim[1],]
}
if(zh>-1 & zh<(lenA-1)){
polygontemp<-polygonB[(nas[zh+1]+1):(nas[zh+2]-1),]
}
}


nombres<-names(data)
Lat<-which(nombres==dataLat)
Lon<-which(nombres==dataLon)
Variable<-which(nombres==var)
dataGeo<-geoR::as.geodata(data, coords.col =c(Lon, Lat), data.col = Variable)
dimpoly<-dim(polygontemp)

if(dimpoly[1]>2){

maxpoLon<-max(polygontemp[, polyLon], na.rm=TRUE)
minpoLon<-min(polygontemp[, polyLon], na.rm=TRUE)
maxpoLat<-max(polygontemp[, polyLat], na.rm=TRUE)
minpoLat<-min(polygontemp[, polyLat], na.rm=TRUE)
fila<-which((data.grid[,"Lon"] >= minpoLon) &  (data.grid[,"Lon"] <= maxpoLon) & (data.grid[,"Lat"] >= minpoLat) &  (data.grid[,"Lat"] <= maxpoLat))
borrar<-data.grid[fila,]
logT<-mgcv::in.out(as.matrix(polygontemp[,c(polyLon, polyLat)]),as.matrix(data.grid[, c("Lon", "Lat")]))
remove(borrar)
ver<-which(logT==TRUE)

if(any(logT)==TRUE & length(ver)>1){

data.kc<-geoR::krige.conv(dataGeo, loc=data.grid, bord=polygontemp[,c(polyLon,polyLat)],
krige= geoR::krige.control(beta=beta, type.krige=type.krige, trend.d = trend.d, trend.l = trend.l,  kappa=kappa, nugget=nugget, cov.model=modelo, cov.pars=c(sigmasq=sill, phi=range))) 



exe<-paste("image(x=data.kc, add=TRUE,", "zlim=", zt, "col=", color,  ")")
eval(parse(text=exe))


####Contour

if(contour==TRUE){

nlevels<-paste(NLEVELS,",", sep="")
labcex<-paste(LABCEX,",", sep="")
nlevels<-noquote(nlevels)
labcex<-noquote(labcex)
colorc<-paste("'", COLORC, "'", ",", sep="")
colorc<-noquote(colorc)

exe<-paste("contour(x=data.kc,",  "col=", colorc, "nlevels=", nlevels, "labcex=", labcex, "add=TRUE,", "zlim=", zt, ")")
eval(parse(text=exe))

}

}###End logT

}###End dimpoly


zh<-zh+1


}#End while


##Color legend

if (xl==0){
x1<-(maxLon-minLon)*7/100+maxLon
x2<-(maxLon-minLon)*12/100+maxLon
}
else{
x1<-xl
x2<-xr
}

if(is.null(ZLIM)){
legend.max=max(Predictions[,var], na.rm=TRUE)
legend.min=min(Predictions[,var], na.rm=TRUE)
}
else{
legend.max<-ZLIM[2]
legend.min<-ZLIM[1]
}

legend.freq=abs((legend.max-legend.min)/(breaks-1))
iniF<-legend.min
if(legend.max<=10){
sequ<-(seq(legend.min,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{

if(iniF==0){
legend.freq=abs((legend.max-iniF)/(breaks-1))
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}

}


exe<-paste("color.legend(xl=x1, yb=minLat, xr= x2, yt=maxLat, sequ, gradient='y', align='rb',cex=1,", "rect.col=", color, ")")
eval(parse(text=exe))

par(tmp)


####Standard error

if(error==TRUE){

dev.new()

bold(PAR=PAR)

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)
plot(0,0, xlim=c(minLon,maxLon), ylim=c(minLat, maxLat), xlab=xlab, ylab=ylab, type="n", main="Standard errors")

zh<-(-1)

error<-sqrt(kc$krige.var)

zlim<-c(min(error, na.rm=TRUE),max(error, na.rm=TRUE))

zt<-paste("c(",zlim[1], ",", zlim[2],"),", sep="")
zt<-noquote(zt)

sumgrid<-1

if(!is.null(Area) & convex==TRUE){
polygon(AA$Lon,AA$Lat,col=COLORM, border="black")
}


while(zh<lenA){

if(any(is.na(polygonB))==TRUE){

if(zh==-1){
polygontemp<-polygonB[1:(nas[zh+2]-1),]
}
if(zh==(lenA-1)){
polygontemp<-polygonB[(nas[zh+1]+1):dim[1],]
}
if(zh>-1 & zh<(lenA-1)){
polygontemp<-polygonB[(nas[zh+1]+1):(nas[zh+2]-1),]
}
}


nombres<-names(data)
Lat<-which(nombres==dataLat)
Lon<-which(nombres==dataLon)
Variable<-which(nombres==var)
dataGeo<-geoR::as.geodata(data, coords.col =c(Lon, Lat), data.col = Variable)
dimpoly<-dim(polygontemp)

if(dimpoly[1]>2){

maxpoLon<-max(polygontemp[, polyLon], na.rm=TRUE)
minpoLon<-min(polygontemp[, polyLon], na.rm=TRUE)
maxpoLat<-max(polygontemp[, polyLat], na.rm=TRUE)
minpoLat<-min(polygontemp[, polyLat], na.rm=TRUE)
fila<-which((data.grid[,"Lon"] >= minpoLon) &  (data.grid[,"Lon"] <= maxpoLon) & (data.grid[,"Lat"] >= minpoLat) &  (data.grid[,"Lat"] <= maxpoLat))
borrar<-data.grid[fila,]
logT<-mgcv::in.out(as.matrix(polygontemp[,c(polyLon, polyLat)]),as.matrix(data.grid[, c("Lon", "Lat")]))
remove(borrar)
ver<-which(logT==TRUE)

if(any(logT)==TRUE & length(ver)>1){

data.kc<-geoR::krige.conv(dataGeo, loc=data.grid, bord=polygontemp[,c(polyLon,polyLat)],
krige= geoR::krige.control(beta=beta, type.krige=type.krige, trend.d = trend.d, trend.l = trend.l,  kappa=kappa, nugget=nugget, cov.model=modelo, cov.pars=c(sigmasq=sill, phi=range))) 



errores<-sqrt(data.kc$krige.var)

exe<-paste("image(x=data.kc, val=errores, add=TRUE,", "zlim=", zt, "col=", color,  ")")
eval(parse(text=exe))


}###End logT

}###End dimpoly


zh<-zh+1


}#End while


##Color legend

if (xl==0){
x1<-(maxLon-minLon)*7/100+maxLon
x2<-(maxLon-minLon)*12/100+maxLon
}
else{
x1<-xl
x2<-xr
}

legend.max=max(Serror, na.rm=TRUE)
legend.min=min(Serror, na.rm=TRUE)

legend.freq=abs((legend.max-legend.min)/(breaks-1))
iniF<-legend.min
if(legend.max<=10){
sequ<-(seq(legend.min,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{

if(iniF==0){
legend.freq=abs((legend.max-iniF)/(breaks-1))
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}

}


exe<-paste("color.legend(xl=x1, yb=minLat, xr= x2, yt=maxLat, sequ, gradient='y', align='rb',cex=1,", "rect.col=", color, ")")
eval(parse(text=exe))




par(tmp)

}





####Save the files

if(dec=="."){
write.csv(x=Pre,file = file1, fileEncoding = "", row.names=row.names,na=na)
write.csv(x=Predictions,file = file2, fileEncoding = "", row.names=row.names,na=na)
write.csv(x=accu,file = file3, fileEncoding = "", row.names=row.names,na=na)
write.csv(x=varig,file = file4, fileEncoding = "", row.names=row.names,na=na)
write.csv(x = SErrors,file = file5, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = Pre,file = file1, fileEncoding = "", row.names=row.names,na=na)
write.csv2(x = Predictions,file = file2, fileEncoding = "", row.names=row.names,na=na)
write.csv2(x = accu,file = file3, fileEncoding = "", row.names=row.names,na=na)
write.csv2(x = varig,file = file4, fileEncoding = "", row.names=row.names,na=na)
write.csv2(x = SErrors,file = file5, fileEncoding = "", row.names=row.names,na=na)
}




}
