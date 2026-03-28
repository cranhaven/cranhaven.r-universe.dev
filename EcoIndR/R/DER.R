DER<-function(data, Samples, Species, Taxon, TaxonFunc=NULL, TaxonPhyl=NULL, pos=NULL,
varSize="Richness", varColor="Rarity.G", Index=NULL, corr="sqrt",
palette= "heat.colors", size=c(1,5), digitsS=1, digitsC=2, ncolor=100,
transparency=1, references=TRUE, a=1.5, q=2.5, ResetPAR=TRUE, PAR=NULL, dbFD=NULL, LEGENDS=NULL,
TEXT=NULL,COLOR=c("#EEC591FF", "black", "grey50"),
file1="Diversity indices.csv", file2="Polar coordinates.csv",
file3="Indices and area of the polygon.csv", na="NA", dec=",", row.names=FALSE, save=TRUE){



Arrowhead <- function(x0, y0, angle=0, arr.length=0.4,
  arr.width=arr.length/2, arr.adj=0.5, arr.type="curved",
  lcol="black", lty=1, arr.col=lcol, arr.lwd = 2, npoint = 5, ...) {


  if (arr.type=="none") {
    return()
  }

  if ( arr.type=="curved") { # composed as section of circels

    rad <- 0.7                                        
    len <- 0.25*pi
    mid <- c(0,rad)

    x   <- seq(1.5*pi+len,1.5*pi,length.out=npoint)
    rr  <- cbind(mid[1]-rad*cos(x),mid[2]+rad*sin(x)) 
    mid <- c(0,-rad)
    x   <- rev(x)
    rr  <- rbind(rr,cbind(mid[1]-rad*cos(x),mid[2]-rad*sin(x)))
    mid <-c(rr[nrow(rr),1],0)
    rd  <-rr[1,2]
    x   <-seq(pi/2,3*pi/2,length.out=3*npoint)       
    rr <- rbind(rr,cbind(mid[1]-rd*0.25*cos(x),mid[2]-rd*sin(x)))
    rr[,1] <- rr[,1]*2.6
    rr[,2] <- rr[,2]*3.45
  } else

  if (arr.type=="triangle") {
    x   <- c(-0.2,0.0,-0.2)
    y   <- c(-0.1,0.0,0.1)
    rr  <- 6.22*cbind(x,y)
  } else

  if (arr.type %in% c("circle","ellipse") )  {

    if (arr.type=="circle")
      arr.width=arr.length
    rad <- 0.1               
    mid <- c(-rad,0)
    x<- seq(0,2*pi,length.out=15*npoint)
    rr <- 6.22*cbind(mid[1]+rad*sin(x),mid[2]+rad*cos(x))
  }

  if(arr.adj == 0.5)
    rr[,1] <- rr[,1]-min(rr[,1])/2
  if(arr.adj == 0)
    rr[,1] <- rr[,1]-min(rr[,1])

  user <- par("usr")
  pcm  <- par("pin")*2.54

  sy<- (user[4]-user[3])/pcm[2]
  sx<- (user[2]-user[1])/pcm[1]
  nr <- max(length(x0),length(y0),length(angle),
            length(arr.length),length(arr.width),
            length(lcol),length(lty),length(arr.col))
  if (nr>1) {
    x0         <- rep(x0        ,length.out=nr)
    y0         <- rep(y0        ,length.out=nr)
    angle      <- rep(angle     ,length.out=nr)
    arr.length <- rep(arr.length,length.out=nr)
    arr.width  <- rep(arr.width,length.out=nr)
    lcol       <- rep(lcol      ,length.out=nr)
    lty        <- rep(lty       ,length.out=nr)
    arr.col    <- rep(arr.col   ,length.out=nr)
  }
  RR<-rr
  for (i in 1:nr) {
  ## rotation around midpoint
    dx <- rr[,1]*arr.length [i]
    dy <- rr[,2]*arr.width  [i]

    angpi <- angle[i] / 180 *pi
    cosa  <-cos(angpi)
    sina  <-sin(angpi)

    RR[,1]<-  cosa*dx-sina*dy
    RR[,2]<-  sina*dx+cosa*dy

## rescaling and transposing
    RR[,1]<- x0[i] +RR[,1]*sx
    RR[,2]<- y0[i] +RR[,2]*sy

## drawing...
    polygon(RR,col=arr.col[i],border=lcol[i],lty=lty[i],lwd=arr.lwd, ...)
  }
}

rotatexy   <- function (xy, angle, mid=colMeans(xy), asp=FALSE) {

  xy    <- matrix(ncol=2,data=xy)
  angpi <- angle / 180 *pi
  cosa  <-cos(angpi)
  sina  <-sin(angpi)

  dx    <- xy[,1] - mid[1]
  dy    <- xy[,2] - mid[2]

  ex    <-mid[1] + cosa*dx-sina*dy
  ey    <-mid[2] + sina*dx+cosa*dy

  if (asp) {
    user <- par("usr")
    pin  <- par("pin")
    sy   <- user[4]-user[3]
    sx   <- user[2]-user[1]
    ey   <- mid[2] + (ey -mid[2])*sy/sx*pin[1]/pin[2]
  }

  return(cbind(ex,ey))
}
Arrows <- function(x0, y0, x1, y1, code=2,
  arr.length=0.4, arr.width=arr.length/2, arr.adj=0.5,
  arr.type="curved", segment=TRUE, col="black", lcol=col, lty=1,
  arr.col=lcol, lwd = 1, arr.lwd = lwd, ...)  {

  if (arr.type=="simple") {
    arrows(x0,y0,x1,y1,code=code,length=arr.length/2.54,
           lty=lty, col=col, lwd=lwd, ...)
    return()
  }
  if (arr.type=="none") {
    return()
  }
  if (arr.type=="T") {
    arrows(x0,y0,x1,y1,code=code,length=arr.length/(2*2.54),
           lty=lty, angle=90, col=col, lwd=lwd,  ...)
    return()
  }

  ## draw segment
  if (segment)                                
    segments(x0,y0,x1,y1,col=lcol,lty=lty,lwd=lwd,...)

  ## scaling factor
  user<-par("usr")
  pin <-par("pin")
  pin <- pin/max(pin)
  sy<- (user[4]-user[3]) /pin[2]
  sx<- (user[2]-user[1]) /pin[1]

  ## code = 2
  angle<- atan((y1-y0) /(x1-x0) *sx/sy)/pi*180
  angle[is.nan(angle)]<-0
  angle [x1<x0] <-180+angle[x1<x0]
  xx<-x1
  yy<-y1
  if (sy < 0 & sx < 0) 
    angle <- angle + 180
  else if (sx < 0) 
    angle <- angle + 180
  
  ## code =3 draws two arrowheads
  if (code == 3)
    Arrowhead(x0=xx,y0=yy,angle=angle,
              lcol=lcol,arr.col=arr.col,arr.adj=arr.adj,
              lty=lty,arr.length=arr.length,arr.width=arr.width,
              arr.type=arr.type,arr.lwd=arr.lwd, ...)

  if (code != 2) {
    angle <-180 + angle
    xx<-x0
    yy<-y0
  }

  Arrowhead(x0=xx,y0=yy,angle=angle,lcol=lcol,arr.col=arr.col,
            arr.adj=arr.adj,lty=lty,arr.length=arr.length,
            arr.width=arr.width,arr.type=arr.type,arr.lwd=arr.lwd, ...)
}


getellipse <- function (rx=1, ry=rx, mid=c(0,0), dr=0.01,
  angle=0, from=-pi, to=pi) {

  dr <- abs(dr)
  if (to < from) to <- 2*pi + to
  x  <- c( seq(from,to,by=dr), to)
  if (x[length(x)] == x[length(x)-1])
    x <- x[-length(x)]
  xy <- cbind( mid[1] + rx * cos(x), mid[2] + ry * sin(x))

  if (angle != 0)
    xy <- rotatexy (xy, angle=angle, mid=mid)  # rotate around mid
  return(xy)
}

plotellipse <- function (rx=1, ry=0.2, mid=c(0,0), dr=0.01,
  angle=0, from=-pi, to=pi, type="l", lwd=2, lcol="black",
  col=NULL, arrow=FALSE, arr.length=0.4, arr.width=arr.length*0.5,
  arr.type="curved", arr.pos=1, arr.code=2, arr.adj=0.5,
  arr.col="black",  ...) {


  xy<-getellipse (rx,ry,mid,angle=angle,dr=dr,from=from,to=to)

  if (! is.null(col))
    polygon(xy,col=col,border=NA)
  if (type != "n" )
    lines(xy,type=type,lwd=lwd,col=lcol,...)
  nr <- nrow(xy)

  if (arrow) {
    ilen <- length(arr.pos)
    if (ilen>1) {
      arr.code  <- rep(arr.code  ,length.out=ilen)
      arr.col   <- rep(arr.col   ,length.out=ilen)
      arr.length<- rep(arr.length,length.out=ilen)
      arr.width <- rep(arr.width ,length.out=ilen)
      arr.type  <- rep(arr.type  ,length.out=ilen)
      arr.adj   <- rep(arr.adj   ,length.out=ilen)
    }

    for (i in 1: ilen) {
      ii <- max(2,trunc(nr*arr.pos[i]))
      Arrows(xy[ii-1,1], xy[ii-1,2], xy[ii,1], xy[ii,2],
            lcol=arr.col[i], code=arr.code[i], arr.col=arr.col[i],
            arr.length =arr.length[i], arr.width=arr.width[i],
            arr.type=arr.type[i], arr.adj=arr.adj[i])
    }
  }
}

#####Checking data required
if(!is.null(Index) & (length(Index)!=4 & length(Index)!=5)){
stop("The number of indices in the argument Index must be 4 or 5")
}

if(!is.null(TaxonPhyl)){
if(file.exists(TaxonPhyl)==FALSE){
stop("The file of TaxonPhyl does not exist in the working directory")
}
}

if(length(COLOR)!=3){
stop("The number of colors in the argument COLOR must be 3")
}


###All the samples with species
dati<-data[,Samples]
col<-apply(X = dati , MARGIN = 2 , FUN = sum , na.rm=TRUE)
col<-which(col==0)
if(length(col)>0){
stop(paste("There are no records of species in the following samples:", paste(names(col), collapse=", ")))
}

###All species with records
dati<-data[,Samples]
col<-apply(X = dati , MARGIN = 1 , FUN = sum , na.rm=TRUE)
pos1<-which(col==0)
if(length(pos1)>0){
sps<-as.character(data[pos1,Species])
stop(paste("There are no records of the following species:", paste(sps, collapse=", ")))
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


####Function for calculating the area of the polar coordinates
cha<-function(x,y){
chull(x,y)->i
return(splancs::areapl(cbind(x[i],y[i])))
}

##########Function time() to estimate remaining time
time<-function(t, from, to, c31="", c32="", c41="", c42=""){
ZZ<-matrix(rep("",8),nrow=4) 
ZZ[3,1]<-c31; ZZ[3,2]<-c32
ZZ[4,1]<-c41; ZZ[4,2]<-c42

end.time<-Sys.time() 
end.times<- format(end.time, "%b %d, %Y at %X")
run.time<-difftime(end.time,begin.time,units="secs")
run<-as.numeric(run.time)
run1<-(to-t)*run/(t-from)

if(!is.na(run1)){
if(run1>=3600){
ZZ[2,2]<-"remaining hours...."
}
else{
if(run1<=60) ZZ[2,2]<-"remaining seconds...." else ZZ[2,2]<-"remaining minutes...."
}

if(run1>=3600){
minutes<-run1/3600
}
else{
if(run1<=60) minutes<-run1 else minutes<-run1/60 
}
minutes<-round(minutes, digits=1)
if(minutes==Inf){
ZZ[1,1]<-end.times
ZZ[2,1]<-"It is not possible to estimate remaining time...." 
ZZ[2,2]<-""
}
else{
ZZ[1,1]<-end.times
ZZ[2,1]<-minutes
}
}
else{
ZZ[1,1]<-end.times
ZZ[2,1]<-"It is not possible to estimate remaining time...." 
ZZ[2,2]<-""
}
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
}
##########End time()



####Function for bubble plot
Bubble<-function(data, varY, varX, varSize=NULL, varColor=NULL, palette= palette, size=c(1,5),
digitsS=0, digitsC=1, ncolor=100, transparency=1,  LEGENDS=NULL,TaxonFunc=TaxonFunc){

datos<-data

datosT<-data.frame(subset(datos, select=varX), subset(datos, select=varY))

if(!is.null(varSize)){
datosT<-data.frame(datosT, subset(datos, select=varSize))
}

if(!is.null(varColor)){
datosT<-data.frame(datosT, subset(datos, select=varColor))
}

datos<-na.exclude(datosT)

if(!is.null(varSize)){
maxS<-max(datos[,varSize])
minS<-min(datos[,varSize])
matriz<-matrix(c(minS,maxS, size[1], size[2]),nrow = 2 , ncol = 2)
regS<-lm(matriz[,2]~matriz[,1])
}

if(!is.null(varColor)){
maxC<-max(datos[,varColor])
minC<-min(datos[,varColor])
matriz<-matrix(c(maxC,minC, 1, ncolor),nrow = 2 , ncol = 2)
regC<-lm(matriz[,2]~matriz[,1])
}


if(!is.null(varSize)) cex<-regS$coefficients[1]+regS$coefficients[2]*datos[1,varSize] else cex<-1

if(!is.null(varColor)){
color<-round(regC$coefficients[1]+regC$coefficients[2]*datos[1,varColor])
pch<-16
}
else{
rampa<-"black"
pch<-1
color<-1
}

if(is.null(TaxonFunc)){
plot(x=datos[1,varX],y=datos[1,varY],cex=cex,col=rampa[color], xlim=c(-1.41,1.41), ylim=c(-0.6,3.67), xlab='',ylab='',pch=pch,axes=FALSE)
}
else{
plot(x=datos[1,varX],y=datos[1,varY],cex=cex,col=rampa[color], xlim=c(-1.77,1.77), ylim=c(-0.5,4.34), xlab='',ylab='',pch=pch,axes=FALSE)
}

dimS<-dim(datos)
for(zz in 2:dimS[1]){
if(!is.null(varSize)) cex<-regS$coefficients[1]+regS$coefficients[2]*datos[zz,varSize] else cex=1
if(!is.null(varColor)){
color<-round(regC$coefficients[1]+regC$coefficients[2]*datos[zz,varColor])
}

if(is.null(TaxonFunc)){
points(x=datos[zz,varX],y=datos[zz,varY],cex=cex/1.5,col=rampa[color],xlim=c(-1.41,1.41), ylim=c(-0.6,3.67),pch=pch)
}
else{
points(x=datos[zz,varX],y=datos[zz,varY],cex=cex/1.5,col=rampa[color],xlim=c(-1.77,1.77), ylim=c(-0.5,4.34),pch=pch)
}
}

ranX<-2.4
ranY<-3.6

if(!is.null(LEGENDS)){
legendexe<-paste("legend(",toString(x=LEGENDS), ")")
eval(parse(text=legendexe))
}
else{
if(!is.null(varSize)){
val<-format((size[1]-regS$coefficients[1])/regS$coefficients[2],digits=digitsS)
for(zz in (size[1]+1):size[2]){
val<-append(val,format((zz-regS$coefficients[1])/regS$coefficients[2],digits=digitsS))
}

hh<-0
for(zz in size[1]:size[2]){
hh<-hh+1
if(as.numeric(val[size[2]])>1000) mul<-10 else mul<-8
if(is.null(TaxonFunc)){
points(-1.2+ranX*((-size[1]+zz)*mul)/100, 3.4+abs(3*1/100)*(-1), cex=zz/1.5, xlim=c(-1.41,1.41), ylim=c(-0.6,3.67),
col=COLOR[2], pch=16)
text(-1.2+ranX*((-size[1]+zz)*mul)/100, 3.5+ranY*9/100*(-1),label=val[hh])
}
else{
points(-1.55+ranX*((-size[1]+zz)*mul)/100, 4.1+abs(3*1/100)*(-1), cex=zz/1.5, xlim=c(-1.77,1.77), ylim=c(-0.5,4.34),
col=COLOR[2], pch=16)
text(-1.55+ranX*((-size[1]+zz)*mul)/100, 4.2+ranY*9/100*(-1),label=val[hh])
}

}
}
}

if(!is.null(varColor)){
int<-as.numeric(format((maxC-minC)/5, digits=digitsC))
maxC<-as.numeric(format(maxC,digits=digitsC))
minC<-as.numeric(format(minC,digits=digitsC))
color<-minC
valor<-minC
for(zz in 1:5){
valor<-valor+int
color<-append(color,valor)
}
if(is.null(TaxonFunc)){
color.legend(xl=-1.2,yb=3.8,xr=1.2,yt=3.9,
legend=color,gradient='x',align='rb',cex=1,rect.col=rev(rampa))
}
else{
color.legend(xl=-1.55,yb=4.5,xr=1.55,yt=4.6,
legend=color,gradient='x',align='rb',cex=1,rect.col=rev(rampa))
}
}


}####End function Bubble


if(ResetPAR==TRUE){

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar()) }
else{
}

if(!is.null(PAR)){
parexe<-paste("par(new,",toString(x=PAR), ")")
eval(parse(text=parexe))
}
else{
par(font.lab=2, mar=c(1,1,1,1),cex.lab=1.5)
}


if(palette== "heat.colors"){
rampa<-heat.colors(n=ncolor, alpha=transparency)
}

if(palette== "terrain.colors"){
rampa<-terrain.colors(n=ncolor, alpha=transparency)
}

if(palette== "gray.colors"){
rampa<-gray.colors(n=ncolor, alpha=transparency)
}


if(palette== "topo.colors"){
rampa<-topo.colors(n=ncolor, alpha=transparency)
}


if(palette== "cm.colors"){
rampa<-cm.colors(n=ncolor, alpha=transparency)
}

#####Make species unique
sps<-data[,Species]

if(any(grepl("-", sps)==TRUE)==TRUE){
sps<-sub("-", "",sps)
}

sps<-as.character(sps)

if(anyDuplicated(sps)>0){
sps<-make.unique(sps, sep= "")
}

data[,Species]<-sps



####Samples selection

datos<-t(data[,Samples])
colnames(datos)<-data[,Species]

###Total abundance per sample
Abu<-apply(X = datos , MARGIN = 1 , FUN = sum, na.rm=TRUE)

###Maximum per sample
Max<-apply(X = datos , MARGIN = 1 , FUN = max, na.rm=TRUE)

###Total abundance to the square per sample
dasq<-datos^2
Dsq<-apply(X = dasq , MARGIN = 1 , FUN = sum, na.rm=TRUE)

###Richness
Richness<-rowSums(datos != 0, na.rm=TRUE)

###Shannon
S1<-sweep(datos, 1, Abu, "/")
S2<-S1*log(S1)
Shannonln<--apply(X = S2 , MARGIN = 1 , FUN = sum, na.rm=TRUE)
S2<-S1*log(S1,base=2)
Shannonlog<--apply(X = S2 , MARGIN = 1 , FUN = sum, na.rm=TRUE)


###Rarity.G
R1<-datos
R1[R1==0]<-NA
R1[!is.na(R1)]<-1
R2<-apply(X = R1 , MARGIN = 2 , FUN = sum, na.rm=TRUE)
R3<-R2/length(Samples)
R4<-sweep(R1, 2, R3, "*")
R5<-apply(X = R4 , MARGIN = 1 , FUN = mean, na.rm=TRUE)
Rarity.G<-1-R5

###Rarity.O
R1<-datos
R1[R1==0]<-NA
R6<-R1
R6[!is.na(R6)]<-1
suma<-sum(R1, na.rm=TRUE)
R2<-apply(X = R1 , MARGIN = 2 , FUN = sum, na.rm=TRUE)
R3<-R2/suma
R4<-sweep(R6, 2, R3, "*")
R5<-apply(X = R4 , MARGIN = 1 , FUN = mean, na.rm=TRUE)
Rarity.O<-1-R5


###Rarity Leroy
rarity.weights <- Rarity::rWeights(R2)
R1[is.na(R1)]<-0
TR1<-t(R1)
Irr<-Rarity::Irr(TR1, rarity.weights)


###Simpson
D1<-sweep(datos, 1, Abu, "/")^2
D2<-apply(X = D1 , MARGIN = 1 , FUN = sum, na.rm=TRUE)
D2[is.na(D2)]<-1
Simpson1<-1-D2
Simpson2<-1/D2

###Simpson evenness
SE<-Simpson1/Richness

###Margalef
Mar<-(Richness-1)/(log(Abu))
Mar[is.na(Mar)]<-0

###Menhinick
Men<-Richness/sqrt(Abu)

###Berger-Parker
B1<-Max/Abu
Ber<-1/B1

###McIntosh
MCI<-(Abu-sqrt(Dsq))/(Abu-sqrt(Abu))
MCI[is.na(MCI)]<-0


###Brillouin index
Hb<- function(ns) {
N<-sum(ns, na.rm=TRUE)
(lfactorial(N) - sum(lfactorial(ns), na.rm=TRUE))/N
}
Bri<-apply(X = datos , MARGIN = 1 , FUN = Hb)

#Fisher alpha
datii<-datos
datii[is.na(datii)]<-0
verdad<-any(lapply(datii,class)!="integer")
if(verdad==FALSE){
FA<-vegan::fisher.alpha(datii)
FA[FA>100]<-0
}

#Pielou
Pie<-Shannonln/log(Richness)
Pie[is.na(Pie)]<-0

#Renyi
Re<-as.numeric(vegan::renyi(x = datii, scale=a))

#Hill.Renyi
HiRe<-exp(Re)

#Tsallis
Ts<-as.numeric(vegan::tsallis(x = datii, scale=q))

#Hill.Tsallis
HiTs<-(1-(q-1)*Ts)^(1/(1-q))

#Hill evenness
HE2<-as.numeric(vegan::renyi(x = datii, scale=2))
HE1<-as.numeric(vegan::renyi(x = datii, scale=1))
HE2<-exp(HE2)
HE1<-exp(HE1)
HE<-HE2/HE1

###McIntosh evenness
McE<-(Abu-sqrt(Dsq))/(Abu-(Abu/sqrt(Richness)))
McE[is.na(McE)]<-0

###Heip 
Hei<-(exp(Shannonln)-1)/(Richness-1)
Hei[is.na(Hei)]<-0


###Camargo eveness from Zhou Xiang
camargo <- function(n_spec, include_zeros = T)
{
  if (include_zeros) n <- n_spec else n <- n_spec[n_spec > 0]
  S <- length(n)
  temp<-0
  for (i in 1:(S - 1))
  {
    temp<-temp+sum(abs(n[(i+1):S]-n[i]))
  }
  camar <-1-temp/(sum(n)*S)
  return(camar)
}

cam<-datos
cam[is.na(cam)]<-0
camargo<-apply(X = cam, MARGIN = 1 , camargo )

###Smith and Wilsons Index of Evenness
SW1<-apply(X = datos , MARGIN = 1 , FUN = sum, na.rm=TRUE)
SW2<-SW1/Richness
SW3<-as.matrix(log(datos))
SW3[is.infinite(as.matrix(SW3))]<-NA
SW4<-(sweep(SW3, 1, SW2, "-"))^2
SW5<-apply(X = SW4 , MARGIN = 1 , FUN = sum, na.rm=TRUE)
SW6<-SW5/Richness
Evar<-(1-(2/pi)*atan(SW6))


###Taxonomic diversity
dtaxa<-data[,Taxon]
rownames(dtaxa)<-data[,Species]
taxdis <- vegan::taxa2dist(dtaxa, varstep=TRUE)
taxon<-vegan::taxondive(datii, taxdis)
taxon<-data.frame(taxon$D,taxon$Dstar)
taxon[is.na(taxon)]<-0

###Final results

if(verdad==FALSE){
results<-data.frame(rownames(datos), Abu,Irr[,1],Rarity.G, Rarity.O, Richness,Shannonlog,Shannonln, FA,Simpson1,Simpson2,Bri, Mar,
Re, Men,MCI,Ber,HiRe,HiTs,Ts,SE,Pie, McE, HE, Hei, camargo, Evar, taxon[,1], taxon[,2])

names(results)<-c("Samples", "Abundances","Rarity.Leroy","Rarity.G", "Rarity.O", "Richness","S.W.LOG2","S.W","Fisher","Simpson", "InvSimpson",
"Brillouin","Margalef", "Renyi", "Menhinick", "McIntosh","InvB.P", "Hill.Renyi","Hill.Tsallis","Tsallis","SimpsonE","PielouE",
"McIntoshE",  "HillE",  "HeipE","CamargoE", "Evar","D","Dstar")
}
else{
results<-data.frame(rownames(datos), Abu,Irr[,1], Rarity.G, Rarity.O, Richness,Shannonlog,Shannonln,Simpson1,Simpson2,Bri, Mar,
Re, Men,MCI,Ber,HiRe,HiTs,Ts,SE,Pie, McE, HE, Hei, camargo, Evar, taxon[,1], taxon[,2])

names(results)<-c("Samples", "Abundances","Rarity.Leroy", "Rarity.G", "Rarity.O", "Richness","S.W.LOG2","S.W","Simpson", "InvSimpson",
"Brillouin","Margalef", "Renyi", "Menhinick", "McIntosh","InvB.P", "Hill.Renyi","Hill.Tsallis","Tsallis","SimpsonE","PielouE",
"McIntoshE",  "HillE",  "HeipE","CamargoE", "Evar","D","Dstar")
}


if(!is.null(TaxonPhyl)){

datos[is.na(datos)]<-0

aa<-load(file=TaxonPhyl)
exe<-paste("names(",aa,")")
names<-eval(parse(text=exe))
if(any(names=="phylo")==TRUE){
aa<-paste(aa,"$","phylo", sep="")
}

#Faith phylogenetic diversity
exe<-paste("picante::pd(","datos,",aa,")")
PD<-eval(parse(text=exe))

#Mean pairwise phylogenetic distance
exe<-paste("cophenetic(",aa,")")
cop<-eval(parse(text=exe))
exe<-paste("picante::mpd(","datos,","cop,", "abundance.weighted=TRUE", ")")
MPD<-eval(parse(text=exe))

#Mean nearest taxon distance
exe<-paste("picante::mntd(","datos,","cop,", "abundance.weighted=TRUE", ")")
MNTD<-eval(parse(text=exe))

#Phylogenetic species variability
exe<-paste("picante::psv(","datos,",aa,",compute.var=TRUE", ")")
PSV<-eval(parse(text=exe))

#Phylogenetic species richness
exe<-paste("picante::psr(","datos,",aa,",compute.var=TRUE", ")")
PSR<-eval(parse(text=exe))

#Phylogenetic species evenness
exe<-paste("picante::pse(","datos,",aa, ")")
PSE<-eval(parse(text=exe))

#Phylogenetic species clustering
exe<-paste("picante::psc(","datos,",aa, ")")
PSC<-eval(parse(text=exe))

#Quadratic entropy
exe<-paste("picante::raoD(","datos,",aa, ")")
raoD<-eval(parse(text=exe))

nombres<-colnames(results)
results<-data.frame(results, PD[,1], MPD, MNTD, PSV[,1], PSR[,1], PSE[,1], PSC[,1],raoD$Dkk)
names(results)<-c(nombres,"PD", "MPD", "MNTD", "PSV", "PSR", "PSE", "PSCC", "raoD")

}



if(!is.null(TaxonFunc)){

###Functional diversity
dtaxa<-as.data.frame(data[,TaxonFunc])
datos[is.na(datos)]<-0
sps<-data[, Species]
colnames(datos)<-sps
rownames(dtaxa)<-sps

if(!is.null(dbFD)){
splomtexe<-paste("FD::dbFD(",toString(x=dbFD), ")")
FD<-eval(parse(text=splomtexe))
}
else{
splomtexe<-paste("FD::dbFD(","x=dtaxa,", "a=datos,", "corr=corr", ")")
FD<-eval(parse(text=splomtexe))
}

Fundatos<-data.frame(FD$FEve,FD$FDis,FD$RaoQ)
Funnames<-c("FEve","FDis","RaoQ")

if(any(names(FD)=="FRic")==TRUE){
Funnames<-c(Funnames,"FRic")
Fundatos<-data.frame(Fundatos,FD$FRic)
}

if(any(names(FD)=="FDiv")==TRUE){
Funnames<-c(Funnames,"FDiv")
Fundatos<-data.frame(Fundatos,FD$FDiv)
}

if(any(names(FD)=="FGR")==TRUE){
Funnames<-c(Funnames,"FGR")
Fundatos<-data.frame(Fundatos,FD$FGR)
}

fin<-dim(Fundatos)

###Final results
nombres<-colnames(results)
results<-data.frame(results, Fundatos)
names(results)<-c(nombres,Funnames)

}


#Save the diversity indices
if(save==TRUE){
if(dec=="."){
write.csv(x=results,file = file1, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = results,file = file1, fileEncoding = "", row.names=row.names,na=na)
}
}

####Polar coordinates

if(is.null(TaxonFunc)){
angle<-c(36,72,108,144)
cc<-4
#References points
x<-c(0,0, 0.87,1.12,0.87,-0.87,-1.12,-0.87)
y<-c(0,3.077, 0.587786441, 1.5385, 2.4899,2.4899,1.5385, 0.587786441)

if(verdad==FALSE) ini1<-19 else ini1<-18
 ini2<-ini1+1
ini3<-ini2+6
if(is.null(TaxonPhyl)) ini4<-ini3+1 else ini4<-ini3+3
if(is.null(TaxonPhyl)) final<-ini4+1 else final<-ini4+7

if(is.null(Index)){

ind<-data.frame("a","b","c","d",1,1,1)
names(ind)<-c("Index1","Index2","Index3","Index4","Area","Euclidean","Mean")
dist<-0
for(jj in 3:5){
for(yy in 6:ini1){
for(tt in ini2:ini3){
for(hh in ini4:final){

Indices<-c(names(results)[jj], names(results)[yy], names(results)[tt], names(results)[hh])
du<-duplicated(Indices)
if(any(du==TRUE)==TRUE){

}
else{
selection<-data.frame(subset(results, select=Indices[1]), subset(results, select=Indices[2]),
subset(results, select=Indices[3]), subset(results, select=Indices[4]))

#Standardization 0 to 1
a<-dim(selection)

datosE<-selection

for (z in 1:cc){
matrixE<-matrix(c(0, 1, min(selection[,z],na.rm=TRUE),max(selection[,z],na.rm=TRUE)), nrow = 2 , ncol = 2)
reg<-lm(matrixE[,1]~matrixE[,2])
datosC<-reg$coefficients[1]+selection[,z]*reg$coefficients[2]
datosE<-cbind(datosE,datosC)
}

datosE<-datosE[,-c(1:a[2])]

colnames(datosE)<-colnames(selection)

selection<-datosE

#Estimation of polar coordinates


datosX<-selection[,1]
for (z in 1:cc){
datosC<-abs(selection[,z])*cos(angle[z]*3.1416/180)
datosX<-data.frame(datosX,datosC)
}
datosX<-datosX[,-1]

XX<-apply(datosX,1,sum)

datosY<-selection[,1]
for (z in 1:cc){
datosC<- abs(selection[,z])*sin(angle[z]*3.1416/180)
datosY<-cbind(datosY,datosC)
}

datosY<-datosY[,-1]

YY<-apply(datosY,1,sum)

datosF<-data.frame(results[,1],XX,YY)
colnames(datosF)<-c("Samples", "X","Y")

NA1<-is.na(datosF[,2])
NA2<-is.na(datosF[,3])
NA1<-length(NA1[NA1==TRUE])
NA2<-length(NA2[NA2==TRUE])


if(NA1==0 & NA2==0){
###Area of the polygon
uni<-unique(datosF[,2:3])
eu<-cha(x = uni[,1], y = uni[,2])
eucli<-mean(dist(x = uni),na.rm=TRUE)
meanb<-mean(c(eu,eucli))
indi<-data.frame(as.character(Indices[1]),as.character(Indices[2]),as.character(Indices[3]),as.character(Indices[4]),eu, eucli,meanb)
names(indi)<-c("Index1","Index2","Index3","Index4", "Area","Euclidean","Mean")
ind<-rbind(ind,indi)

if(meanb>dist){
IndicesF<-Indices
datosFF<-datosF
dist<-meanb
}

}

}#End duplicate

}#End fourth FOR
}#End third FOR
}#End second FOR
}#End first FOR
}
else{

selection<-data.frame(subset(results, select=Index[1]), subset(results, select=Index[2]),
subset(results, select=Index[3]), subset(results, select=Index[4]))

#Standardization 0 to 1
a<-dim(selection)

datosE<-selection

for (z in 1:cc){
matrixE<-matrix(c(0, 1, min(selection[,z],na.rm=TRUE),max(selection[,z],na.rm=TRUE)), nrow = 2 , ncol = 2)
reg<-lm(matrixE[,1]~matrixE[,2])
datosC<-reg$coefficients[1]+selection[,z]*reg$coefficients[2]
datosE<-cbind(datosE,datosC)
}

datosE<-datosE[,-c(1:a[2])]

colnames(datosE)<-colnames(selection)

selection<-datosE

#Estimation of polar coordinates


datosX<-selection[,1]
for (z in 1:cc){
datosC<-abs(selection[,z])*cos(angle[z]*3.1416/180)
datosX<-data.frame(datosX,datosC)
}
datosX<-datosX[,-1]

XX<-apply(datosX,1,sum)

datosY<-selection[,1]
for (z in 1:cc){
datosC<- abs(selection[,z])*sin(angle[z]*3.1416/180)
datosY<-cbind(datosY,datosC)
}

datosY<-datosY[,-1]

YY<-apply(datosY,1,sum)

datosF<-data.frame(results[,1],XX,YY)
colnames(datosF)<-c("Samples", "X","Y")
IndicesF<-Index

datosFF<-datosF

}


####DER plot

Indices<-IndicesF
datosF<-datosFF

plot(x=0,y=0, cex=0, bty="n", xlim=c(-1.41,1.41), ylim=c(-0.6,3.67), xlab="",
ylab="", axes=FALSE)


plotellipse(rx=1.12,ry=1.5385,mid=c(0,1.5385),lcol='white',col=COLOR[1])



if(is.null(Index)){
vari1<-Indices[1]
vari2<-Indices[2]
vari3<-Indices[3]
vari4<-Indices[4]
}
else{
vari1<-Index[1]
vari2<-Index[2]
vari3<-Index[3]
vari4<-Index[4]
}

if(references==TRUE){points(x=x,y=y, cex=1.2, bty="n", xlim=c(-1.77,1.77), ylim=c(-0.5,4.34), col=COLOR[3], pch=17)}

text<-paste("Max", vari1,  "\n", "Min",vari2,"\n", "Min",vari3,"\n", "Min",vari4 )
text(x = 0.7 , y = 0.3 , labels = text, pos=4, cex=1, offset=4.5)
text<-paste("Max", vari1, "\n", "Max",vari2,"\n", "Min",vari3, "\n","Min",vari4 )
text(x = 1.1 , y = 1.5 , labels = text, pos=4, cex=1, offset=1.2)
text<-paste("Max", vari1,  "\n", "Max",vari2,"\n", "Max",vari3,"\n", "Min",vari4 )
text(x = 0.7 , y = 2.65 , labels = text, pos=4, cex=1, offset=4.5)
text<-paste("Max", vari1, "\n", "Max",vari2,"\n", "Max",vari3, "\n", "Max",vari4 )
text(x = 0 , y = 3.1 , labels = text, pos=3, cex=1)
text<-paste("Min", vari1, "\n", "Max",vari2,"\n", "Max",vari3, "\n", "Max",vari4 )
text(x = -0.85 , y = 2.65 , labels = text, pos=2, cex=1, offset=1)
text<-paste("Min", vari1, "\n", "Min",vari2,"\n", "Max",vari3, "\n", "Max",vari4 )
text(x = -1.1 , y = 1.5 , labels = text, pos=2, cex=1, offset=1.2)
text<-paste("Min", vari1, "\n", "Min",vari2,"\n", "Min",vari3, "\n", "Max",vari4 )
text(x = -0.85 , y = 0.3 , labels = text, pos=2, cex=1, offset=1)
text<-paste("Min", vari1, "\n", "Min",vari2,"\n", "Min",vari3, "\n", "Min",vari4)
text(x = 0, y = 0 , labels = text, pos=1, cex=1)

dtii<-data.frame(datosF, results[,varSize], results[,varColor])
names(dtii)<-c(names(datosF), varSize,varColor)

####Bubble plot
par(new=TRUE)
Bubble(data = dtii , varY = "Y" , varX = "X" , varSize = varSize , varColor = varColor, palette= palette,
size=size, digitsS=digitsS, digitsC=digitsC, ncolor=ncolor, transparency=transparency,  LEGENDS=LEGENDS,
TaxonFunc=NULL)

####Text labels
pos<-data[,pos]
if(is.null(pos)) pos=4 else pos<-pos[!is.na(pos)]

if(!is.null(TEXT)){
textexe<-paste("text(",toString(x=TEXT), ")")
eval(parse(text=textexe))
}
else{
textexe<-paste("text(","x=datosF[,2],", "y=datosF[,3],", "labels=datosF[,1],","pos=pos",  ")")
eval(parse(text=textexe))
}



####Save the polar coordinates
if(save==TRUE){
if(dec=="."){
write.csv(x=datosF,file = file2, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = datosF,file = file2, fileEncoding = "", row.names=row.names,na=na)
}

if(is.null(Index)){
####Save indices and the area
ind<-ind[-1,]
ind<-ind[order(ind[,7] , decreasing = TRUE),]
if(dec=="."){
write.csv(x=ind,file = file3, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = ind,file = file3, fileEncoding = "", row.names=row.names,na=na)
}
}
}


###Area of the polygon

print("Final selection:")
if(is.null(Index)){
print(paste(Indices[1],"/",Indices[2],"/",Indices[3],"/",Indices[4], sep=""))
}
else{
print(paste(Index[1],"/",Index[2],"/",Index[3], "/",Index[4], sep=""))
}
uni<-unique(datosF[,2:3])
eu<-cha(x = uni[,1], y = uni[,2])
eucli<-mean(dist(x = uni),na.rm=TRUE)
meanb<-mean(c(eu,eucli))
print(paste("Area of the polygon:", eu))
print(paste("Euclidean distance:", eucli))
paste("Mean:", meanb)



}
else{
angle<-c(30,60,90,120,150)
cc<-5
x<-c(0,0.866014756,1.366018998,1.366022671,0.866024792,0,-0.866028465,-1.366032706,-1.366032706,-1.366036379,-0.8660385)
y<-c(3.732045342,3.332050644,2.366027689,1.366027689,0.40000106,0,0.399994698,1.366017653,1.366017653,2.366017653,3.332044281)

if(verdad==FALSE) ini1<-20 else ini1<-19
ini2<-ini1+1
ini3<-ini2+6
if(is.null(TaxonPhyl)) ini4<-ini3+1 else ini4<-ini3+3
if(is.null(TaxonPhyl)) ini5<-ini4+1 else ini5<-ini4+7
ini6<-ini5+1
final<-ini6+fin[2]-1


if(is.null(Index)){

ind<-data.frame("a","b","c","d", "e",1,1,1)
names(ind)<-c("Index1","Index2","Index3","Index4", "Index5","Area","Euclidean","Mean")
dist<-0
begin.time<-Sys.time()
hhhh<-0
fff<-0
if(is.null(TaxonPhyl)) fintime<-1960 else fintime<-7840
for(jj in 3:4){
for(yy in 5:ini1){
for(tt in ini2:ini3){
for(hh in ini4:ini5){
for(kk in ini6:final){
hhhh<-hhhh+1
if(hhhh>fff+300){
fff<-hhhh
time(t=hhhh, from=1, to=fintime, c31=paste("","")) 
}
Indices<-c(names(results)[jj], names(results)[yy], names(results)[tt], names(results)[hh], names(results)[kk])
du<-duplicated(Indices)
if(any(du==TRUE)==TRUE){

}
else{
selection<-data.frame(subset(results, select=Indices[1]), subset(results, select=Indices[2]),
subset(results, select=Indices[3]), subset(results, select=Indices[4]), subset(results, select=Indices[5]))

#Standardization 0 to 1
a<-dim(selection)

datosE<-selection

for (z in 1:cc){
matrixE<-matrix(c(0, 1, min(selection[,z],na.rm=TRUE),max(selection[,z],na.rm=TRUE)), nrow = 2 , ncol = 2)
reg<-lm(matrixE[,1]~matrixE[,2])
datosC<-reg$coefficients[1]+selection[,z]*reg$coefficients[2]
datosE<-cbind(datosE,datosC)
}

datosE<-datosE[,-c(1:a[2])]

colnames(datosE)<-colnames(selection)

selection<-datosE

#Estimation of polar coordinates


datosX<-selection[,1]
for (z in 1:cc){
datosC<-abs(selection[,z])*cos(angle[z]*3.1416/180)
datosX<-data.frame(datosX,datosC)
}
datosX<-datosX[,-1]

XX<-apply(datosX,1,sum)

datosY<-selection[,1]
for (z in 1:cc){
datosC<- abs(selection[,z])*sin(angle[z]*3.1416/180)
datosY<-cbind(datosY,datosC)
}

datosY<-datosY[,-1]

YY<-apply(datosY,1,sum)

datosF<-data.frame(results[,1],XX,YY)
colnames(datosF)<-c("Samples", "X","Y")

NA1<-is.na(datosF[,2])
NA2<-is.na(datosF[,3])
NA1<-length(NA1[NA1==TRUE])
NA2<-length(NA2[NA2==TRUE])


if(NA1==0 & NA2==0){

###Area of the polygon
uni<-unique(datosF[,2:3])
eu<-cha(x = uni[,1], y = uni[,2])
eucli<-mean(dist(x = uni),na.rm=TRUE)
meanb<-mean(c(eu,eucli))
indi<-data.frame(as.character(Indices[1]),as.character(Indices[2]),as.character(Indices[3]),as.character(Indices[4]),
as.character(Indices[5]),eu, eucli,meanb)
names(indi)<-c("Index1","Index2","Index3","Index4", "Index5","Area","Euclidean","Mean")
ind<-rbind(ind,indi)

if(meanb>dist){
IndicesF<-Indices
datosFF<-datosF
dist<-meanb
}

}

}#End duplicate

}#End fith FOR
}#End fourth FOR
}#End third FOR
}#End second FOR
}#End first FOR
}
else{

selection<-data.frame(subset(results, select=Index[1]), subset(results, select=Index[2]),
subset(results, select=Index[3]), subset(results, select=Index[4]), subset(results, select=Index[5]))

#Standardization 0 to 1
a<-dim(selection)

datosE<-selection

for (z in 1:cc){
matrixE<-matrix(c(0, 1, min(selection[,z],na.rm=TRUE),max(selection[,z],na.rm=TRUE)), nrow = 2 , ncol = 2)
reg<-lm(matrixE[,1]~matrixE[,2])
datosC<-reg$coefficients[1]+selection[,z]*reg$coefficients[2]
datosE<-cbind(datosE,datosC)
}

datosE<-datosE[,-c(1:a[2])]

colnames(datosE)<-colnames(selection)

selection<-datosE

#Estimation of polar coordinates


datosX<-selection[,1]
for (z in 1:cc){
datosC<-abs(selection[,z])*cos(angle[z]*3.1416/180)
datosX<-data.frame(datosX,datosC)
}
datosX<-datosX[,-1]

XX<-apply(datosX,1,sum)

datosY<-selection[,1]
for (z in 1:cc){
datosC<- abs(selection[,z])*sin(angle[z]*3.1416/180)
datosY<-cbind(datosY,datosC)
}

datosY<-datosY[,-1]

YY<-apply(datosY,1,sum)

datosF<-data.frame(results[,1],XX,YY)
colnames(datosF)<-c("Samples", "X","Y")
IndicesF<-Index

datosFF<-datosF

}


####DER plot

Indices<-IndicesF
datosF<-datosFF

plot(x=0,y=0, cex=0, bty="n", xlim=c(-1.77,1.77), ylim=c(-0.5,4.34), xlab="",
ylab="", axes=FALSE)

plotellipse(rx=1.42,ry=1.866,mid=c(0,1.866),lcol='white',col=COLOR[1])


if(is.null(Index)){
vari1<-Indices[1]
vari2<-Indices[2]
vari3<-Indices[3]
vari4<-Indices[4]
vari5<-Indices[5]
}
else{
vari1<-Index[1]
vari2<-Index[2]
vari3<-Index[3]
vari4<-Index[4]
vari5<-Index[5]
}

if(references==TRUE){points(x=x,y=y, cex=1.2, bty="n", xlim=c(-1.77,1.77), ylim=c(-0.5,4.34), col=COLOR[3], pch=17)}

text<-paste("Max", vari1,"/ Max",vari2,"\n", "Max",vari3, "/ Max",vari4, "\n", "Max",vari5 )
text(x = 0 , y = 3.73 , labels = text, pos=3, cex=1)

text<-paste("Max", vari1,  "/ Max",vari2,"\n", "Max",vari3, "/ Max",vari4,"\n", "Min",vari5)
text(x = 0.65 , y = 3.5 , labels = text, pos=4, cex=1, offset=4.5)

text<-paste("Max", vari1,  "\n","Max",vari2,"\n", "Max",vari3, "\n", "Min",vari4,"\n", "Min",vari5)
text(x = 1.35 , y = 2.3 , labels = text, pos=4, cex=1, offset=1.2)

text<-paste("Max", vari1,  "\n","Max",vari2,"\n", "Min",vari3, "\n", "Min",vari4,"\n", "Min",vari5)
text(x = 1.35 , y = 1.3 , labels = text, pos=4, cex=1, offset=1.2)

text<-paste("Max", vari1,  "\n", "Min",vari2,"\n", "Min",vari3, "\n", "Min",vari4,"\n", "Min",vari5)
text(x = 0.6 , y = -0.1 , labels = text, pos=4, cex=1, offset=4.5)

text<-paste("Min", vari1, "/ Min",vari2,"\n", "Min",vari3, "/ Min",vari4, "\n", "Min",vari5)
text(x = 0, y = 0 , labels = text, pos=1, cex=1)

text<-paste("Min", vari1,  "\n", "Min",vari2,"\n", "Min",vari3, "\n", "Min",vari4,"\n", "Max",vari5)
text(x = -1.4 , y = -0.1 , labels = text, pos=4, cex=1, offset=4.5)

text<-paste("Min", vari1,  "\n","Min",vari2,"\n", "Min",vari3, "\n", "Max",vari4,"\n", "Max",vari5)
text(x = -1.8 , y = 1.3 , labels = text, pos=4, cex=1, offset=1.2)

text<-paste("Min", vari1,  "\n","Min",vari2,"\n", "Max",vari3, "\n", "Max",vari4,"\n", "Max",vari5)
text(x = -1.8 , y = 2.3 , labels = text, pos=4, cex=1, offset=1.2)

text<-paste("Min", vari1,  "/ Max",vari2,"\n", "Max",vari3, "/ Max",vari4,"\n", "Max",vari5)
text(x = -1.35 , y = 3.5 , labels = text, pos=4, cex=1, offset=4.5)



dtii<-data.frame(datosF, results[,varSize], results[,varColor])
names(dtii)<-c(names(datosF), varSize,varColor)

####Bubble plot
par(new=TRUE)
Bubble(data = dtii , varY = "Y" , varX = "X" , varSize = varSize , varColor = varColor, palette= palette,
size=size, digitsS=digitsS, digitsC=digitsC, ncolor=ncolor, transparency=transparency,  LEGENDS=LEGENDS,
TaxonFunc="TA")

####Text labels
pos<-data[,pos]
if(is.null(pos)) pos=4 else pos<-pos[!is.na(pos)]

if(!is.null(TEXT)){
textexe<-paste("text(",toString(x=TEXT), ")")
eval(parse(text=textexe))
}
else{
textexe<-paste("text(","x=datosF[,2],", "y=datosF[,3],", "labels=datosF[,1],","pos=pos",  ")")
eval(parse(text=textexe))
}



####Save the polar coordinates
if(save==TRUE){
if(dec=="."){
write.csv(x=datosF,file = file2, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = datosF,file = file2, fileEncoding = "", row.names=row.names,na=na)
}

if(is.null(Index)){
####Save indices and the area
ind<-ind[-1,]
ind<-ind[order(ind[,8] , decreasing = TRUE),]
if(dec=="."){
write.csv(x=ind,file = file3, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = ind,file = file3, fileEncoding = "", row.names=row.names,na=na)
}
}
}


###Area of the polygon

print("Final selection:")
if(is.null(Index)){
print(paste(Indices[1],"/",Indices[2],"/",Indices[3],"/",Indices[4], "/",Indices[5],sep=""))
}
else{
print(paste(Index[1],"/",Index[2],"/",Index[3], "/",Index[4], "/",Index[5],sep=""))
}
uni<-unique(datosF[,2:3])
eu<-cha(x = uni[,1], y = uni[,2])
eucli<-mean(dist(x = uni),na.rm=TRUE)
meanb<-mean(c(eu,eucli))
print(paste("Area of the polygon:", eu))
print(paste("Euclidean distance:", eucli))
paste("Mean:", meanb)
}


}
