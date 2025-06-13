#' xytb class definition 
#'
#' xytb is a trajectory object with observed behaviour
#'
#' @slot desc a character vector. A description of the data.
#'
#' @slot xyt a data frame of the track information. One row is one position
#' described by 11 variables :
#' \tabular{ll}{
#' id: \tab individual id\cr
#' t: \tab time in POSIXct\cr
#' x: \tab longitude\cr
#' y: \tab latitude\cr
#' dt: \tab time (s) between the current location and the next one\cr
#' dist: \tab distance (m) between the current location and the next one
#'   calculated using the Vincenty ellipsoid method.
#'   See \link{distVincentyEllipsoid})\cr
#' v: \tab speed (m.s^{-1})\cr
#' dx: \tab the increase of the move in the x direction.\cr
#' dy: \tab the increase of the move in the y direction.\cr
#' theta: \tab the angle between each move and the x axis.\cr
#' thetarel: \tab the turning angle between successive moves.\cr
#' }
#' This slot is very similar to the ltraj class of the adehabitatLT package.
#'
#' @slot b a data frame of the observed behaviour related to the track. One row
#' is one behavioural observation related to the track described by 3 variables: 
#' \tabular{ll}{
#' id: \tab individual id\cr
#' t: \tab time in POSIXct\cr
#' b: \tab observed behaviour (character)\cr
#' }
#'
#' @slot dxyt a data frame of the derived variables from the track. 
#'
#' @slot befdxyt a data frame of the derived variables shifted back in time. 
#'
#' @slot model a randomForest model (see
#' \link[randomForest]{randomForest}).
#'
#' @slot rfcv the output of the cross-validation procedure (see
#' \link[randomForest]{rfcv}).
#'
#' @slot predb a data frame of the predicted behaviour using the random forest
#' model of the slot model. 3 variables :
#' \tabular{ll}{
#' id: \tab individual id\cr
#' t: \tab time in POSIXct\cr
#' b: \tab observed behaviour (character)\cr
#' }
#'
#' @author Laurent Dubroca
#'
#' @name xytb-class
#' @exportClass xytb
setClass(Class='xytb',
	 slots=list(
		    desc='character',
		    xyt='data.frame',
		    b='data.frame',
		    dxyt='data.frame',
		    befdxyt='data.frame',
		    model='list',
		    rfcv='list',
		    predb='data.frame'
		    ),
	 prototype=list(
			desc='unknown track',
			xyt=data.frame(),
			b=data.frame(),
			dxyt=data.frame(),
			befdxyt=data.frame(),
		    	model=list(),
		    	rfcv=list(),
		    	predb=data.frame()
			)
	 )

#' xytb class conversion to ltraj object 
#'
#' The function converts a xytb object to a ltraj object as defined in the
#' adehabitatLT package (see 
#' \url{https://CRAN.R-project.org/package=adehabitatLT}
#' \link[adehabitatLT]{as.ltraj}).
#'
#' @param xytb An xytb object
#' @return A ltraj object with behavioural information recorded in the infoloc
#' @examples
#' #track_CAGA_005 is a dataset
#' xytb<-xytb(track_CAGA_005,"a track",3,.5)
#' ltraj<-xytb2ltraj(xytb)
#' #all adehabitatLT function are now available
#' summary(ltraj)
#' plot(ltraj)
#' @author Laurent Dubroca
#' @name xytb2ltraj
#' @export
xytb2ltraj<-function(xytb){
	if(!requireNamespace("adehabitatLT", quietly = TRUE)){
	 stop("adehabitatLT needed for this function to work. Please install it.",call.=FALSE)
	}
	if(class(xytb)!="xytb"){stop("the object is not an xytb object")}
	if(nrow(xytb@xyt)==0){stop("the xyt slot seems empty")}
	xyt<-xytb@xyt
	track<-adehabitatLT::as.ltraj(xy=xyt[,c("x","y")],date=xyt$t,id=xyt$id,infolocs=xytb@b)
	return(track)
}

#' ltraj object conversion to xytb object 
#'
#' The function converts a ltraj object to a xytb object 
#' (see  
#' \url{https://CRAN.R-project.org/package=adehabitatLT}
#' and \link{xytb-class}).
#'
#' @param ltraj A ltraj object
#' @param desc General descriptor of the data
#' @return A xytb object with behavioural information taken from the infolocs
#' (aka ltraj object should correspond the an export of xytb object in ltraj).
#' @examples
#' #track_CAGA_005 is a dataset
#' xytb<-xytb(track_CAGA_005,"a track",3,.5)
#' ltraj<-xytb2ltraj(xytb)
#' xytb2<-ltraj2xytb(ltraj,"a track")
#' @author Laurent Dubroca
#' @name ltraj2xytb
#' @export
ltraj2xytb<-function(ltraj,desc="ltraj object convert to xytb"){
	if(!requireNamespace("adehabitatLT", quietly = TRUE)){
	 stop("adehabitatLT needed for this function to work. Please install it.",call.=FALSE)
	}
	if(!any(class(ltraj)!="ltraj")){stop("the object is not a ltraj object")}
	nbid<-length(ltraj)
	xy<-data.frame()
	b<-data.frame()
	for(i in 1:nbid){
		xy0<-ltraj[[i]]
		names(xy0)[names(xy0)=="date"]<-"t"
		xy0$id<-attr(xy0,"id")
		b0<-attr(xy0,"infolocs")
		xy0<-merge(xy0,b0)
		xy<-rbind(xy,xy0)
	}
	xytb<-xytb(xy[,names(xy)%in%c("id","t","x","y","b")],"pipo")
	return(xytb)
}

#' xytb object conversion to moveHMM object 
#'
#' The function converts a xytb object to a moveHMM dataframe 
#' (see 
#' \url{https://CRAN.R-project.org/package=moveHMM}
#' and \link[moveHMM]{prepData}).
#'
#' @param xytb A xytb object
#' @return A dataframe ready to be used by the function of the `moveHMM`
#' @examples
#' #track_CAGA_005 is a dataset
#' xytb<-xytb(track_CAGA_005,"a track",3,.5)
#' xyhmm <-xytb2hmm(xytb)
#' #then you can use moveHMM functions
#' if(requireNamespace("moveHMM")){
#' 	plot(xyhmm)
#' }
#' @author Laurent Dubroca
#' @name xytb2hmm 
#' @export
xytb2hmm<-function(xytb){
	if(!requireNamespace("moveHMM", quietly = TRUE)){
	 stop("moveHMM needed for this function to work. Please install it.",call.=FALSE)
	}
	if(class(xytb)!="xytb"){stop("the object is not an xytb object")}
	xy<-xytb@xyt
	xy<-data.frame(ID=xy$id,x=xy$x,y=xy$y)
	rez<-moveHMM::prepData(xy)
	return(rez)
}

#' internal test function for dev purposes
#'
#' blablablablbalalb
#'
#' @keywords internal
test<-function(){
	#library(m2b)
	#library(moveHMM)
	#Examples
	#' #track_CAGA_005 is a dataset
	#library(m2b)
	# xytb<-xytb(track_CAGA_005,"a track",3,.5)
	#xyhmm<-xytb2hmm(xytb)
	#plot(xyhmm)
}



#' xytb class constructor
#'
#' The methods to build an xytb object (see \link{xytb-class} for the class
#' description).
#' 
#' @param object a data frame with 5 columns
#' \tabular{ccc}{
#' id \tab individual id \tab character \cr
#' x \tab longitude \tab decimal degree \cr
#' y \tab latitude \tab decimal degree \cr
#' t \tab date and time \tab POSIXct \cr
#' b \tab behaviour \tab character \cr
#' }
#' @param desc vector of character describing the dataset
#' @param winsize a numerical vector giving the length of the windows used to
#' calculate moving standard deviation, average, mad and quantile for the speed
#' (v), the distance (dist) and the relative angle (thetarel).
#' @param idquant a numerical vector giving the quantiles to be calculated. For example if
#' idquant=c(0,0.25,.5,1), the quantiles at 0\% (min), 25\% (first
#' quartile), 50\% (median) and 100\% (max) will be
#' calculated.
#' @param move a numerical vector providing the shift used to computed parameters
#' back in time. For example if move=c(5,10,100), the parameters will be shifted
#' backward by 5, 10 and 100 locations. Optional.
#' @param ... part of the generic definition 
#' 
#' @section Methods' signature:
#' \itemize{
#' 	\item \code{xytb()}: generate an empty xytb object.
#' 	\item \code{xytb(data.frame,character vector)}: generate an xytb object
#' 	with track information (only slot desc, xyt and b are computed).
#' 	\item \code{xytb(data.frame,character vector,numerical vector,
#' 	numerical vector,numerical vector)}: generate an xytb object with track
#' 	information (slots \code{desc}, \code{xyt}, \code{b}) and derived
#' 	information \code{dxyt} and
#' 	\code{befdxyt}. \code{dxyt} contains statistical derivatives of speed, distance and
#' 	relative angle calculated on moving windows given by the winsize
#' 	parameters. Statistical derivatides are standard deviation, mean, median
#' 	absolute deviation and quantiles. Quantiles are defined by the
#' 	\code{idquant}
#' 	parameters. \code{befdxyt} contains \code{dxyt} values shifted back in
#' 	time according to the \code{move} parameters.
#' }
#' @return an xytb object 
#' @examples
#' #generate an empty xytb object
#' xytb()
#' #generate an xytb object with track information only
#' #track_CAGA_005 is a dataset
#' simplexytb<-xytb(track_CAGA_005,"a track")
#' #generate a complete xytb object with derived (over moving windows of 3, 5
#' #and 9 points, with quantile at 0, 50 and 100%) and shifted information on 10
#' #and 100 points
#' xytb<-xytb(track_CAGA_005,"a track",c(3,5,9),c(0,.5,1),c(10,100))
#'
#' @author Laurent Dubroca
#' @name xytb
#' @rdname xytb-methods
#' @exportMethod xytb
setGeneric("xytb",
	  function(object,desc,winsize,idquant,move,...){
		standardGeneric("xytb")
	  } 
	  )

#' @rdname xytb-methods
#' @aliases xytb
setMethod("xytb",
	  #signature("missing", "missing", "missing", "missing", 
	  #	    "missing","missing","missing"), 
	  signature(object="missing",desc="missing"),
	  #function(desc="null xybt object",xyt,b,dxyt,befdxyt,model,predb){
	  function(desc="null xybt object"){
		  return(methods::new("xytb"))
	  }
	  )

#' @rdname xytb-methods
#' @aliases xytb,data.frame,character
setMethod("xytb",
	  signature(object="data.frame",desc="character"),
	  function(object,desc="unknow track",...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y, t and/or b is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$id)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  #order data according to id and time
		  object<-object[order(object$id,object$t),]
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y,stringsAsFactors=F)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b,stringsAsFactors=F)
		  dxyt0<-dxyt(xyt0)
		  methods::new("xytb",xyt=dxyt0,b=b0,desc=desc)
	  }
	  )

#' @rdname xytb-methods
#' @aliases xytb,data.frame,character,vector,vector
setMethod("xytb",
	  signature(object="data.frame",desc="character",winsize="vector",idquant="vector"),
	  function(object,desc="unknow track",winsize=seq(3,13,2),idquant=seq(0,1,.25),...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y, t and/or b is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$b)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  if(min(winsize,na.rm=T)<=2 | 
		     any(is.na(winsize))|
		     any(winsize%%1!=0)|
		     any(winsize%%2L==0L)
			){
			  stop("invalid values in winsize (has to be >2, integer and odd)")
		  }
		  if(min(idquant,na.rm=T)<0 | 
		     max(idquant,na.rm=T)>1 |
		     any(is.na(idquant))
			){
			  stop("invalid values in idquant (values in [0,1])")
		  }
		  #order data according to id and time
		  object<-object[order(object$id,object$t),]
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y,stringsAsFactors=F)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b,stringsAsFactors=F)
		  dxyt0<-dxyt(xyt0)
		  dxyt1<-dxyt2(dxyt0,winsize,idquant)
		  methods::new("xytb",xyt=dxyt0,b=b0,desc=desc,dxyt=dxyt1)
	  }
	  )

#' @rdname xytb-methods
#' @aliases xytb,data.frame,character,vector,vector,vector
setMethod("xytb",
	  signature(object="data.frame",desc="character",winsize="vector",idquant="vector",move="vector"),
	  function(object,desc="unknow track",winsize=seq(3,13,2),idquant=seq(0,1,.25),move=c(5,10),...){
		  #check trajectory and behavioural info
		  if(!all(names(object)%in%c("id","x","y","t","b"))){
			  stop("wrong variables in the xyt data.frame: 
			       id, x, y, t and/or b is missing")
		  }
		  if(!is.numeric(object$x)){
			  stop("x is not numeric")
		  }
		  if(!is.numeric(object$y)){
			  stop("y is not numeric")
		  }
		  if(!is.character(object$b)){
			  stop("b is not character")
		  }
		  if(!is.character(object$id)){
			  stop("id is not character")
		  }
		  if(!any(class(object$t)%in%"POSIXct")){
			  stop("t is not POSIXct")
		  }
		  if(min(winsize,na.rm=T)<=2 | 
		     any(is.na(winsize))|
		     any(winsize%%1!=0)|
		     any(winsize%%2L==0L)
			){
			  stop("invalid values in winsize (has to be >2, integer and odd)")
		  }
		  if(min(idquant,na.rm=T)<0 | 
		     max(idquant,na.rm=T)>1 |
		     any(is.na(idquant))
			){
			  stop("invalid values in idquant (values in [0,1])")
		  }
		  if(min(move,na.rm=T)<=0 | 
		     any(move%%1!=0)|
		     max(move)>nrow(object)|
		     any(is.na(move))
			){
			  stop("invalid values in move (has to be >0, integer and < nb of track points)")
		  }
		  #order data according to id and time
		  object<-object[order(object$id,object$t),]
		  xyt0<-data.frame(id=object$id,t=object$t,x=object$x,y=object$y,stringsAsFactors=F)
		  b0<-data.frame(id=object$id,t=object$t,b=object$b,stringsAsFactors=F)
		  dxyt0<-dxyt(xyt0)
		  dxyt1<-dxyt2(dxyt0,winsize,idquant)
		  dxyt2<-shiftvalue(dxyt1,move)
		  methods::new("xytb",xyt=dxyt0,b=b0,desc=desc,dxyt=dxyt1,befdxyt=dxyt2)
	  }
	  )

#' internal function
#'
#' @author Laurent Dubroca
#' @name dxyt
#' @param xyt xyt parameters
#' @export
dxyt<-function(xyt){
  xy<-xyt
  xy$dt<-as.numeric(c(difftime(xy$t[-1],xy$t[-nrow(xy)],units="s"),"NA"))
  idx<-which(names(xy)=="x")
  idy<-which(names(xy)=="y")
  xy$dist<- c(geosphere::distVincentyEllipsoid(as.matrix(xy[1:(nrow(xy)-1),c(idx,idy)]),as.matrix(xy[2:nrow(xy),c(idx,idy)])),NA)
  xy$v<-xy$dist/xy$dt
  xy$dx<-c(diff(xy$x),NA)
  xy$dy<-c(diff(xy$y),NA)
  xy$theta<-atan2(xy$dy,xy$dx) 
  #relative angle for theta
  xy$thetarel[2:nrow(xy)]<-diff(xy$theta)
  xy$thetarel <- ifelse(xy$thetarel <= (-pi), 2 * pi + xy$thetarel, xy$thetarel)
  xy$thetarel <- ifelse(xy$thetarel > pi, xy$thetarel - 2 * pi, xy$thetarel)
  return(xy)
}

#' internal function
#'
#' @author Laurent Dubroca
#' @name dxyt2
#' @param dxyt A parameter
#' @param winsize A parameter
#' @param idquant A parameter
#' @export
dxyt2<-function(dxyt,winsize=seq(3,13,2),idquant=seq(0,1,.25)){
 #init size window (odd number please) and quantile range
  nbw<-length(winsize)
  #dataframe variable names generation
  vnomquant<-expand.grid(paste("vquant",winsize,sep="_w"),idquant)
  thetanomquant<-expand.grid(paste("thetarelquant",winsize,sep="_w"),idquant)
  distnomquant<-expand.grid(paste("distquant",winsize,sep="_w"),idquant)
  vnomquant<-paste(vnomquant[,1],vnomquant[,2],sep="_")
  thetanomquant<-paste(thetanomquant[,1],thetanomquant[,2],sep="_")
  distnomquant<-paste(distnomquant[,1],distnomquant[,2],sep="_")
  nomparam<-c(names(dxyt),
  	apply(
		expand.grid(c("v","thetarel","dist"),
			    c("mean","sd","mad"),
			    paste0("_w",winsize),
		stringsAsFactors=F),1,paste0,collapse=""),
	vnomquant,thetanomquant,distnomquant)
  dat0<-data.frame(matrix(NA,ncol=length(nomparam),nrow=nrow(dxyt)),stringsAsFactors=F)
  names(dat0)<-nomparam
  #calculate variables
  dat0[,1:ncol(dxyt)]<-dxyt
  ######################################################
  #absolute value of thetarel (bug correction 18/12/2014...)
  ######################################################
  dat0$theta<-abs(dxyt$thetarel)
  #####################################################
  dat0$v[is.na(dat0$v)]<-0
  dat0$theta[is.na(dat0$theta)]<-0
  dat0$dist[is.na(dat0$dist)]<-0
  print(paste("Compute",ncol(dat0)-10,"indicators on",length(winsize),"moving windows"))
  for(i in winsize){
    print(paste("Compute indicators on",i,"points"))
    #runmean
    idcol<-which(paste("vmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$v,i)
    idcol<-which(paste("thetarelmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$theta,i)
    idcol<-which(paste("distmean_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmean(dat0$dist,i)
    #runsd
    idcol<-which(paste("vsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$v,i)
    idcol<-which(paste("thetarelsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$theta,i)
    idcol<-which(paste("distsd_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runsd(dat0$dist,i)
    #runmad
    idcol<-which(paste("vmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$v,i)
    idcol<-which(paste("thetarelmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$theta,i)
    idcol<-which(paste("distmad_w",i,sep="")==names(dat0))
    dat0[,idcol]<-caTools::runmad(dat0$dist,i)
    #runquantile
    idcol<-grep(paste("vquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$v,i,idquant)
    idcol<-grep(paste("thetarelquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$theta,i,idquant)
    idcol<-grep(paste("distquant_w",i,sep=""),names(dat0))
    dat0[,idcol]<-caTools::runquantile(dat0$dist,i,idquant)
    print(paste("Done"))
  }
  dat0<-dat0[,names(dat0)%in%c("v","dist","thetarel") | !names(dat0)%in%names(dxyt)]
 return(dat0)
}
			 
#' internal function
#'
#' @author Laurent Dubroca
#' @name shiftvalue 
#' @param dat0 A parameter
#' @param mov A parameter
#' @export
shiftvalue<-function(dat0,mov=seq(5,250,5)){
  print("shift value backward")
  #nbcoldat<-length(8:ncol(dat0))
  nbcoldat<-ncol(dat0)
  datadd<-data.frame(matrix(NA,ncol=nbcoldat*length(mov),nrow=nrow(dat0)),stringsAsFactors=F)
  #varn<-expand.grid(colnames(dat0)[c(-1:-7)],mov)
  varn<-expand.grid(colnames(dat0),mov)
  varn<-paste(varn[,1],sprintf("%02d",varn[,2]),sep="_mov_")
  names(datadd)<-varn
  for (i in 1:length(mov)){
    print(paste("shift backward",mov[i]))
    idcol<-grep(paste("_mov_",sprintf("%02d",mov[i]),sep=""),names(datadd))
    datadd[(mov[i]+1):nrow(datadd),idcol]<-dat0[1:(nrow(datadd)-mov[i]),1:ncol(dat0)]
  }
  print("Done")
  datadd<-cbind(dat0,datadd,stringsAsFactors=F)
  datadd<-datadd[,!names(datadd)%in%names(dat0)]
  return(datadd)
}

#' xytb plot method 
#'
#' plot method for xytb object
#'
#' @param x An xytb object.
#' @param y empty 
#' @return a plot
#' @docType methods
#' @rdname plot-methods
#' @name plot
NULL

# @method plot xytb
#' @rdname plot-methods
#' @export
setMethod("plot",
	  signature(x="xytb",y="missing"),
	  function(x,y){
		  pipo<-data.frame(id=x@xyt$id,x=x@xyt$x,y=x@xyt$y,b=x@b$b,stringsAsFactors=FALSE)	
		  p1<-ggplot2::ggplot(pipo,ggplot2::aes(x=pipo$x,y=pipo$y,color=pipo$b,group=pipo$id))+
		  	ggplot2::geom_path()+ggplot2::facet_wrap(~id)+
			ggplot2::scale_colour_discrete(name="Behaviour")+#guide_legend(title="type")+
			ggplot2::xlab("Longitude")+ggplot2::ylab("Latitude")+ggplot2::ggtitle(x@desc)
		  return(p1)
	  }
	  )
		
#' xytb randomForest function
#'
#' Build a random forest model on a xytb object, predicting behaviour using
#' only the variables calculated at the time of observation (type `actual`) or
#' using the variable shifted backwards in time (type `shifted`). Parameters
#' are transfered to the randomForest or the rfcv functions of the randomForest
#' package if needed.
#'
#' @author Laurent Dubroca and Andréa Thiebault
#'
#' @param xytb an xytb object
#' @param type character -actual or shifted- use actual data or shifted one to
#' build the model
#' @param nob character. Define the unobserved value of the behaviour (and
#' where prediction are done)
#' @param colin boolean - remove colinearity among predictors (see the caret
#' package for more details)
#' @param varkeep character vector - the variables names in this vector are
#' keeped in the model even if colinearity is found (usefull to keep 'classical'
#' parameters and to help interpretation)
#' @param zerovar boolean - remove near zero variance predictor (see the caret
#' package for more details)
#' @param rfcv boolean - run a random forest cross-validation for feature selection
#' procedure for xybt (this call the \code{rfcv} fonction for the model).
#' This function shows the cross-validated prediction performance of models 
#' with sequentially reduced number of predictors (ranked by variable
#' importance) via a nested cross-validation procedure for a xytb object. 
#' @param ntree number of trees in the random Forest (see the randomForest
#' package for more details)
#' @param importance boolean (see the randomForest package for more details)
#' @param ... other arguements passed to randonForest or rfcv
#'
#' @examples
#' #track_CAGA_005 is dataset
#' #generate a complete xytb object with derived (over moving windows of 3, 5
#' #and 9 points, with quantile at 0, 50 and 100%) and shifted information on 10
#' #and 100 points
#' xytb<-xytb(track_CAGA_005,"a track",c(3,5,9),c(0,.5,1),c(10,100))
#' #compute a random forest model to predict behaviour (b, where -1 is
#' #unobserved behaviour) using the derived
#' #parameters ("actual")
#' xytb<-modelRF(xytb,"actual",nob="-1",colin=TRUE,varkeep=c("v","thetarel"),
#' zerovar=TRUE)
#' \dontrun{ 
#' #cross-validation for the same model (time consuming !)
#' xytb<-modelRF(xytb,"actual",nob="-1",colin=TRUE,varkeep=c("v","thetarel"),
#' zerovar=TRUE,rfcv=TRUE)
#' }
#' 
#'
#' @seealso See \link[randomForest]{randomForest} and \link[randomForest]{rfcv}
#' @name modelRF 
#' @export
modelRF<-function(xytb,type=c("actual","shifted"),nob="-1",
		  colin=TRUE,varkeep=c("v","dist","thetarel"),
		  zerovar=TRUE,rfcv=FALSE,ntree=501,importance=TRUE,...){
	if(class(xytb)!="xytb"){ stop("invalid xytb object") }
	if(!type%in%c("actual","shifted") | length(type)!=1){
	 stop("invalid type")
	}
	if(type=="actual"){ dat0<-cbind(xytb@b,xytb@dxyt,stringsAsFactors=F) }
	if(type=="shifted"){ dat0<-cbind(xytb@b,xytb@befdxyt,stringsAsFactors=F) }
	if(!nob%in%dat0$b){
		stop("wrong no behavioural observation code")
	}else{
		idna<-as.numeric(attr(stats::na.omit(dat0),"na.action"))
		dat0$b[dat0$b==nob]<-NA
		initdat0<-dat0
	}
	#remove NA value
	print("removing lines with NA values")
	dat0<-stats::na.omit(dat0)
	if(nrow(dat0)==0){stop("no data",call.=F)}
	b0<-dat0$b
	dat0<-dat0[,-1:-3]
	#remove colinearity
	if(colin){
		print("removing colinearity among predictors")
		idcolin<-caret::findCorrelation(stats::cor(dat0),.9,verbose=F)
		if(length(idcolin)>0){
			print(paste(paste(varkeep,collapse=","),"keeped",collapse=" "))
			idcolin<-idcolin[which(!names(dat0)[idcolin]%in%varkeep)]
			if(length(idcolin)>0){
				print(paste(paste(names(dat0)[idcolin],collapse=","),
					    "removed",collapse=" "))
				dat0<-dat0[,-idcolin]
			}else{
				print("no colinearity found")
			}
		}
	}
	#remove zerovar 
	if(zerovar){
		print("removing near zero variance predictors")
		idzerovar<-caret::nearZeroVar(dat0)
		if(length(idzerovar)>0){
			print(paste(paste(varkeep,collapse=","),"keeped",collapse=" "))
			idzerovar<-idzerovar[which(!names(dat0)[idzerovar]%in%varkeep)]
			if(length(idzerovar)>0){
				print(paste(paste(names(dat0)[idzerovar],collapse=","),
					    "removed",collapse=" "))
				dat0<-dat0[,-idcolin]
			}else{
				print("no zero variance predictors found")
			}
		}
	}
	tabb<-table(b0)
	#print(tabb)
	#print(str(b0))
	if(rfcv){
		print("rfcv called")
		#xytb@rfcv<-randomForest::rfcv(dat0,as.factor(b0),sampsize=rep(min(tabb),length(tabb)),...)
		xytb@rfcv<-randomForest::rfcv(dat0,as.factor(b0),...)
	}
	#build and compute model
	rfFit<-randomForest::randomForest(dat0,as.factor(b0),ntree=ntree,sampsize=rep(min(tabb),length(tabb)),
		     importance=importance,...)
	#prediction
	initdat0[is.na(initdat0)]<-0
	newb<-as.character(stats::predict(rfFit,new=initdat0))
	newb[idna]<-"no data"
	class(rfFit)<-c("list")
	xytb@model<-rfFit
	xytb@predb<-data.frame(id=initdat0$id,t=initdat0$t,b=newb)
	return(xytb)
}

#' Extract the random forest model from an xytb object
#'
#' The random forest model is extracted from the xytb object to a randomForest
#' object.
#'
#' @seealso  See \link[randomForest]{randomForest}
#' @author Laurent Dubroca
#' @name extractRF
#' @param xytb an xytb object with a model.
#' @return a randomForest object as defined by the randomForest package.
#' @examples
#' #track_CAGA_005 is dataset
#' #generate a complete xytb object with derived (over moving windows of 3, 5
#' #and 9 points, with quantile at 0, 50 and 100%) and shifted information on 10
#' #and 100 points
#' xytb<-xytb(track_CAGA_005,"a track",c(3,5,9),c(0,.5,1),c(10,100))
#' #compute a random forest model to predict behaviour (b, where -1 is
#' #unobserved behaviour) using the derived
#' #parameters ("actual")
#' xytb<-modelRF(xytb,"actual",nob="-1",colin=TRUE,varkeep=c("v","thetarel"),zerovar=TRUE)
#' #extract the model
#' modRF<-extractRF(xytb)
#' # results from randomForest package:
#' print(modRF)
#' plot(modRF)
#' @export
extractRF<-function(xytb){
	if(class(xytb)!="xytb"){ stop("invalid xytb object") }
	if(length(xytb@model)==0){stop("No model in the object, please run randomRF first",call.=F)}
	modRF<-xytb@model
	class(modRF)<-c("randomForest")
	return(modRF)
}


#' Random forest model outputs for a xytb object
#'
#' Diagnostic plots and tables for the random forest model used to predict behaviour on a
#' xytb objecti (random forest convergence plot, variable importance plot,
#' cross-validation plot, confusion matrix of the observed vs predicted
#' behaviours).
#'
#' @seealso  See \link[randomForest]{randomForest}
#' @author Laurent Dubroca
#' @name resRF 
#' @param xytb An xytb object with a model.
#' @param type
#' \itemize{
#'   \item \code{rf}: plot of the OOB versus the number of trees (see \link[randomForest]{randomForest}).
#'   \item \code{importance}: importance plot (see \link[randomForest]{varImpPlot}).
#'   \item \code{rfcv}: plot of the cross-validated prediction performance of models
#'	with sequentially reduced number of predictors (see \link[randomForest]{rfcv}).
#'   \item \code{confusion}: confusion matrix between observed and predicted behaviours (see \link[caret]{confusionMatrix}).
#' }
#'
#' @return plots or tables.
#' @examples
#' \dontrun{ 
#' #track_CAGA_005 is dataset
#' #generate a complete xytb object with derived (over moving windows of 3, 5
#' #and 9 points, with quantile at 0, 50 and 100%) and shifted information on 10
#' #and 100 points
#' xytb<-xytb(track_CAGA_005,"a track",c(3,5,9),c(0,.5,1),c(10,100))
#' #compute a random forest model to predict behaviour (b, where -1 is
#' #unobserved behaviour) using the derived
#' #parameters ("actual")
#' xytb<-modelRF(xytb,"actual",nob="-1",colin=TRUE,varkeep=c("v","thetarel"),
#' zerovar=TRUE,rfcv=TRUE,step=.9)
#' #modelling results
#' resRF(xytb,type="rf")
#' resRF(xytb,type="importance")
#' resRF(xytb,type="rfcv")
#' resRF(xytb,type="confusion")
#' }
#' @export
resRF<-function(xytb,type="rf"){
	if(class(xytb)!="xytb"){ stop("invalid xytb object") }
	if(length(xytb@model)==0){stop("No model in the object, please run randomRF first",call.=F)}
	if(any(is.na(type)) | length(type)>1){stop("Wrong type",call.=F)}
	if(!any(type%in%c("rfcv","rf","importance","confusion"))){stop("Wrong type",call.=F)}
	#compute results summary
	modRF<-xytb@model
	class(modRF)<-c("randomForest")
	ntree<-modRF$ntree
	errOOB<-round(modRF$err.rate[ntree,1],3)*100
	mtry<-modRF$mtry
	#graph and table
	if(type=="rfcv"){
		testmtry<-xytb@rfcv
		testmtry<-data.frame(n=testmtry$n.var,error=100*testmtry$error.cv,Model=xytb@desc)
		p1<-ggplot2::ggplot(data=testmtry,aes(x=testmtry$n,y=testmtry$error,linetype=testmtry$Model))+
			geom_line()+
			coord_trans(x="log10")+
			theme(legend.position=c(.8,.8))+
			ggtitle(paste0("Results: OOB error ",errOOB,"%/",ntree," trees"))+
			xlab("Number of variables")+
			ylab("Classification error rate (%)")
		return(p1)
	}
	if(type=="importance"){
		randomForest::varImpPlot(modRF,type=1,n.var=20,main="")
		ntree<-modRF$ntree
		errOOB<-round(modRF$err.rate[ntree,1],3)*100
		mtry<-modRF$mtry
		graphics::title(paste0("Results: OOB error ",errOOB,"%/",ntree," trees/mtry ",mtry))
	}
	if(type=="rf"){
		pipo<-modRF$err.rate
		pipo2<-data.frame()
		for(i in 1:ncol(pipo)){
		  pipo2tmp<-data.frame(ntree=1:nrow(pipo),value=pipo[,i])
		  pipo2tmp$var<-attr(pipo,"dimnames")[[2]][i]
		  pipo2<-rbind(pipo2,pipo2tmp)
		}
		p1<-ggplot2::ggplot(data=pipo2,aes(x=pipo2$ntree,y=100*pipo2$value,linetype=pipo2$var))+
		geom_line()+
		ggplot2::ggtitle(paste0("Results: OOB error ",errOOB,"%/",ntree," trees/mtry ",mtry))+
		ggplot2::scale_linetype_discrete(name="Behaviours\nand OOB")+#guide_legend(title="type")+
		xlab("Number of trees")+
		ylab("OOB classification error rate (%)")
		return(p1)
	}
	if(type=="confusion"){
	 caret::confusionMatrix(modRF$predicted,modRF$y)
	}
}


#' Representation of the predicted vs observed behaviour of an xytb object
#'
#' @seealso  See \link[randomForest]{randomForest}
#' @author Laurent Dubroca
#' @name resB
#' @param xytb an xytb object with predicted behaviour.
#' @param type
#' \itemize{
#'   \item \code{time}: plot results in time.
#'   \item \code{space}: plot results in space.
#'   \item \code{density}: plot results in space, adding density surface by behaviour.
#' }
#' @param nob character. Define the unobserved value of the behaviour (and
#' where prediction are done)
#'
#' @return a ggplot 
#' @examples
#' \dontrun{
#' #track_CAGA_005 is dataset
#' #generate a complete xytb object with derived (over moving windows of 3, 5
#' #and 9 points, with quantile at 0, 50 and 100%) and shifted information on 10
#' #and 100 points
#' xytb<-xytb(track_CAGA_005,"a track",c(3,5,9),c(0,.5,1),c(10,100))
#' #compute a random forest model to predict behaviour (b, where -1 is
#' #unobserved behaviour) using the derived
#' #parameters ("actual")
#' xytb<-modelRF(xytb,"actual",nob="-1",colin=TRUE,varkeep=c("v","thetarel"),
#' zerovar=TRUE,rfcv=FALSE,step=.9)
#' #behaviour results:
#' resB(xytb,type="time",nob="-1")
#' resB(xytb,type="space",nob="-1")
#' resB(xytb,type="density",nob="-1")
#' }
#' @import ggplot2
#' @export
resB<-function(xytb,type="time",nob="-1"){
	if(any(is.na(type)) | length(type)>1){stop("Wrong type",call.=F)}
	if(!any(type%in%c("space","time","density"))){stop("Wrong type",call.=F)}
	if(class(xytb)!="xytb"){ stop("invalid xytb object",call.=F) }
	if(length(xytb@predb)==0){stop("No predicted behaviour in the object, please run randomRF first.",call.=F)}
	if(length(xytb@b)==0){stop("No observed behaviour in the object, please build the xytb object wisely.",call.=F)}
	if(!nob%in%xytb@b$b){ stop("Wrong no behavioural observation code",call.=F) }
	#new test
	pipo1<-xytb@xyt
	pipo1$b<-xytb@b$b
 	pipo1$type<-"Observation"
	pipo1<-pipo1[pipo1$b!=nob,]
	pipo1$which<-"Data with observation"
	pipo2<-xytb@xyt
	pipo2$b<-xytb@predb$b
 	pipo2$type<-"Prediction"
	pipo2$which<-"Data with observation"
	pipo2$which[!pipo2$t%in%pipo1$t]<-"Data without observation"
	pipo<-rbind(pipo1,pipo2)
	pipo<-pipo[pipo$b!="no data",]
	#the plot
	if(type=="time"){
	p1<-ggplot2::ggplot(pipo,aes(x=pipo$t,y=pipo$b,group=pipo$id,color=pipo$type))+
		geom_line(alpha=.8)+
		xlab("time")+ylab("Behaviour")+
		theme(legend.position="bottom")+
		scale_colour_discrete(name="type")+#guide_legend(title="type")+
		facet_grid(pipo$id~pipo$which,scales="free_x")
	}
	if(type=="space"){
	p1<-ggplot2::ggplot(pipo,aes(x=pipo$x,y=pipo$y,group=pipo$type,color=pipo$b,shape=pipo$b))+
		geom_path(color="black")+
		geom_point(alpha=.4)+
		scale_colour_discrete(name="Behaviour")+#guide_legend(title="type")+
		scale_shape_discrete(name="Behaviour")+#guide_legend(title="type")+
		xlab("Longitude")+ylab("Latitude")+
		facet_grid(pipo$id~pipo$which+type)
	}
	if(type=="density"){
	p1<-ggplot2::ggplot(pipo,aes(x=pipo$x,y=pipo$y,group=pipo$type,color=pipo$b,shape=pipo$b))+
		geom_path(color="black")+
		geom_point(alpha=.6)+
		geom_density2d(data=pipo,aes(x=pipo$x,y=pipo$y,color=pipo$b,group=pipo$b),alpha=.6)+
		scale_colour_discrete(name="Behaviour")+#guide_legend(title="type")+
		scale_shape_discrete(name="Behaviour")+#guide_legend(title="type")+
		xlab("Longitude")+ylab("Latitude")+
		facet_grid(pipo$id~pipo$which+pipo$type)
	}
	return(p1)
}
