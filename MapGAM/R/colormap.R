#******************************************************************************
#
# Maps Predicted Values and Clusters on a Two-Dimentional Map
# Copyright (C) 2016, 2022, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************

colormap <- function(obj, map=NULL, exp=FALSE, add=FALSE,  mapmin=NULL, mapmax=NULL,
                     col.seq=rev(rainbow(201,start=0,end=0.66)), anchor=FALSE,
					 border.gray=0.3, contours="none", contours.drawlabels=FALSE, contours.lty=1, 
					 contours.lwd=1, contours.levels, contours.labcex=0.7, interval.exclude=0, 
					 arrow=TRUE, axes=FALSE, ptsize=0.9, mai, legend.name="predicted values", 
					 legend.cex=1, legend.add.line, alpha=0.05, ...) 
  {
    op.mai <- par()$mai
    op.xpd <- par()$xpd
	  if (exp) fitvals=exp(obj$fit) else fitvals=obj$fit 
	  results = cbind(obj$grid,fitvals)
	  
	  ## Range of map values
	  if (is.null(mapmin)) mapmin=min(results[,3], na.rm=TRUE)
	  if (is.null(mapmax)) mapmax=max(results[,3], na.rm=TRUE)
	  if (!is.null(map)) {
		  if (add==T) {add=F; warning("add=T ignored because the map argument was used")}  
		  if (inherits(map,"map")) maprange = map$range else
		  if (inherits(map,"SpatialPolygonsDataFrame")) {
			  mapcenter = apply(bbox(map),1,mean)
			  center2edges = c(-1,1)*max(apply(bbox(map),1,diff))/2
			  maprange = c(mapcenter[1]+center2edges,mapcenter[2]+center2edges)
		  } else
		  maprange = c(Inf,-Inf,Inf,-Inf)
	  }
	  dataXmin=min(results[,1],if(!is.null(map)) maprange[1], na.rm=TRUE)
	  dataXmax=max(results[,1],if(!is.null(map)) maprange[2], na.rm=TRUE)
	  dataYmin=min(results[,2],if(!is.null(map)) maprange[3], na.rm=TRUE)
	  dataYmax=max(results[,2],if(!is.null(map)) maprange[4], na.rm=TRUE)
	  offsetX=(dataXmax-dataXmin)/5
	  offsetY=(dataYmax-dataYmin)/5
	  ## Color distribution
	  col.len = length(col.seq)
#	  col.seq = rev(rainbow(2252,start=0,end=0.66))    # original RGB palette
      if(!missing(legend.add.line) & anchor){ 	    
        skip = FALSE
	    if(legend.add.line < mapmin | legend.add.line > mapmax) {
	   	  warning(paste("legend.add.line =",legend.add.line,
	      "is outside the range of (mapmin, mapmax)."))
	      skip = TRUE
	      }
	    if (!skip & !exp) {	
          diff1 = legend.add.line - mapmin
          diff2 = mapmax - legend.add.line 
          pos.ref = round(col.len/2)
          col.inc = max(diff1, diff2) / ((col.len-1)/2)  # change in risk per color increment
            if (diff1 > diff2) {
            r.trunc = pos.ref + round(diff2/col.inc) 
            col.seq = col.seq[1:r.trunc]
            }
            if (diff2 > diff1) {
            l.trunc = pos.ref - round(diff1/col.inc) 
            col.seq = col.seq[l.trunc:col.len]
            }            	
          }
	  if (!skip & exp) {	
          diff1 = log(legend.add.line) - log(mapmin)
          diff2 = log(mapmax) - log(legend.add.line) 
          pos.ref = round(col.len/2)
          col.inc = max(diff1, diff2) / ((col.len-1)/2)  # change in log risk per color increment
          if (diff1 > diff2) {
            r.trunc = pos.ref + round(diff2/col.inc) 
            col.seq = col.seq[1:r.trunc]
            }
          if (diff2 > diff1) {
            l.trunc = pos.ref - round(diff1/col.inc) 
            col.seq = col.seq[l.trunc:col.len]
            }            	
          }
          col.len = length(col.seq)
        }  	
	  if (!exp) qu = seq(mapmin,mapmax,length=col.len-1)
	  if (exp) qu = exp(seq(log(mapmin),log(mapmax),length=col.len-1))
	  grad = cut(results[,3],breaks=c(0,qu,Inf),labels=F)
	  ## plot the heatmap
	  par(xpd=TRUE)
	  if (add==T) points(obj$grid,col=col.seq[grad],pch=15,cex=ptsize,...) else
	  if (axes==F) {
	    mmai = if(missing(mai))c(0,0.35,0,0.35) else mai
		  par(mai=mmai)
		  plot(obj$grid,col=col.seq[grad],pch=15,cex=ptsize,type="p",
			  xlim=c(dataXmin-0.4*offsetX,dataXmax+0.4*offsetX),
			  ylim=c(dataYmin-0.8*offsetY,dataYmax),ann=F,axes=F,...)
	    }  else
	  if (axes==T) { ## add axes if needed
  	  mmai = if(missing(mai))c(1,0.82,1,0.5) else mai
	    par(mai=mmai)
	  	plot(obj$grid,col=col.seq[grad],pch=15,cex=ptsize,type="p",
		  	xlab="", xaxt="n", ylab="")
		  axis(3)
		  mtext(names(obj$grid[1]), side=3, line=2)
		  mtext(names(obj$grid[2]), side=2, line=2)
	  }
	  
	  ## Add the map
	  if (!is.null(map)) {
		  if (inherits(map,"SpatialPolygonsDataFrame")) plot(map, border=gray(border.gray), add=T) else {
			  if (!inherits(map,"map")) warning("map class not recognized--attempting generic plot")
			  lines(map,col=gray(border.gray),type="l")
		  }
	  }
	
	  # Contour lines / isobles
	  if (contours!="none") {		# if contour lines should be drawn
	    result_contour = NULL
	    if(contours=="interval"){ ## contours based on confidence intervals
	      if(any(c(is.null(obj$conf.low),is.null(obj$conf.high))))
	        warning("interval contours omitted because no conf.low or conf.high in obj")
	      else{
	        if(!exp)
	          result_contour = apply(cbind(obj$conf.low,obj$conf.high),1,
	          function(x){if(any(is.na(x))) 0 else 
	             sign(x[1]-interval.exclude)*((x[1]-interval.exclude)*(x[2]-interval.exclude)>=0)})
	        else
	          result_contour = apply(cbind(exp(obj$conf.low),exp(obj$conf.high)),1,
	          function(x){if(any(is.na(x))) 0 else sign(x[1]-exp(interval.exclude))*
	               ((x[1]-exp(interval.exclude))*(x[2]-exp(interval.exclude))>=0)})
	        levels= c(0,1)
	        labels = c("lower","higher")
	        drawlabels=if(!missing(contours.drawlabels))contours.drawlabels else FALSE
	        lwd=if(!missing(contours.lwd)) contours.lwd else 2
	        lty=if(!missing(contours.lty)) contours.lty else 1
	      } 
	    }else{
	      if(!is.element(contours,names(obj))){
	        if(contours =="permrank"){ # for back-compatibility
	          contours = "pointwise.permt"
	          contours.levels = c(alpha/2, 1-alpha/2)
	        }else{
	          if(contours == "response")  ## for back-compatibility
	          contours = "fit"
	          else warning(paste("contours omitted because no",contours,"in obj"))
	        }
	      }
	      if(is.element(contours,names(obj))){
	        result_contour = obj[[contours]]
	        if(exp) result_contour = exp(result_contour)
	        levels= if(!missing(contours.levels)) contours.levels else pretty(range(result_contour,finite=TRUE),10) 
          labels = levels
	        drawlabels=if(!missing(contours.drawlabels))contours.drawlabels else FALSE
	        lwd=contours.lwd
	        lty=contours.lty
	      }
	    }
	    if(!is.null(result_contour)){
    	  df=cbind(results[(order(results[,2])),1:2],result_contour[(order(results[,2]))])
  		  cx=as.matrix(unique(df[(order(df[,1])),1]))
  		  cy=as.matrix(unique(df[,2]))
  		  cz=as.matrix(reshape(df,v.names=names(df)[3],idvar=names(df)[1],
  			  timevar=names(df)[2],direction="wide")[,-1])
  		  cz=cz[order(unique(df[,1])),]	
  		  contour(x=cx, y=cy, z=cz, add=TRUE,levels=levels, labels = labels,drawlabels=drawlabels, lwd=lwd, lty=lty, labcex = contours.labcex)
	    }
	  }
		
	  # Legend
	  leglab = legend.name
    fY = 0.5 + 0.5*(axes==T)
	  ypos = dataYmin-fY*offsetY
	  len = (dataXmax-dataXmin)*7/12
	  points(dataXmin+(1:col.len*len/col.len),rep(ypos,col.len),cex=1.6,col=col.seq,pch=15)
	  text(x=dataXmin,y=ypos,pos=1,labels=format(round(mapmin,2),digits=3),cex=.8*legend.cex)
	  text(x=dataXmin+len,y=ypos,pos=1,labels=format(round(mapmax,2),digits=3),cex=.8*legend.cex)
	  text(x=dataXmin+len/2,y=ypos,pos=3,labels=leglab,cex=legend.cex) 
    if(!missing(legend.add.line)){  # Add referent line to legend
    	  if (!exp) xshift = len*(legend.add.line-mapmin)/(mapmax-mapmin)
    	  if (exp) xshift = len*(log(legend.add.line)-log(mapmin))/(log(mapmax)-log(mapmin)) 
      points(dataXmin+xshift,ypos,cex=.8*legend.cex,col="black", pch="|", lwd=2)  
      text(x=dataXmin+xshift,y=ypos,pos=1,labels=paste(format(legend.add.line,nsmall=1)),cex=.8*legend.cex)
    }
	  # North arrow
	  if (arrow) {
		  points(dataXmax,dataYmin-0.65*offsetY,cex=1.2,col="black", pch="|", lwd=2)
		  points(dataXmax-0.01*offsetX,ypos,col="black", bg="black", pch=24)
		  text(x=dataXmax,y=ypos,"N",cex=legend.cex,pos=3)
	  }

	  # Scale bar
	  if (!is.null(map) & inherits(map,"SpatialPolygonsDataFrame")) {
		  d = 0.17*(dataXmax - dataXmin)
		  d = signif(d,1)
		  leftedge = dataXmin+1.125*len
		  lines(c(leftedge,leftedge+d),c(ypos,ypos),col="black",lwd=3)
		  points(leftedge,ypos,cex=1.2,col="black", pch="|", lwd=2)
		  points(leftedge+d,ypos,cex=1.2,col="black",pch="|",lwd=2)
		  {scdist=d; units="m"}
                  if (!is.na(proj4string(map))) units=strsplit(grep('+units=',
			strsplit(proj4string(map),"[ ]")[[1]],value=T),'=')[[1]][2]
		  if (d>=1000 & units=="m") {scdist=d/1000; units="km"} 
		  sclab = paste(as.character(format(scdist,digits=2)),units)
		  text(x=leftedge+0.5*d,y=ypos,sclab,cex=0.8*legend.cex,pos=1)
	  }
	  par(mai=op.mai,xpd=op.xpd)
  }
