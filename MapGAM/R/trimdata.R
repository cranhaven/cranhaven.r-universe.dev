#***********************************************************************************
#
# Trim a Data Set To Map Boundaries 
# Copyright (C) 2016, 2022, 2023, The University of California, Irvine
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
trimdata <- function(rdata, map, Xcol=2, Ycol=3, rectangle=F, buffer=0.05) {
	if (inherits(map,"Spatial") | inherits(map,"Raster")) {  
		mrmat <- bbox(map)
        Xrange <- mrmat[1,]
        Yrange <- mrmat[2,]
        mr <- c(Xrange,Yrange)
        centroid <- data.frame(X=mean(Xrange),Y=mean(Yrange)) 
    }
    else if (inherits(map,"sf")) {
      	mrsf = st_bbox(map)
      	Xrange = mrsf[c(1,3)]
      	Yrange = mrsf[c(2,4)]
        mr <- c(Xrange,Yrange)
        centroid <- data.frame(X=mean(Xrange),Y=mean(Yrange)) 
    }
    else if (inherits(map,"map")) {
      	mr = map$range
      	Xrange = mr[1:2]
      	Yrange = mr[3:4]
        centroid <- data.frame(X=mean(Xrange),Y=mean(Yrange)) 
	} else
		stop(paste("map class not recognized by trimdata function"))
	# if rdata has only 2 columns assign them as X and Y
	if (length(rdata)==2) {
		Xcol = 1
		Ycol = 2
	}
    dataXmin=min(rdata[,Xcol])
	dataXmax=max(rdata[,Xcol])
	dataYmin=min(rdata[,Ycol])
	dataYmax=max(rdata[,Ycol])
	dr = c(dataXmin,dataXmax,dataYmin,dataYmax)		# data range
	
	# Print warning if the map bbox centroid is not within the data range
	  mapmean=(dataXmin<centroid$X)+(centroid$X<dataXmax)+(dataYmin<centroid$Y)+(centroid$Y<dataYmax)
	if (mapmean!=4) {				
		warning("The map centroid is not in the range of the data.  This might indicate differing projections.")
		cat(paste(c("Data X: "," to ","; Data Y: "," to "),signif(dr,digits=4),sep="",collapse=""),fill=TRUE)
		cat(paste(c("Map X: "," to ","; Map Y: "," to "),signif(mr,digits=4),sep="",collapse=""),fill=TRUE)
		cat(paste(c("Map centroid: ",", "),signif(centroid,digits=4),sep="",collapse=""),fill=TRUE)
	}
	if (rectangle==T | inherits(map,"Raster")) { 
		# Keep subset of data within a rectangular boundary using map coordinate ranges
		if (length(buffer)<=2) buffer = rep(c(mr[2]-mr[1],mr[4]-mr[3])*buffer,each=2) 
		mr = mr + c(-1,1,-1,1)*buffer
		rdata = rdata[rdata[,2] > mr[1] & rdata[,2] < mr[2] & rdata[,3] > mr[3] & rdata[,3] < mr[4],]
		cat(paste(c("Data trimmed to rectangle on X: "," to "," and Y: "," to "),
				signif(mr,digits=4),sep="",collapse=""),fill=TRUE)
	} else {
		# Keep subset of data within boundaries of map
		map.sf <- st_as_sf(map)
		rdata_sf <- rdata
		if (!inherits(rdata,"sf")) {
			mapcrs <- st_crs(map.sf)
			rdata_sf <- st_as_sf(rdata,coords=c(Xcol,Ycol),crs=mapcrs)
			cat(paste(c("Data assumed to use same CRS as map:",mapcrs)), fill=T)
			}
		if(inherits(map,"map")) sf_use_s2(FALSE)     # fix errors when using maps library
		inside <- lengths(st_intersects(rdata_sf,map.sf)) > 0
		rdata <- na.omit(rdata[inside,])
		sf_use_s2(TRUE)
	}
	return(rdata)
}
