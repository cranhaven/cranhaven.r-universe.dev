#***********************************************************************************
#
# Create a Grid and Clip It to a Map and Data Bounds
# Copyright (C) 2016, 2023, The University of California, Irvine
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
predgrid <-  function(dataXY=NULL, map=NULL, nrow=100, ncol=100, X=NULL, Y=NULL) {
	done = FALSE
    if (!is.null(dataXY)) {
	  	XYcol = 1:2 + (length(dataXY) > 2)		# check for outcome column
		if (any(is.na(dataXY))) warning("Missing values (NA) in coordinates have been excluded")
		Xrange = range(dataXY[,XYcol[1]], na.rm=TRUE)
		Yrange = range(dataXY[,XYcol[2]], na.rm=TRUE)
		done = TRUE
	} 
	if (!is.null(X) ) {
		if (any(is.na(X))) warning("Missing values (NA) in X coordinates have been excluded")
		Xrange = range(X, na.rm=TRUE)
		done = TRUE
	}
	if (!is.null(Y) ) {
		if (any(is.na(Y))) warning("Missing values (NA) in Y coordinates have been excluded")		
		Yrange = range(Y, na.rm=TRUE)
		done = TRUE
	}
	if (!done && (inherits(map,"Spatial") | inherits(map,"RasterLayer"))) {  
		mr <- bbox(map)
        Xrange <- mr[1,]
        Yrange <- mr[2,]
		done = TRUE
    }
    else if (!done && inherits(map,"sf")) {
      	mr = st_bbox(map)
      	Xrange = mr[c(1,3)]
      	Yrange = mr[c(2,4)]
		done = TRUE
    }
    else if (!done && inherits(map,"map")) {
      	mr = map$range
      	Xrange = mr[1:2]
      	Yrange = mr[3:4]
		done = TRUE
    }
    if (!done) stop(paste("dataXY, X and Y, and/or a valid map must be specified"))
	# Creates a rectangular grid
	grid=as.data.frame(expand.grid(X=seq(Xrange[1],Xrange[2],len=ncol),
								   Y=seq(Yrange[1],Yrange[2],len=nrow)))							
	# If input data geolocation columns have names assign them to the output
	if (!is.null(names(dataXY))) names(grid) = names(dataXY)[XYcol]
	# If map is provided, grid is clipped using trimdata function
	if (!is.null(map)) grid = trimdata(grid,map) 					
	return(grid)	
}
