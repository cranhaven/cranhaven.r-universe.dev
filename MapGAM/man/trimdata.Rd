\name{trimdata}
\alias{trimdata}
\title{
Trim a Data Set To Map Boundaries 
}
\description{
Takes a subset of a data frame: returns rows with geolocations inside the boundaries determined by a map.  If \code{rectangle=FALSE}, strict map boundaries are used.  If \code{rectangle=TRUE}, a rectangular boundary is determined based on the range of X and Y coordinates for the map, along with an optional buffer.   
}
\usage{
trimdata(rdata, map, Xcol=2, Ycol=3, rectangle=F, buffer=0.05)
}
\arguments{
  \item{rdata}{
the data set (required).  If \code{rdata} has only 2 columns then they are assumed to be the X and Y coordinates, in that order.  If \code{rdata} has more than 2 columns then identify the positions of the X and Y coordinates with \code{Xcol} and \code{Ycol}, respectively.
}
  \item{map}{
a map for clipping the grid, provided in a recognized format.  Supported map classes include "map" (the format produced by the maps package), "sf" (the ISO standard format for maps, supported by the sf package), "Raster" (used by the raster package), or "Spatial" (a legacy format used by the maptools package).  To load a map from an ESRI shapefile into R, use the \code{st_read} function in the sf library, the \code{readOGR} function in the rgdal package, or the \code{readShapePoly} function in the maptools package (soon to be deprecated).  For Raster format maps, the grid will be rectangular, rather than clipped to the raster boundaries.
}
  \item{Xcol}{
the column number of rdata for the X coordinates of geolocation.  Only used if rdata has >2 columns.
  }
  \item{Ycol}{
the column number of rdata for the Y coordinates of geolocation.  Only used if rdata has >2 columns.
  }
  \item{rectangle}{
 if \code{rectangle=FALSE} (default), only data with geolocations strictly within the map boundaries are retained.  If \code{rectangle=TRUE}, only data with geolocations within a rectangular boundary are retained.  The rectangular boundary coordinates are determined using the ranges of X and Y coordinates for the map, along with an optional buffer.   
}
  \item{buffer}{
a fraction of the map range to add to either side of the rectangular boundary before trimming the data (default 5\%).  The buffer argument is ignored if \code{rectangle=FALSE}.  If a vector of length 2 is supplied, the first value as the buffer fraction for the X coordinate range and the second value is used as the buffer fraction for the Y coordinate range. 
}
}
\details{
Functions from the sf package are used to convert map formats and clip the grid to points inside the map boundaries.  Be sure to use \code{fill=T} when using the \code{map} function to produce maps for \code{trimdata}: maps produced using the default \code{fill=F} argument do not work properly with this function!  If the map bounding box centroid is not in the range of the data, a warning message is printed; this might indicate differing projections, but can occur naturally when the data were not sampled from the entire extent of the map, or when map boundaries are concave.      
}

\value{
A subset of the rdata data frame containing only those rows with geolocations within the specified boundaries.      
}

\author{
Veronica Vieira, Scott Bartell, and Robin Bliss

Send bug reports to \email{sbartell@uci.edu}.
}

\seealso{
\code{\link{predgrid}},
\code{\link{optspan}}, 
\code{\link{modgam}}, 
\code{\link{colormap}}.
}

\examples{
	
# This example uses the "sf" package to read in an external ESRI shapefile for Maine
if (require(sf)) {
  # download example shapefile zip from github, and unzip
  zippath <- paste(tempdir(),"Income_schooling.zip",sep="/")	
  download.file("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling.zip", 
              destfile = zippath, mode='wb')
  unzip(zippath, exdir = tempdir())

  # read shapefile into sf format 
  shppath <- paste(tempdir(),"Income_schooling.shp",sep="/")	
  basemap0 <- st_read(shppath)  
  
  # Create example data by randomly sampling within bounding box
  rs <- st_bbox(basemap0)   # get ranges of X and Y
  MEdata <- data.frame(X=runif(300,rs[1],rs[3]), Y=runif(300,rs[2],rs[4]))
  plot(basemap0["NAME"], reset=FALSE)
  plot(st_as_sf(MEdata,coords=1:2), add=TRUE)  # plot data in black
  
  # trim data to basemap, and plot trimmed data with red X's
  dME <- trimdata(MEdata, basemap0)
  plot(st_as_sf(dME,coords=1:2), col="red", pch="X", add=TRUE) 
  dev.off() 	  # clear map settings
}
	
# The following examples use the "maps" package, which includes its own maps
if (require(maps)) {
  data(beertweets)

  ### Trim beer tweet data to US base map, and plot them
  basemap1 <- map("usa", fill=TRUE, col="transparent")	
  dUS <- trimdata(beertweets, basemap1)	 				 
  # Plot tweet locations (beer tweets in red)
  points(dUS$longitude, dUS$latitude, col=dUS$beer+1, cex=0.5)		
  dev.off()  # clear map settings
  
  ### Trim beer tweet data to Texas base map, and plot them
  basemap2 <- map("state", regions="texas", fill=TRUE, col="transparent") 
  dTX <- trimdata(beertweets, basemap2) 								
  # Plot tweet locations (beer tweets in red)
  points(dTX$longitude, dTX$latitude, col=dTX$beer+1, cex=0.5) 
}
}
\keyword{misc}
