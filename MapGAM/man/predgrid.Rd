\name{predgrid}
\alias{predgrid}
\title{
Create a Grid and Clip It to a Map and Data Bounds
}
\description{
Creates a data frame containing a rectangular grid of points to cover the range of X and Y coordinates provided in a data set, and trims the grid so that the points do not extend beyond the boundaries shown on a map.  Users can omit a data set to create a grid covering the whole map, or omit a map to create a grid covering the whole data set. 
}
\usage{
predgrid(dataXY = NULL, map = NULL, nrow = 100, ncol = 100, X = NULL, Y = NULL)
}
\arguments{
  \item{dataXY}{
A data frame with X and Y coordinates.  If the data frame has more than 2 columns, the X and Y coordinates must be in the 2nd and 3rd columns, respectively (the format for the data argument in the \code{modgam} function).  If the data frame has only 2 columns, the X coordinates must be in the 1st column and Y coordinates in the 2nd column.  If no data are provided for this argument or for the X and Y arguments, the grid will cover the entire map.          	
}
  \item{map}{
a map for clipping the grid, provided in a recognized format.  Supported map classes include "map" (the format produced by the maps package), "sf" (the ISO standard format supported by the sf package), "Raster" (used by the raster package), or "Spatial" (a legacy format used by the maptools package).  To load a map from an ESRI shapefile into R, use the st_read function in the sf library, the readOGR function in the rgdal package, or the readShapePoly function in the maptools package. If \code{map = NULL} (default), a rectangular grid is produced covering the range of geolocations within \code{dataXY}.  For Raster format maps, the grid will be rectangular, rather than clipped to the raster boundaries.  If no map is provided the grid will cover the whole data set.  
}
  \item{nrow}{
the number of rows in the grid (default=100).
}
  \item{ncol}{
the number of columns in the grid (default=100).
}
  \item{X}{
a vector of X coordinates, supplied instead of (or in addition to) dataXY.  If both X and dataXY are provided, X will be used instead of the corresponding column of dataXY.
}    
  \item{Y}{
a vector of Y coordinates, supplied instead of (or in addition to) dataXY.  If both Y and dataXY are provided, Y will be used instead of the corresponding column of dataXY.
}    
}
\details{
\code{predgrid} creates a grid of dimensions nrow*ncol using the range of X and Y coordinates (e.g., longitude and latitude) in the data frame supplied as \code{dataXY}. If the map argument is used, the function \code{trimdata} is automatically used to clip the grid.  Users should be sure to use the same projection for the map and the data; putting both on the same plot can help reveal differing projections.  If the map centroid is not in the range of the data a warning message is printed; this might indicate differing projections but can occur naturally when the data were not sampled from the entire extent of the map, or when map boundaries are concave.  
}
\value{
A data frame with X and Y coordinates in the first two columns.  The column names from dataXY are used for the output.  If the columns of dataXY are unnamed then the names "X" and "Y" are assigned to the data frame.        
}

\author{
Veronica Vieira, Scott Bartell, and Robin Bliss

Send bug reports to \email{sbartell@uci.edu}.
}

\seealso{
\code{\link{trimdata}}, 
\code{\link{optspan}}, 
\code{\link{modgam}}, 
\code{\link{colormap}}.
}

\examples{
# define a rectangular 100x100 grid covering the MA data 
data(MAdata)
gamgrid <- predgrid(MAdata)
# plot the grid points
plot(gamgrid$Xcoord, gamgrid$Ycoord, cex=0.1, col="red")
# and the data locations
points(MAdata$Xcoord,MAdata$Ycoord)

# But that grid extends beyond the state boundaries and into the ocean!
# Better to also clip the grid to a map of MA using the following code:
\donttest{
# Clip a 50x50 grid covering the MA data to a map of MA 
data(MAmap)
gamgrid2 <- predgrid(MAdata, map=MAmap, nrow=50, ncol=50)
# plot the MA map and grid points
plot(MAmap)
points(gamgrid2, cex=0.1, col="red")
}
}

\keyword{misc}
