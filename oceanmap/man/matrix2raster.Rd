\name{matrix2raster}
\alias{matrix2raster}
\title{
Converts a matrix to a RasterLayer or arrays to a RasterStack-object}
\description{
\code{matrix2raster} Converts a matrix to a RasterLayer or arrays to a RasterStack-object.
}
\usage{
matrix2raster(z,x,y,layer,proj="+proj=longlat")
}
\arguments{
  \item{z}{
matrix or array to be converted.
}
  \item{x}{
optional x-coordinates giving the horizontal \link[base]{range} of the raster layer, its size does not need to coincide with ncol(z)! 
}
  \item{y}{
optional y-coordinates giving the verical \link[base]{range} of the raster layer, its size does not need to coincide with nrow(z)! 
}
  \item{layer}{
layer to be selected (only valid if z is an array).
}
  \item{proj}{
optional argument, setting the coordinate reference system (CRS) of a Raster* object (default is \code{+proj=longlat}).
}
}

\author{
Robert K. Bauer
}

\examples{
## Example 1: convert a matrix
m <- matrix(3,2,2)
matrix2raster(m)

## Example 2: convert an array
a <- array(3,dim=c(2,2,2))
matrix2raster(a)
matrix2raster(a,layer=1)

## Example 3: convert '.nc'-file to raster-object manually
owd <- getwd()
path <- system.file("test_files", package="oceanmap")
ncfile <- Sys.glob(paste0(path,'/herring*.nc')) # load sample-'.nc'-files

library('ncdf4')
library('raster')
nc <- nc_open(ncfile) # open netcdf file
z <- ncvar_get(nc,'Conc')[,,1]
lon <- as.vector(ncvar_get(nc,'lon')) # fillvalues are automatically replaced by NA
lat <- as.vector(ncvar_get(nc,'lat')) # fillvalues are automatically replaced by NA
matrix2raster(z,x=lon,y=lat)  

## Example 4: convert '.nc'-file to raster-object using nc2raster
nc2raster(ncfile,varname='Conc',layer=1:4)

}
