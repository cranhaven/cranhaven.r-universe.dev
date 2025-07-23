\name{get.bathy}
\alias{get.bathy}
\title{
Returns bathymetric data from the NOAA ETOPO1 database as RasterLayer, given coordinate bounds and resolution.
}

\description{
Returns and optionally stores bathymetric data from the ETOPO1 database hosted on the NOAA server as a RasterLayer, based on the defined resolution and provided coordinate bounds or region definition. Stored bathymetry files can be reloaded through the same function call.
}

\usage{
get.bathy(v_area, lon, lat, resolution=4, keep=F ,
          savename.bathy, folder.bathy, visualize=T, terrain=F,...)
}

\arguments{
\item{v_area}{
  character string identifying the region that should be plotted, or in case of \code{x == 'bathy'}, also a Raster* or Extent object. If missing, region is derived from the \code{'.gz'}-filename. 
  See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

 \item{lon,lat}{
 longitude and latitude describing the extend of the region of interest.
 }

  \item{resolution}{
resolution of the bathymetric grid, in minutes (default is 4).
}

  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE).
}

  \item{savename.bathy}{
savename for the bathymetric data file, if not specified set to type 'bathy_lon-lat_res.resolution.dat' or 'bathy_v_area_res.resolution.dat'.
}

  \item{folder.bathy}{
directory where bathymetric data should be saved (default is current working directory).
}

  \item{visualize}{
whether the bathymetric data should be plotted instantly.
}

  \item{terrain}{
whether the to keep terrain data (default is \code{FALSE}). If set \code{FALSE} and \code{visualize} is \code{TRUE}, \code{grid} command in \link{plotmap} is disabled!
}

  \item{...}{
additional arguments to be passed to \link{v}, used if \code{visualize} is set \code{TRUE}.
}
}
\seealso{\link{v}, \link{add.region}, \link{region_definitions}, \link{regions}, \link{writebin}, \link{get.bathy}} 

\author{
Robert K. Bauer
}

\keyword{bathymetry}
\keyword{image plot}


\examples{

## Example 1: load & plot bathymetry of the Baltic Sea, defined by longitudes and latidtues
lon <- c(9, 31)
lat <- c(53.5, 66)
# get.bathy(lon=lon, lat=lat, main="Baltic Sea", cbpos='r')

## Example 2: plot bathymetry using a v_area-keyword
#get.bathy("lion",res=4, keep=T) # can take some time, requires server connection!
#get.bathy("lion",res=1, keep=T,visualize=FALSE)

## Example 3: plot landmask of the Baltic Sea defined by an extent- or raster-object
library('raster')
ext <- extent(lon,lat)
#get.bathy(ext,visualize=T,main="Baltic Sea",res=4,levels=200) # extent-object

## Example 4: plot bathymetry and topography of the western Mediterranean Sea
### a) download, assign and save bathymetry
# bathy <- get.bathy("medw4",visualize=F,terrain=T,res=3,keep=T)
# # load('bathy_medw4_res.3.dat',verbose = T); bathy <- h
# par(mfrow=c(2,1))
# v(bathy,param="bathy",subplot = T)
# get.bathy("medw4",visualize=T,terrain=F,res=3,levels=c(200,2000), 
# subplot = T,grid=F) # show contours

### b) only contour lines:
# par(mfrow=c(1,2))
# h <- get.bathy("lion",visualize=T,terrain=F,res=3,levels=c(200,2000),
#                v_image=F, subplot=T,grid=F)

### use v-function for same plot but on subregion:
# v(h,v_area = "survey", param="bathy",subplot = T, v_contour = T, 
#   v_image = F, levels=c(200,2000)) 

}
