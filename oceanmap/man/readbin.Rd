\name{readbin}
\alias{readbin}
\title{
Returns \code{'.gz'}-file as matrix or raster-object}
\description{
Returns \code{'.gz'}-file as \link[base]{matrix} or \link[raster]{raster}-object.}
\usage{
readbin(filename, area, Image = F, byte = F, Raster = T)
}
\arguments{
  \item{filename}{
Character string indicating search criteria for the \code{'.gz'}-file of interest. Only \code{'.gz'}-files with valid filenames can be read, consisting of:

\code{area, source, parameter, resolution, timestep, date1, date2} and \code{option}-criteria, separated by an underscore with only \code{option} being aligned by a point and ending with \code{'.gz'}, e.g.:

\code{area_source_parameter_resolution_timestep_date1_date2.option.gz}.

See \link{region_definitions} for valid area- and \link{parameter_definitions} for valid \code{parameter}-values, respecively.
}
  \item{Image}{
whether the a the \code{'.gz'}-file should be plotted immediately using \link[fields]{image.plot}-function of the \link{fields}-package (default is \code{FALSE})
}
  \item{byte}{
whether the a the data of the \code{'.gz'}-file should be returned unconverted as a byte-values (default is \code{FALSE})
}
  \item{Raster}{
whether the a the data of the \code{'.gz'}-file should be returned in a \link[raster]{raster}-object (default is \code{TRUE})
}
  \item{area}{
Character string identifying the region that should be extracted. If missing, region is derived from the \code{'.gz'}-filename. 
See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.}
}
\seealso{ \link{writebin}, \link{regions}, \link[raster]{crop}, \link{raster2matrix}, \link{param_convert}} 

\author{
Robert K. Bauer
}

\keyword{parameter_definitions}

\examples{
### Example: read and plot '.gz'-file
path <- system.file("test_files", package="oceanmap")
check_gzfiles(folder=path) # return file summary-table
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files

### all manual:
obj <- readbin(gz.files[2],area='lion')
obj
ticks <- seq(20,30,5)
data('cmap')
image(obj,zlim=range(ticks),col=cmap$jet)
plotmap('lion',add=TRUE) # add landmask
#set.colorbar(ticks=ticks,cb.title='cb.title',cb.xlab='cb.xlab')

### using v:

## ticks set by adaptive.vals
v(obj,varname="sst2",cb.title='cb.title',cb.xlab='cb.xlab') 

## ticks set by parameter definition
v(obj,varname="sst2",cb.title='cb.title',cb.xlab='cb.xlab',adaptive.vals=FALSE) 

### extracting subregion:
obj <- readbin(gz.files[2])
area.extent <- extent(c(5,10,35,40))
subarea <- crop(obj,area.extent)
# v(subarea)

## getting average value:
mean(subarea[,],na.rm=TRUE)
}