\name{cmap}
\alias{cmap}
\alias{cmap_topo}
\docType{data}
\title{
color maps 
}
\description{
list holding different color maps that can be used in image plots (see: \link{v}, \link{get.bathy}, \link[graphics]{image}, \link[fields]{image.plot}s, \link{clim_plot})

available color maps are: \code{ano}, \code{bathy}, \code{blue}, \code{chla}, \code{haxby}, \code{jet} (obtained from matlab), \code{rainbow}, \code{red}, \code{orange}, \code{green}, \code{sst} and \code{haxbyrev}.
}
\usage{
data(cmap)
data(cmap_topo)
}
\format{
  list
}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data('cmap') # load color maps data
names(cmap) # list available color maps

path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
# figure(width=15,height=15)
# par(mfrow=c(4,5))
# for(n in names(cmap)) v(gz.files[2], v_area='lion', subplot=TRUE,
#                         pal=n, adaptive.vals=TRUE, main=n)

## simple example of the \link{image}-function
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100))
image(x, y, volcano, col = cmap$jet) # jet color map
image(x, y, volcano, col = cmap$haxby) # haxby color map
image(x, y, volcano, col = cmap$chla) # chlorophyll color map
image(x, y, volcano, col = cmap$sst) # sst color map

data(cmap_topo)
image(x, y, volcano, col = cmap_topo$col) # topography color map

## another example: plot bathymetry and topography of the western Mediterranean Sea
#get.bathy("medw4",visualize=T,terrain=T,res=3)
#get.bathy("medw4",visualize=T,terrain=F,res=3,levels=c(200,2000)) # show contours

}
\keyword{colorbar}

\author{
Robert K. Bauer
}
