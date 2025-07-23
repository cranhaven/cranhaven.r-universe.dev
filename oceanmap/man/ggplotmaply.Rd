\name{ggplotmaply}
\alias{ggplotmaply}

\title{
Converts a ggplot2 object from ggplotmap() to plotly
}
\description{
This function converts a ggplot2 object created by oceanmap::ggplotmap() to a plotly object.
}
\usage{
ggplotmaply(ggobj, fixedrange=F, grid=F,expand=3)

}
\arguments{
  \item{ggobj}{
Character string identifying regions predefined by the \link{region_definitions}-dataset, Raster* or Extent object (corresponds to \code{v_area} of the \link{v}-function). If missing, region is derived from geographical coordinates, denoted by lat and lon. See \link{add.region} to define new region definitions and \link{delete.region} to delete unproper region definitions.
}
  \item{fixedrange}{
Vector returning longitude coordinates of the area to be plotted.
}
  \item{grid}{
whether a grid should be plotted (default is \code{TRUE})
}
  \item{expand}{
By default, the underlying \link{ggplotly}-function does not stick to the plotting region of the ggobj, but extends it. This can result in missing countries or islands. The \code{expand}-argument extends the plotly-plotting window in each direction in order to cover the corresponding landmasks.)
}
}
\details{
\code{ggoplotmaply} uses the \code{ggplotly} functions to convert the ggplot object into the plotly format.
}
\seealso{ \code{ \link{ggplotmap}, \link{ggplotly}} } 


\author{
Robert K. Bauer
}

\examples{
library(ggplot2)

#### Example 1: plot landmask of the Western Mediterranean Sea
## a) by using longitude and latitude coordinates:
# lon <- c(-6, 16.5)
# lat <- c(34, 44.5)
# ggobj <- ggplotmap(xlim=lon, ylim=lat)
# ggobj
# ggplotmaply(ggobj,expand = 10) ## we need to expand the plotting region


## b) plot landmask of the Western Mediterranean Sea by using an extent-object:
# library('raster')
# ext <- extent(lon, lat)
# plotmap(ext, main="Western Mediterranean Sea") # extent-object
# ggobj <- ggplotmap(ext)
# ggobj
# ggplotmaply(ggobj)


## c) plot landmask of the Western Mediterranean Sea by using a raster-object:
# r <- raster(ext)
# ggobj <- ggplotmap(r)
# ggobj
# ggplotmaply(ggobj)
# ggplotmaply(ggobj)

## d) plot landmask of the entire Mediterranean Sea by using keyword:
 ggobj <- ggplotmap("med4") + 
          geom_point(data=data.frame(x=3.7008, y=43.4079),aes(x,y),size=5,colour="blue")
# ggobj
# ggplotmaply(ggobj,expand = 10)


## e) add landmask to raster image plot (similar to v()-call)
# library(dplyr)
# library(ggplot2)
# data(cmap)
# setwd(system.file("test_files", package="oceanmap"))
# nc <- nc2raster(ncfiles[1])
# rs2df <- nc[[1]] %>% ## take first layer
#          rasterToPoints() %>% ## convert raster to xyz matrix
#          as.data.frame() ## convert to data frame
# names(rs2df) <- c("Lon","Lat","Conc") ## reset names (important for ggplotmaply hover text)
# ggobj <- ggplot() + geom_raster(data = rs2df, aes(x=Lon,y=Lat,fill=Conc)) 
# ggobj_with_land_mask <- ggplotmap(add_to = ggobj) + 
#                         scale_fill_gradientn(colours=cmap$jet) # change colorbar
# ggobj_with_land_mask 
# ggplotmaply(ggobj_with_land_mask)


}

