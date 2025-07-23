\name{plotmap}
\alias{plotmap}
\alias{ggplotmap}

\title{
plots landmask of a defined region}
\description{
plots the landmask of a region defined by a \code{region}-key word, georgraphical coordinates (\code{lon}gitude and \code{lat}itude), a raster- or \link[raster]{extent}-object. See \link{add.region} to add and save new region definitions. Attention! Unlike \link{add.region}, \code{plotmap} does not include colorbar placement (see: \link{set.colorbar})
}
\usage{
plotmap(region=v_area, lon, lat, add=F, asp,
        grid=T, grid.res, resolution=0, 
        main, axes=T, axeslabels=axes, ticklabels=T, cex.lab=0.8, cex.ticks=0.8, 
        fill.land=T, col.land="grey", col.bg=NA, border='black', bwd=2, las=1, 
        v_area, xlim, ylim
        )

ggplotmap(region=v_area, lon=xlim, lat=ylim, add_to, asp, 
          grid=T, grid.res, resolution=0, 
          main, axes=T, axeslabels=axes, ticklabels=T,
          fill.land=T, col.land="grey", col.bg=NA, border='black', 
          col.scale = "black", bwd=1.5, v_area, xlim, ylim)
}
\arguments{
  \item{region, v_area}{
Character string identifying regions predefined by the \link{region_definitions}-dataset, Raster* or Extent object (corresponds to \code{v_area} of the \link{v}-function). If missing, region is derived from geographical coordinates, denoted by lat and lon. See \link{add.region} to define new region definitions and \link{delete.region} to delete unproper region definitions.
}
  \item{lon, xlim}{
Vector returning longitude coordinates of the area to be plotted.
}
  \item{lat, ylim}{
Vector returning latitude coordinates of the area to be plotted.
}
  \item{add, add_to}{
whether the a the landmask should be added to an existent figure (default is \code{FALSE}) or an existing ggplot object, in case of
 \code{ggplotmap}.
}

  \item{asp}{
numeric, giving the aspect y/x-ratio of the y- and x-axes. See \link{plot.window} for more details.
}

  \item{main}{
title to be plotted
}
  \item{axes, axeslabels}{
whether axes and axes-labels (longitude and latitude) should be plotted (default is \code{TRUE}). 
\code{axes-labels} can be a single value or a vector of size two, representing values for x and y axis, respectively.
}

  \item{ticklabels}{
whether tick-labels should be added to the axes (default is \code{TRUE}). Can be a single value or a vector.
}
  \item{cex.lab}{
font size of axis labels
}
  \item{cex.ticks}{
font size of tick labels
}
  \item{grid}{
whether a grid should be plotted (default is \code{TRUE})
}
  \item{grid.res}{
resolution of the grid, in degrees (default is is derived from the region extent)
}
\item{resolution}{
number that specifies the resolution with which to draw the map. Resolution 0 is the full resolution of the database [default]. Otherwise, just before polylines are plotted they are thinned: roughly speaking, successive points on the polyline that are within resolution device pixels of one another are collapsed to a single point (see the Reference for further details). Thinning is not performed if plot = FALSE or when polygons are drawn (fill = TRUE or database is a list of polygons).
}
  \item{bwd}{
width is of the axes bars (default is 1)
}
  \item{fill.land}{
whether the a the landmask should be filled by a color (default is \code{TRUE})
}
  \item{col.land}{
fill color of the landmask to be plotted (default is grey)
}
  \item{col.bg}{
background color (ocean) to be plotted (default is NA)
}
  \item{border}{
country border color of the landmask to be plotted (default is black)
}
  \item{col.scale}{
color of the map scale to be plotted around the map (default is black)
}

\item{las}{numeric in \{0,1,2,3\}; the style of axis labels
\describe{
  \item{0:}{always parallel to the axis [\emph{default}],}
  \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.}
  }
}

}
\details{
\code{plotmap} uses the \code{maps} and \code{maptools} functions to plot the landmask.
}
\seealso{ \code{ \link{v}, \link{regions}} } 


\author{
Robert K. Bauer
}

\examples{
#### Example 1: plot landmask of the Mediterranean Sea
## a) by using longitude and latitude coordinates:
lon <- c(-6, 37)
lat <- c(30, 46)
figure(width=9.75,height=5.28)
plotmap(lon=lon, lat=lat, main="Mediterranean Sea")
plotmap(xlim=lon, ylim=lat, main="Mediterranean Sea")
ggobj <- ggplotmap(xlim=lon, ylim=lat)
ggobj


## b) plot landmask of the Mediterranean Sea by using an extent-object:
# library('raster')
# ext <- extent(lon, lat)
# plotmap(ext, main="Mediterranean Sea") # extent-object
# ggplotmap(ext)

## c) plot landmask of the Mediterranean Sea by using a raster-object:
# r <- raster(ext)
# plotmap(r, main="Mediterranean Sea") # raster-object
# ggplotmap(r)

## d) plot landmask of the Mediterranean Sea by using a region label:
# plotmap('med4', main="Mediterranean Sea") # region-label
# regions() ## check preinstalled region label

## e) add landmask to an existing plot:
# plot(3.7008, 43.4079, xlim=lon, ylim=lat)
# plotmap(add=T)
# points(3.7008, 43.4079, pch=19)
# ggplotmap(xlim=lon, ylim=lat)

# library(ggplot2)
# ggobj <- ggplotmap("lion") + 
#         geom_point(data=data.frame(x=3.7008, y=43.4079),aes(x,y),size=5,colour="blue")
# ggobj
## ggplotmaply(ggobj)
## f <- ggplotmaply(ggobj)
## pos <- as.data.frame(list(x=c(5.83, 4.91, 5.67, 5.91, 6.31, 6.37, 
##                               5.66, 5.54, 5.51, 5.67, 5.89, 5.97),
##                           y=c(42.89, 42.27, 42.42, 42.33, 42.1, 41.92, 
##                               41.74, 41.45, 41.32, 41.21, 41.04, 40.96)
##                          ))
## library(dplyr)
## library(plotly)
## f %>% add_trace(data = pos,x = ~x, y= ~y,type='scatter',mode='marker',name="new pos") 



#### Example 2: subplots and some additional arguments of plotmap()
# par(mfrow=c(2, 1))
# plotmap('medw4', main="Western Mediterranean Sea",col.bg="darkblue")
# plotmap('medw4', main="Western Mediterranean Sea", bwd=3, border='grey', grid=FALSE)



#### Example 3: plotmap() and figure()
# do.save <- FALSE ## open a plotting window
# figure("Gulf_of_Lions_extended", do.save=do.save, width=5, height=5, type="pdf")
# plotmap("lion",col.bg='darkblue',grid=FALSE)
# close_fig(do.save)

## now resize figure manually and get new figure dimensions:
# width <- dev.size()[1]
# height <- dev.size()[2]

# do.save <- TRUE ## do NOT open a plotting window, but save figure internally
# figure("Gulf_of_Lions_extended", do.save=do.save, width=width, height=height, type="pdf")
# plotmap("lion",col.bg='darkblue',grid=FALSE)
# close_fig(do.save)


#### Example 4: between hemispheres
# par(mfrow=c(2,1))
# plotmap(lon=c(-180, 180), lat=c(-80, 80), main="map from West to East")
# plotmap(lon=c(0, 360), lat=c(-80, 80), main="map from West to East")
# plotmap(lon=c(-360, 00), lat=c(-80, 80), main="map from West to East") # same as before

#### Example 5: plot bathymetry and topography of the western Mediterranean Sea
#get.bathy("medw4",visualize=T,terrain=T,res=3)
#get.bathy("medw4",visualize=T,terrain=F,res=3,levels=c(200,2000)) # show contours
#get.bathy("lion",visualize=T,terrain=F,res=3,levels=c(200,2000),v_image=F) # show only contours

#### Example 6: testing some additional arguments
# lon <- c(-180,200); lat <- c(-80,90); 
# ext <- extent(lon, lat)
# plotmap(ext, border=NA, bwd=NA, grid=FALSE, col.land = "#9ac0cd",axes=FALSE)

}

