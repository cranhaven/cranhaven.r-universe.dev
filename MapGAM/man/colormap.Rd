\name{colormap}
\alias{colormap}
\title{
Maps Predicted Values and Clusters on a Two-Dimentional Map
}
\description{
Displays a color image map, including a legend, scale bar, and optional North arrow, showing values for a grid of points. Irregular grids are allowed.  Also draws contour lines for regions of signficantly increased or decreased predicted values ("clusters").      
}
\usage{
colormap(obj, map=NULL, exp=FALSE, add=FALSE,  mapmin=NULL, mapmax=NULL, 
         col.seq=rev(rainbow(201,start=0,end=0.66)), anchor=FALSE,
         border.gray = 0.3, contours="none", 
         contours.drawlabels=FALSE, contours.lty=1,
         contours.lwd=1, contours.levels, contours.labcex=0.7, 
         interval.exclude=0, arrow=TRUE, axes=FALSE, ptsize=0.9, mai, 
         legend.name = "predicted values",legend.cex=1,
         legend.add.line,alpha,...)
}
\arguments{
  \item{obj}{
(Required) A list containing at least these two elements:  "grid" (a 2 column data frame with X and Y coordinates) and "fit" (the value to displayed on the map).   
}
  \item{map}{
Can be used to map \code{fit} on a base map from the \code{map} function in the \pkg{maps} package, or on a base map produced by the \code{readOGR} function in the \pkg{rgdal} package.  \code{readOGR} reads maps from external files in the ESRI shapefile format.  \code{map=NULL} produces a color image without any base map.
}
  \item{exp}{
If \code{exp=T}, \code{fit} will be displayed in exponential scale.  
}
  \item{add}{
Use \code{add=T} to add the color map to an existing plot.  This will often result in loss of the legend and scale, which are added outside of the normal
    map boundaries.  \code{add} is ignored when a map is provided using the 
    \code{map} argument.
}
  \item{mapmin}{
The minimum value for the color scale legend.  When not specified, it will be set to the minimum predicted value. 
}
  \item{mapmax}{
The maximum value for the color scale legend.  When not specified, it will be set to the maximum predicted value. 
}
  \item{col.seq}{
The color sequence (palette) used to display the predicted values on the map.  The default is a 
rainbow palette, but divergent or sequential palettes may be better for some applications.  See the
colorspace library and the example below for other options.      
}
  \item{anchor}{
Use \code{anchor=TRUE} to center the color palette on \code{legend.add.line}.  When \code{anchor=FALSE} (the default) or when \code{legend.add.line} is not specified, the color palette will be centered halfway between mapmin and mapmax.  \code{anchor=TRUE} is recommended when using a divergent palette.          	
}
  \item{border.gray}{
Gray scale for the border of \code{map}, ranges from 0 to 1. It is white with \code{border.gray=1}, and black with \code{border.gray=0}. The defalt scale is 0.9.
}
  \item{contours}{
When \code{contours="interval"}, two other elements \code{conf.low} and \code{conf.high} must be included in \code{obj}, and regions excluding a user-specified \code{interval.exclude} will be circled on the map. By specifying an element name in \code{obj} for \code{contours}, contour lines for the specified element will be added on the map, e.g. \code{contours="fit"} or  \code{contours="exp.fit"} for a \code{modgam} object.  The default is "none" which produces no contour lines.  
}
  \item{contours.drawlabels}{
Use \code{contours.drawlabels = TRUE} add labels on contour lines.   
}
  \item{contours.lty}{
The line type for contour lines
}
  \item{contours.lwd}{
The width for contour lines.
} 
  \item{contours.levels}{
The levels for contour lines. When \code{exp=T}, the levels will also be used in an exponential scale.
} 
  \item{contours.labcex}{
cex for contour labelling. This is an absolute size, not a multiple of \code{par("cex")} 
}
  \item{interval.exclude}{
If \code{contours="interval"}, regions with confidence intervals excluding \code{interval.exclude} will be circled  
}
  \item{arrow}{
Use arrow=T to add a North arrow to the map.  	
}
  \item{axes}{
Use axes=T to add axes to the map (useful for chemical mixture isoboles).
}
  \item{ptsize}{
The size of the points used to fill in colors on the map.  Increase to remove white space inside the map or decrease to remove color outside the map boundaries.  NOTE: white space can also be eliminated by increasing the grid size in \code{predgrid}, which is often preferable as it results in a higher resolution map.   
}
  \item{mai}{
The margins of the plot. Details see \link{par},}
  \item{legend.name}{
The name of displayed for the legend bar.}
  \item{legend.cex}{
A numerical value giving the amount by which legend text should be magnified relative to the default.  
}
  \item{legend.add.line}{
A numerical value at which a line will be added on the legend bar to indicate the color for the value.  
}
  \item{alpha}{Levels}
  \item{...}{
Other auguments past to \link{plot} function
  }
}

\value{
Produces an image map.  If the base map is in \code{readOGR} format a scale bar is included, showing whatever measurement units are recorded in the shapefile.  If the 
units are missing from the shapefile conversion they are assumed to be meters.   
}

\author{
Scott Bartell, Lu Bai, Robin Bliss, and Veronica Vieira

Send bug reports to \email{sbartell@uci.edu}.
}

\seealso{
  \code{\link{trimdata}}, 
  \code{\link{predgrid}}, 
  \code{\link{plot.modgam}}.
}

\examples{
data(MAdata)
data(MAmap)
obj <- list(grid=data.frame(MAdata$Xcoord,MAdata$Ycoord),fit=MAdata$Mercury)
colormap(obj, MAmap, legend.name = "mercury")

# map the same data using a divergent color palette anchored to the median
if (require(colorspace)) {
  newpal <- diverge_hsv(201)   # from the colorspace library
  colormap(obj, MAmap, legend.name = "mercury", col.seq = newpal, 
	legend.add.line=round(median(obj$fit),2), anchor = TRUE)
  }
}

\keyword{hplot}
\keyword{misc}
\keyword{smooth}
