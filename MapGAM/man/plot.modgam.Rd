\name{plot.modgam}
\alias{plot.modgam}
\title{
Maps Predicted Values and Clusters for \code{\link{modgam}} Objects
}
\description{
Displays a color image map, including a legend, scale bar, and optional North arrow, showing crude or adjusted odds ratios (or linear predictors) for a grid of points.  Irregular grids are allowed.  Also draws contour lines for regions of signficantly increased or decreased values of the outcome variable ("clusters"), if permutation ranks are provided.  Designed to display \code{modgam} objects but can be used with other model results if the modgamobj list contains the correct elements.      
}
\usage{
\method{plot}{modgam}(x, map = NULL, exp = FALSE, add = FALSE, intervals=TRUE, 
       mapmin = NULL, mapmax = NULL, col.seq = diverge_hsv(201), anchor=FALSE, 
       border.gray = 0.3, contours=c("none","response","permrank","interval"),
       contours.drawlabels=FALSE, contours.lty=1, contours.lwd=1, 
       contours.levels, contours.labcex=0.7, arrow=TRUE, axes=FALSE, ptsize=0.9,
       alpha=0.05, mai, legend.name = "predicted values", legend.cex=1, \dots)
}
\arguments{
  \item{x}{
(required) an object of class \code{modgam}, usually returned by the \code{modgam} function.  
}
  \item{map}{
can be used to map predicted values on a base map from the \code{map} function in the \pkg{maps} package, or on a base map produced from an external shapefile using the \code{st_read} function in \pkg{sf} package. Note that MapGAM used to include examples using the \code{readShapePoly} function in the \pkg{maptools} package, but that package was removed from the CRAN repository in late 2023. \code{map=NULL} produces a color image without any base map.
}
  \item{exp}{
if \code{exp=T}, \code{fit} will be displayed in exponential scale.  
}
  \item{add}{
use \code{add=T} to add the color map to an existing plot.  This will often result in loss of the legend and scale, which are added outside of the normal
    map boundaries.  \code{add} is ignored when a map is provided using the 
    \code{map} argument.
}
  \item{intervals}{
if the \code{modgam} object contains confidence intervals, the lower and higher intervals will be plotted if \code{intervals=T}.  
}
  \item{mapmin}{
the minimum value for the color scale legend. 
}
  \item{mapmax}{
the maximum value for the color scale legend. 
}
  \item{col.seq}{
The color sequence (palette) used to display the predicted values on the map.
}
  \item{anchor}{
Use \code{anchor=TRUE} to center the color palette on \code{legend.add.line}.  When \code{anchor=FALSE} or when \code{legend.add.line} is not specified, the color palette will be centered halfway between mapmin and mapmax.  \code{anchor=TRUE} is recommended when using a divergent palette.    
}
  \item{border.gray}{
gray scale for the border of \code{map}, ranges from 0 to 1. It is white with \code{border.gray=1}, and black with \code{border.gray=0}. The defalt scale is 0.3.
}
  \item{contours}{
use \code{contours="response"} to add contour lines for the predicted response, for example to draw isoboles for mixtures of exposures.  Use \code{contours="permrank"} to add contour lines for pointwise p-values computed from the permutation ranks, at \code{alpha/2} and \code{(1-alpha)/2}. use \code{contours = "intervals"} to add contour lines for areas where the confidence intervals do not include 0. The default is "none" which produces no contour lines.
}
  \item{contours.drawlabels}{
use \code{contours.drawlabels = TRUE} add labels on contour lines.   
}
  \item{contours.lty}{
the line type for contour lines.
}
  \item{contours.lwd}{
the width for contour lines.
} 
  \item{contours.levels}{
the levels for contour lines. When \code{exp=T}, the levels will also be used in an exponential scale.
} 
  \item{contours.labcex}{
cex for contour labelling. This is an absolute size, not a multiple of \code{par("cex")} 
}
  \item{arrow}{
use \code{arrow=T} to add a North arrow to the map.  	
}
  \item{axes}{
use \code{axes=T} to add axes to the map (useful for chemical mixture isoboles).
}
  \item{ptsize}{
the size of the points used to fill in colors on the map.  Increase to remove white space inside the map or decrease to remove color outside the map boundaries.  NOTE: white space can also be eliminated by increasing the grid size in \code{predgrid}, which is often preferable as it results in a higher resolution map.   
}
  \item{alpha}{
the nominal pointwise type I error rate; only used when \code{contours="permrank"}.  
}
  \item{mai}{
the margins of the plot. Details see \link{par}.}
  \item{legend.name}{
the name of displayed for the legend bar.}
  \item{legend.cex}{
a numerical value giving the amount by which legend text should be magnified relative to the default.  
}
  \item{...}{
other arguments to be passed to \link{colormap} function.
  }
}

\value{
Produces an image map showing crude or adjusted linear predictors (or odds ratios).  If the base map is in \code{readShapePoly} format a scale bar is included.  The scale bar assumes that the X and Y coordinates are provided in meters.      
}

\author{
Scott Bartell, Lu Bai, Robin Bliss, and Veronica Vieira

Send bug reports to \email{sbartell@uci.edu}.
}

\section{Warning}{Note that the contour lines use a pointwise nominal type I error rate of alpha; the chance of a type I error occurring for \strong{at least} one point on the map is much higher, typically approaching 100\% at alpha=0.05, because a spatial prediction grid generally contains many points.} 

\seealso{
  \code{\link{trimdata}}, 
  \code{\link{predgrid}}, 
  \code{\link{optspan}}, 
  \code{\link{modgam}},
  \code{\link{colormap}}.
}

\examples{
\donttest{
data(MAmap)		
data(MAdata) 
# Create a grid on the base map 
MAgrid <- predgrid(MAdata, map=MAmap)
# fit crude GAM model to the MA data using span size of 50\%
fit1 <- modgam(data=MAdata, rgrid=MAgrid, m="crude", sp=0.5)  
# Plot a map showing crude odds ratios
plot(fit1, MAmap)					

#### A detailed example including map projections and data trimming
# NOTE: this example requires the maps and mapproj packages
# Convert base map and beer tweet data locations to US Albers projection 
# projected coords yield better distance estimates than (lat,long)
if(require(maps) & require(mapproj)) {
  USmap <- map("state",projection="albers",parameters=c(29.5,45.5),
               plot=FALSE,fill=TRUE,col="transparent")
  data(beertweets)
  case <- beertweets$beer
  # Reuse last map projection to convert data coordinates	
  XY <- mapproject(beertweets$longitude,beertweets$latitude)[1:2]  
  beerproj <- data.frame(case, XY[1], XY[2])			 
  # Generate grid on the US map, trimmed to range of beer data
  USgrid <- predgrid(beerproj, map=USmap)						
  # Fit unadjusted model--geolocation only
  fit2 <- modgam(data=beerproj, rgrid=USgrid, m="unadjusted", sp=0.05)	
  dev.new(width=7,height=5)
  plot(fit2, USmap)
  title(main="Beer Tweet Odds Ratios")	
}
}
}

\keyword{hplot}
\keyword{misc}
\keyword{smooth}
