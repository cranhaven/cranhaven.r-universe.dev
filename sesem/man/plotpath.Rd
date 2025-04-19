\name{plotpath}
\alias{plotpath}
\title{
Function to plot spatial SEM results for individual paths
}
\description{
A function to plot unstandardized path coefficients across lag distances. The default is to plot all directed paths with standard error bars, and greyed bars to indicate nonsignificant paths (z>0.05). A range of options are available to change the paths plotted, to add trendlines, and to alter the look of the plots.   
}
\usage{
plotpath(spatial_model_results, path.type = "directed", selectpath = "none selected",
	add.line = "none", add.error = T, pcut = 0.05, pch = 16, 
	lwd = 2, lty = 1,cex.main=1.2)
}
\arguments{
  \item{spatial_model_results}{
a list object produced by function runModels
}
  \item{path.type}{
An option to select the paths to be plotted. "directed" = only directed paths plotted; "undirected" = only undirected correlations plotted; "both" = all paths plotted; "user" = allows user to specify particular paths and a particular order for plotting. Argument selectpath must also be provided with path.type="user"
}
  \item{selectpath}{
An option to select specific paths for plotting. Usage is as follows: selectpath==c(5,18,16,23,29) where values refer to path numbers. Path numbers can be obtained using spatial_model_results[[2]]
}
  \item{add.line}{
Options for plotting a fit line. "none" indicates no line; "step" plots straight line segments between points; "smooth" plots a smoothed curve fit using function lowess
}
  \item{add.error}{
Should standard error bars be added for each path coefficient
}
  \item{pcut}{
p-value cutoff above which points with non significant p-values are shaded grey. Set pcut=1 to have all points black.
}
  \item{pch}{
Selects plotting symbols
}
  \item{lwd}{
Sets trendline width
}
  \item{lty}{
Sets trendline format
}
  \item{cex.main}{
Sets plot title font
}
}

\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.

Rosseel, Y. 2012 lavaan: an R package for structural equation modeling. \emph{Journal of Statistical Software} \bold{48}:1-36.
}
\author{
Eric Lamb
}

\seealso{
\code{\link{sem}}, \code{\link{make.covar}}, \code{\link{runModels}}, \code{\link{modelsummary}}, \code{\link{plotmodelfit}}, \code{\link{gam.path}}
}
\examples{
#data=truelove
#distancematrix<-calc.dist(truelove)
#Truelove_bins<-make.bin(distancematrix,type="ALL",p.dist=20)
#binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes
#binname<-Truelove_bins[2][[1]] #truelove lowland bin names

#covariances<-make.covar(truelove,distancematrix,binsize,binname)
#covariances

# path model for the truelove dataset
#spatial_model<-'
#	Gram ~ Moisture
#	N_Fix ~ Bryoph + Lich + SoilCrust
#	SoilCrust ~ Bryoph + Lich + Gram + Shrubs + Forbs	
#	Bryoph ~ Gram + Shrubs + Forbs + Moisture
#	Lich ~ Moisture + Forbs + Gram + Shrubs + Bryoph
#	Forbs ~ Moisture
#	Gram ~~ Forbs
#	Shrubs ~ Moisture	
#	Gram ~~ Shrubs
#	Shrubs ~~ Forbs
#	'
#
#results<-runModels(spatial_model,covariances)

#The above script produces the sesem object stored as truelove_results

data=truelove_results

plotpath(truelove_results)
truelove_results[[2]]# list of path names
plotpath(truelove_results,path.type="user",selectpath=c(5,7,8))


}

\keyword{spatial}

