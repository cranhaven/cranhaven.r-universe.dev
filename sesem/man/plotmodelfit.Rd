\name{plotmodelfit}
\alias{plotmodelfit}

\title{
Function to plot model fit indices for spatial SEM analyses
}
\description{
A function to plot model fit indices across lag distances. The default is to plot all of the chi square, cfi, rmsea (including confidence intervals), and srmr indices. Horizontal lines indicating significant cutoffs for each index are plotted (chi square p=0.05, cfi=0.9, rmsea=0.05, and srmr=0.08). Options to add trend lines are available.
}
\usage{
plotmodelfit(spatial_model_results, plots = "all", add.line = "none", rmsea_err = T, 
     pch = 16, lwd = 2, lty = 1, cex = 1, cex.lab = 1, cex.axis = 1, cex.main = 1.5)
}
\arguments{
  \item{spatial_model_results}{
a list object produced by function runModels
}
  \item{plots}{
Indicates which indices should be plotted. The default "all" produces plots of all of the chi square, cfi, rmsea (including confidence intervals), and srmr indices. plot="chi", "cfi", "rmsea", or "srmr" will produce only a single plot.
}
  \item{add.line}{
Indicates whether a trendline should be added connecting the points. The default "none" indicates no line, "step" plots straight line segments between points, and "smooth" plots a smoothed curve fit using function lowess. Smoothed lines do not include the flat model (lag distance zero).
}
  \item{rmsea_err}{
Should the rmsea index be plotted with confidence intervals? rmsea_err=T is the default, rmsea_err=F will suppress confidence intervals. Note that warnings will likely arise if rmsea_err=T is used and there are confidence intervals of zero. All these warnings indicate is that that particular confidence interval is equal to the estimated value and will not be plotted.
}
  \item{pch}{
plotting symbol
}
  \item{lwd}{
line width
}
  \item{lty}{
line format
}
  \item{cex}{
symbol size
}
  \item{cex.lab}{
label font size
}
  \item{cex.axis}{
axis label font size
}
  \item{cex.main}{
plot title font size
}
}
\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.
}
\author{
Eric Lamb
}
\note{
Warnings may arise from the plotting of the rsmea error bars if rmsea_err=T is used and there are confidence intervals of zero. All these warnings indicate is that that particular confidence interval is equal to the estimated value and will not be plotted.
}

\seealso{
\code{\link{sem}}, \code{\link{make.covar}}, \code{\link{runModels}}, \code{\link{modelsummary}}, \code{\link{avg.modindices}}, \code{\link{plotpath}}, \code{\link{gam.path}}
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

plotmodelfit(truelove_results) #note that the warnings that arise here can be ignored
plotmodelfit(truelove_results,rmsea_err=FALSE)
plotmodelfit(truelove_results,plots="chi")


}

\keyword{spatial}