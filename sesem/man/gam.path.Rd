\name{gam.path}
\alias{gam.path}

\title{
Prints and displays spatial sem results using gam models
}
\description{
This function fits generalized additive models (gam) of the path coefficient vs. lag 
distance relationship for each path in the spatial SEM model. Gam functions and figures 
are produced. Requires function mgcv
}
\usage{
gam.path(spatial_model_results, path.type = "directed",selectpath = "none selected", 
	plot.points = T, se.plot = T, lwd.pred = 2, lty.pred = 1, lwd.se = 2, lty.se = 3, 
	cex = 1, cex.axis = 1, cex.lab = 1, xlab = "Lag Distance", 
	ylab = "Unst. Path Coeff.", yaxt = "s", xaxt = "s")
}

\arguments{
  \item{spatial_model_results}{
a list object produced by function runModels
}
  \item{path.type}{
An option to select the paths to be plotted. "directed" = only directed paths plotted; 
"undirected" = only undirected correlations plotted; "both" = all paths plotted; 
"user" = allows user to specify particular paths and a particular order for plotting. 
Argument selectpath must also be provided with path.type="user"
}
  \item{selectpath}{
An option to select specific paths for plotting. Usage is as follows: 
selectpath==c(5,18,16,23,29) where values refer to path numbers. Path numbers 
can be obtained using spatial_model_results[[2]]
}
  \item{plot.points}{
Should points for individual models be plotted?
}
  \item{se.plot}{
Should standard error lines for each gam model be plotted?
}
  \item{lwd.pred}{
width of the predicted line from the gam model
}
  \item{lty.pred}{
format of the predicted line from the gam model
}
  \item{lwd.se}{
width of the standard error line
}
  \item{lty.se}{
format of the standard error line
}
  \item{cex}{
point size
}
  \item{cex.axis}{
axis font size
}
  \item{cex.lab}{
label font size
}
  \item{xlab}{
x-axis label
}
  \item{ylab}{
y-axis label
}
  \item{yaxt}{
argument to suppress plotting of y-axis if set to "n"
}
  \item{xaxt}{
argument to suppress plotting of x-axis if set to "n"
}

}
\details{
Generalized additive models (gam) allow flexible modeling of nonlinear relationships 
with minimal assumptions about the shape of those relationships. This function 
utilizes the gam fitting function in library mgcv to fit and display gam models of the 
relationships between lag distance and unstandardized path coefficients. This is an 
alternative to the lowess smoothed lines available in function plot.path. Potential 
advantages of the gam models include the ability to obtain predictions for lag distance 
values intermediate between modeled lag distances.
}

\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.

Rosseel, Y. 2012 lavaan: an R package for structural equation modeling. \emph{Journal of Statistical Software} \bold{48}:1-36

Wood, S.N. 2011 Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. \emph{Journal of the Royal Statistical Society (B)} \bold{73(1)}:3-36

Wood, S.N. 2006 Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC
}
\author{
Eric Lamb
}

\seealso{
\code{\link{sem}}, \code{\link{gam}}, \code{\link{make.covar}}, \code{\link{runModels}}, 
\code{\link{modelsummary}}, \code{\link{plotmodelfit}}, \code{\link{plotpath}}
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

gam.path(truelove_results)
truelove_results[[2]]# list of path names
gam.path(truelove_results,path.type="user",selectpath=c(5,7,8))
}

\keyword{ spatial}
