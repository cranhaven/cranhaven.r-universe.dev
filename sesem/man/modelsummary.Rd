\name{modelsummary}
\alias{modelsummary}
\title{
Function to extract and display basic summary information for a spatial SEM analysis
}
\description{
extracts basic model summary information from the bin.summary file and the object created by run.Models in a readable format
}
\usage{
modelsummary(spatial_model_results)
}
\arguments{
  \item{spatial_model_results}{
a list object produced by runModels
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
\code{\link{sem}}, \code{\link{make.covar}}, \code{\link{runModels}}, \code{\link{plotmodelfit}}, \code{\link{plotpath}}
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

modelsummary(truelove_results)
}

\keyword{spatial}
