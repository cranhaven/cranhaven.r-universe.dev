\name{avg.modindices}
\alias{avg.modindices}
\title{
Function to display averaged modification indices for a spatial SEM
}
\description{
Modification indices averaged across all lag distance bins (the non-spatial bin is not included) are calculated for each potential path addition to the structural model. Option modcut suppresses the printing of modification indices of less than that value.
}
\usage{
avg.modindices(spatial_model_results, modcut = 4)
}

\arguments{
  \item{spatial_model_results}{
a list object produced by function runModels
}
  \item{modcut}{
The minimum averaged modification index to print. Default is 4. 
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
\code{\link{sem}}, \code{\link{modelsummary}}, \code{\link{runModels}}, \code{\link{plotmodelfit}}
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
avg.modindices(truelove_results)
avg.modindices(truelove_results,modcut=10)
}

\keyword{ spatial }