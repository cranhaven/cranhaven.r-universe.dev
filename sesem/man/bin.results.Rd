\name{bin.results}
\alias{bin.results}
\title{
Extract results for a particular bin}
\description{
Function to extract path coefficients, standard errors, and standardized coefficients for a particular bin in a readable format
}
\usage{
bin.results(spatial_model_results, bin = "binflat")
}
\arguments{
  \item{spatial_model_results}{
a list object produced by function run.Models
}
  \item{bin}{
Name of the bin that results are desired for. Defaults to flat model.
}
}
\details{
Given a spatial SEM object produced by runModels, bin.results allows the results for a particular lag distance bin to be inspected in a readable format.
}

\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.

}
\author{
Eric Lamb
}

\seealso{
\code{\link{sem}}, \code{\link{make.covar}}, \code{\link{runModels}}, \code{\link{modelsummary}}
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

bin.results(truelove_results)
bin.results(truelove_results,bin="Bin2")
}
\keyword{spatial}