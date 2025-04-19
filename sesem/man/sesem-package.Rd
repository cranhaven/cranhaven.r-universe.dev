\name{sesem-package}
\alias{sesem-package}
\alias{sesem}
\docType{package}
\title{
Spatial structural equation modeling (SESEM)
}
\description{
Structural equation modeling (SEM) is a powerful statistical approach for the testing of networks of direct and indirect theoretical causal relationships in complex datasets with intercorrelated dependent and independent variables. Here we implement a simple method for spatially explicit SEM (SE-SEM) based on the analysis of variance covariance matrices calculated across a range of lag distances. This method provides readily interpretable plots of the change in path coefficients across scale.
}
\details{
\tabular{ll}{
Package: \tab sesem\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-06-09\cr
License: \tab GPL (>= 2)\cr
}
Package sesem allows spatially explicit structural equation modeling. It allows a structural equation model to be fit to a number of spatially explicit covariance matrices to explore how the strength of structural path coefficients changes with scale. In brief an SESEM analysis involves:
    calculating pairwise differences among samples
	selecting a series of lag distance bins
	generating spatially explicit variance - covariance matrices for each lag distance bin
	fitting a structural equation model to each of those bins
	comparing and examining how the strength of path coefficients change with scale
}
\author{
Eric G. Lamb, Kerrie Mengersen, Katherine J. Stewart, Udayanga Attanayake, and Steven D. Siciliano

Maintainer: Eric Lamb <eric.lamb@usask.ca>
}
\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.

Rosseel, Y. 2012 lavaan: an R package for structural equation modeling. \emph{Journal of Statistical Software} \bold{48}:1-36.

}

\keyword{ package }
\seealso{
\code{\link{sem}}, \code{\link{lavaan}}
}
\examples{
data=truelove
truelove_red<-truelove[c(1:60),c(1:7)]
distancematrix<-calc.dist(truelove_red)
Truelove_bins<-make.bin(distancematrix,type="ALL",p.dist=10)
binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes
binname<-Truelove_bins[2][[1]] #truelove lowland bin names

plotbin(distancematrix,binsize)

covariances<-make.covar(truelove_red,distancematrix,binsize,binname)
covariances

# reduced path model for the truelove dataset

spatial_model<-'
	N_Fix ~ Bryoph + Lich + SoilCrust
	SoilCrust ~ Bryoph + Lich	
	Lich ~ Bryoph + Moisture
	Bryoph ~ Moisture
	'

results<-runModels(spatial_model,covariances)
modelsummary(results)
plotmodelfit(results)
plotpath(results)

}
