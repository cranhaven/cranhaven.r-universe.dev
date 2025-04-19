\docType{data}
\name{truelove_results}
\alias{truelove_results}
\title{Truelove lowland example sesem output}
\format{A list object sesem results for the truelove path model and 11 lag distance bins.}

\description{
This list object is an example of the output produced by function runModels. It was produced by the following script, and is provided to reduce the computation load of some of the examples in this library.

\code{data=truelove}

\code{distancematrix<-calc.dist(truelove)}

\code{Truelove_bins<-make.bin(distancematrix,type="ALL",p.dist=20)}

\code{binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes}

\code{binname<-Truelove_bins[2][[1]] #truelove lowland bin names}

\code{covariances<-make.covar(truelove,distancematrix,binsize,binname)}

path model for the truelove dataset:

	spatial_model<-'

	Gram ~ Moisture

	N_Fix ~ Bryoph + Lich + SoilCrust

	SoilCrust ~ Bryoph + Lich + Gram + Shrubs + Forbs

	Bryoph ~ Gram + Shrubs + Forbs + Moisture

	Lich ~ Moisture + Forbs + Gram + Shrubs + Bryoph

	Forbs ~ Moisture

	Gram ~~ Forbs

	Shrubs ~ Moisture	

	Gram ~~ Shrubs

	Shrubs ~~ Forbs'

\code{results<-runModels(spatial_model,covariances)}

}
\seealso{
\code{\link{calc.dist}}, \code{\link{make.bin}}, \code{\link{make.covar}}, \code{\link{runModels}}
}
\keyword{datasets}