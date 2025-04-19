\docType{data}
\name{truelove_covar}
\alias{truelove_covar}
\title{Truelove lowland example covariances}
\format{A list object with covariances for 11 lag distance bins.}

\description{
This list object is an example of the output produced by function \code{make.covar}. It was produced by the following script, and is provided to reduce the computation load of some of the examples in this library.

\code{data=truelove}

\code{distancematrix<-calc.dist(truelove)}

\code{Truelove_bins<-make.bin(distancematrix,type="ALL",p.dist=20)}

\code{binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes}

\code{binname<-Truelove_bins[2][[1]] #truelove lowland bin names}

\code{covariances<-make.covar(truelove,distancematrix,binsize,binname)}

}
\seealso{
\code{\link{calc.dist}}, \code{\link{make.bin}}, \code{\link{make.covar}}
}
\keyword{datasets}