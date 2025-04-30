\name{GSAfisher.summary}
\alias{GSAfisher.summary}
\title{GSAfisher.summary}
\description{
Summary method of GSAfisher function that computes a numerical summary of the marginal p-values.
}
\usage{
\method{GSAfisher}{summary}(x,\dots)
}
\arguments{
  \item{x}{Numeric set of marginal p-values (one vector or a sinlge p-value).}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{summary.p}{Summary statistics (min, 1st quantile, median, 3rd quantile, max) of a numerical set of marginal p-values.}
}
\author{Carlos Garcia Prieto}
\examples{
#Generate 1000 random values from an Uniform distribution U (0, 0.25). 
x<-runif(1000, 0, 0.25)
#Set data class to "summary"
class(x)<-"summary"
#Apply the GSAfisher.summary function to this data vector.
GSAfisher(x)
}
\keyword{ summary }