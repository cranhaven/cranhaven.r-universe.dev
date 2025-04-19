\name{calc.dist}
\alias{calc.dist}
\title{
Calculate intersample distances for a set of X-Y coordinates
}
\description{
calc.dist calculates and stores the distance matrix for all X-Y pairs. It works on a dataset where the first two columns are x and y coordinates
}
\usage{
calc.dist(datafile)
}

\arguments{
  \item{datafile}{
a dataset where the first two columns are x and y coordinates. Further columns can contain observed variables for subsequent modeling; these are ignored here. 
}
}
\details{
{datafile} is a dataset where the first two columns are x and y coordinates. If data are distributed on only one dimension (i.e. along a linear transect) one of the columns should contain zeroes.
}
\value{
a vector containing distances between each pair of samples in the input data
}
\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.
}
\author{
Kerrie Mengersen, Eric Lamb
}
\seealso{
\code{\link{make.covar}}
}
\examples{
data=truelove
distdata<-truelove[c(1:60),]
distances<-calc.dist(distdata)
distances
}
\keyword{ ~spatial }

