\name{plotbin}
\alias{plotbin}
\title{
Function to plot the distribution of lag distance bin sizes
}
\description{
A convenience function that provides a histogram of distances among of all sample pairs and a plot of sample sizes for each the selected bin size.
}
\usage{
plotbin(dist.mat,binsize)
}
\arguments{
  \item{dist.mat}{
a vector of x-y distances among all sample pairs as produced by calc.dist
}
  \item{binsize}{
A vector of bin sizes as produced by make.bin, or of the form c(0,2,3,4,....) where the values are the desired bin cutoffs beginning with zero.
}
}
\details{
This function is to assist in visualization of both the distribution of x-y distances in the dataset and sample sizes resulting from the selected lag distance bins. See the help files for calc.dist and make.bin for the inputs to this function.
}

\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.
}
\author{
Eric Lamb
}


\seealso{
\code{\link{make.bin}}, \code{\link{calc.dist}}, \code{\link{make.covar}}
}
\examples{
data=plantcomp
distancematrix<-calc.dist(plantcomp[c(1:100),])
#using subset of plantcomp dataset to cut computation time
Plant_bins<-make.bin(distancematrix,type="s.size",s.size=200) 
#inference distance=50% AND sample size=200
binsize<-Plant_bins[1][[1]] #plant competition bin sizes

plotbin(distancematrix,binsize)

}

\keyword{ ~spatial }
