\name{make.covar}
\alias{make.covar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate covariance matrices for a set of lag distance bins
}
\description{
	calculates variance covariance matrices for each lag distance bin and for a flat (non-spatial) bin
}
\usage{
make.covar(datafile,dist.mat,binsize,binname)
}
\arguments{
  \item{datafile}{
a dataset where the first two columns are x and y coordinates. Further columns contain observed variables for subsequent sesem modeling. 
}
  \item{dist.mat}{
a vector containing distances between all X-Y sample pairs in a dataset. Produced by function calc.dist.
}
  \item{binsize}{
A vector of bin size cutoff distances starting with zero. Produced using make.bin
}
  \item{binname}{
A character vector of bin names. This vector has one element less than binsize.
}
}
\details{
Calculates variance-covariance matrices for a series of lag distance bins and for a flat (non-spatial) bin. This function produces a list object with four components: [[1]]bin.summary, [[2]] variable names [[3]] flat covariance matrix, [[4]][,,i] covariance matrices for each bin i. A summary of the bins is printed.
}
\value{

\item{1}{A bin summary with colums binname, lower cutoff distance, upper cutoff distance, and sample size}
\item{2}{A list of observed variable names}
\item{3}{the flat (non-spatial) variance-covariance matrix}
\item{4}{contains elements [[4]][,,i] where each i is the variance - covariance matrix for a particular lag distance bin}

}
\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.
}
\author{
Kerrie Mengersen, Eric Lamb
}

\seealso{

\code{\link{make.bin}}, \code{\link{make.covar}}, \code{\link{runModels}}
}
\examples{
data=truelove
truelove_red<-truelove[c(1:60),c(1:5)]
distancematrix<-calc.dist(truelove_red)

#calc.dist and make.covar can be time consuming to run, 
# therefore only a small dataset utilized here

Truelove_bins<-make.bin(distancematrix,type="ALL",p.dist=5)
binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes
binname<-Truelove_bins[2][[1]] #truelove lowland bin names

covariances<-make.covar(truelove_red,distancematrix,binsize,binname)
covariances
}
\keyword{ spatial }
