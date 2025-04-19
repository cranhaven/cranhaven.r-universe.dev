\name{make.bin}
\alias{make.bin}

\title{
Function to make lag distance bins
}
\description{
For spatial SEM, lag distance bins need to be described by 2 vectors: binsize and 
binname. make.bin is a function to automatically generate lag distance bins for a 
given dataset. Options to automatically specify bins based on desired within bin sample 
size, or desired number of bins are available.
}
\usage{
make.bin(dist.mat, type = "n.bins", p.dist = 50, n.bins = 10, s.size = 100)
}
\arguments{
  \item{dist.mat}{
a vector containing distances between all X-Y sample pairs in a dataset. 
Produced by function calc.dist
}
  \item{type}{
should a particular number of bins be created ("n.bins"), or should bins each contain 
a particular number of sample pairs ("s.size"), or should all possible bins be created 
("ALL"). Note that "ALL" will only yield sensible results for regular sampling designs.
}
  \item{p.dist}{
Inference distance (i.e. upper limit of largest lag distance bin) as a percentage 
of the maximum distance sampled 
}
  \item{n.bins}{
number of bins to be generated if type="n.bins"
}
  \item{s.size}{
sample size within each bin to be used if type="s.size"
}
}
\details{
make.bin generates cut off values for lag distance bins and corresponding bin names	The function has three default parameter values available, if user does not want to specify:
	
(1) Inference distance as a percentage(p.dist) = 50. This sets the upper limit of the largest bin. Normally lag distances greater than 50% of the maximum sampling extend are not recommended. (2) Number of bins (n.bins) = 10 (3) Sample size (s.size) = 100

You can use type="ALL","n.bins" OR "s.size" to control parameter values. ALL will only produce sensible results for regular sampling designs. The function produces a list object containing (1.)binsize and (2.)binname. These two vectors (binsize and binname) will be used by make.covar to calculate variance covariance matrices for each lag distance bin
	
Special note: User specified number of lag distance bins OR sample size will be used to calculate initial cutoff value of each lag distance bin. However, if the cutoff point is in between a lag distance bin, real cutoff will apply at the upper margin of the particular bin. Therefore, resulting number of bins are less than or equal AND resulting sample sizes are greater than or equal to the value specified by the user.
}
\value{
a List with two components: 
  \item{comp1 }{a vector of bin size cutoff distances starting with zero}
  \item{comp2 }{a character vector of bin names. This vector has one element 
  less than component 1}
}
\references{
Lamb, E. G., K. Mengersen, K. J. Stewart, U. Attanayake, and S. D. Siciliano. 2014. Spatially explicit structural equation modeling. \emph{Ecology} \bold{95}:2434-2442.
}
\author{
Udayanga Attanayake, Eric Lamb
}


\seealso{

\code{\link{calc.dist}}, \code{\link{make.covar}}, \code{\link{plotbin}}
}
\examples{

data=truelove
distdata<-truelove[c(1:60),]
distances<-calc.dist(distdata)
Truelove_bins<-make.bin(distances,type="ALL") 
#	inference distance=50% AND number of bins=ALL
#	note the need to extract sizes and names from the bin size object 
#	as shown below for use in function make.covar
binsize<-Truelove_bins[1][[1]] #truelove lowland bin sizes
binsize
binname<-Truelove_bins[2][[1]] #truelove lowland bin names
binname

# function make.bin to generate bins with 120 samples each to a maximum inference 
# distance of 20% note the need to extract sizes and names from the bin size object 
# as shown below for use in make.covar

data=plantcomp
distdata<-plantcomp[c(1:60),]
distances<-calc.dist(distdata)
#using subset of plantcomp dataset to cut computation time
Plant_bins<-make.bin(distances,type="s.size",s.size=60,p.dist=20) 
#inference distance=20% AND sample size=120
#	note the need to extract sizes and names from the bin size object 
#	as shown below for use in function make.covar
binsize<-Plant_bins[1][[1]] #plant competition bin sizes
binsize
binname<-Plant_bins[2][[1]] #plant competition bin names
binname

# if manual bin sizes are needed use the following approach. 
# Note that the bin name vector must have one fewer elements than bin size
	
binsize = c(0,1,2,2.2,4,5,8,16,32,64,96,128,160)
binsize
binname=c("Bin1","Bin2","Bin3","Bin4","Bin5","Bin6","Bin7","Bin8",
	"Bin9","Bin10","Bin11","Bin12")
binname
}


\keyword{spatial}

