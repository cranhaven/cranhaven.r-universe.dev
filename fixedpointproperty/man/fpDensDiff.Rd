\name{fpDensDiff}
\alias{fpDensDiff}
\title{
Function to compute the crossing point of two kernel-based distribution functions
}
\description{
This function takes a list of \code{fpp} objects, and computes at which point the distributions cross each other. 
}
\usage{
fpDensDiff(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a list of \code{fpp} objects
}
}
\value{
An m * n-array containing the crossing points of the pairs of distributions, with n the length of the list of \code{fpp} objects, and m the number of pairs of distributions.
}
\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen (l.vanmaanen@uu.nl)
}

\seealso{
\code{\link{fpGet}},\code{\link{fpAnova}}
}

\examples{
## generate data
p <- c(.1,.5,.9)
rt <- sapply(1:3, function(i) {rnormMix(10000, c(1,2), c(1,1), p[i])})
dat <- data.frame(rt=c(rt), cond=rep(1:3, each=10000), pp=rep(1:50, each=200, times=3))

## compute the list of fpp objects
res <- tapply(1:nrow(dat), dat$pp, function(X) {fpGet(dat[X,], 1000, bw=.75)})

## get the crossing points
crosses=fpDensDiff(res)
boxplot(t(crosses), frame.plot=FALSE,xlab="Crossing point", ylab="Condition pair", 
  names=c("1-2","2-3","1-3"), horizontal=TRUE)
}
