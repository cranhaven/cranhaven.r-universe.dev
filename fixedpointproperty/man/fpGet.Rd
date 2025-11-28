\name{fpGet}
\alias{fpGet}
\title{
Function to compute densities and density differences for three or more data sets
}
\description{
This function computes standard kernel-based density functions for a response time data set with three or more conditions. In addition, it computes the pairwise differences for each pair of density functions.
}
\usage{
fpGet(dat, n = 512, bw = "nrd0")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{n*2 dataframe or matrix with in col 1: RT (the values for which to compute the density); col 2: condition (an index)
}
  \item{n}{the number of equally spaced points at which the density is to be estimated. See \code{\link{density}} for details.
}
  \item{bw}{the smoothing bandwidth to be used. See \code{\link{density}} for details.
}
}
\value{
an object of class \code{fpp}, with the following components.

\item{dens }{list of objects from class \code{density}}
\item{diff }{dataframe of the density differences}
\item{dat }{dataframe with the input data}

}
\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen  (l.vanmaanen@uu.nl)}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{density}}.
}
\examples{
### one data set or participant
## generate data
p <- c(.1,.5,.9)
rt <- sapply(1:3, function(i) {rnormMix(1000, c(1,2), c(1,1), p[i])})
dat <- data.frame(rt=c(rt), cond=rep(1:3, each=1000))

## compute one fp object
fpobject <- fpGet(dat, 1000, bw=.75)


### multiple participants
## generate data
p <- c(.1,.5,.9)
rt <- sapply(1:3, function(i) {rnormMix(10000, c(1,2), c(1,1), p[i])})
dat <- data.frame(rt=c(rt), cond=rep(1:3, each=10000), pp=rep(1:50, each=200, times=3))

## compute the list of fpp objects
res <- tapply(1:nrow(dat), dat$pp, function(X) {fpGet(dat[X,], 1000, bw=.75)})


}