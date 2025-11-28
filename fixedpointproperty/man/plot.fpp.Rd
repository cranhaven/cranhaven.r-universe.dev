\name{plot.fpp}
\alias{plot.fpp}
\alias{fpPlot}
\alias{plot}
\title{
Function to plot the distributions and differences of multiple binary mixture distributions.
}
\description{
This function plots kernel-based densities as well as density differences for three or more data sets (fpp objects). 
}
\usage{
\method{plot}{fpp}(x, ylab = c("Density", "Density difference"), xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an fpp object
}
  \item{ylab}{y-axis labels of the two plots
}
  \item{xlim}{the x limits (x1, x2) of the two plots
}
  \item{\dots}{additional arguments to pass to both \code{plot.density} and \code{matplot}
}
}
\value{No return value, called for side effects}
\details{Generates two plots, one showing the estimated densities (based on \code{plot.density}) and
one showing the density differences (based on \code{matplot})}

\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen (l.vanmaanen@uu.nl)
}

\seealso{
\code{\link{fpGet}},\code{\link{fpDensDiff}},\code{\link{plot.density}},\code{\link{matplot}}
}
\examples{
## generate data
p <- c(.1,.5,.9)
rt <- sapply(1:3, function(i) {rnormMix(1000, c(1,2), c(1,1), p[i])})
dat <- data.frame(rt=c(rt), cond=rep(1:3, each=1000))

## compute fpp object
fpobject <- fpGet(dat, 1000, bw=.75)

## plot it
op <- par(mfrow=c(1,2))
plot(fpobject)
par(op)

}
