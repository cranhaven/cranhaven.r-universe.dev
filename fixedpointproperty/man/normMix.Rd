\name{normMix}
\alias{dnormMix}
\alias{normMix}
\alias{pnormMix}
\alias{qnormMix}
\alias{rnormMix}
\title{
Gaussian binary mixture distribution
}
\description{
Density, distribution, function, quantile function, and random generation for the mixture of two Gaussian distributions with mixture proportion \code{p} and \code{1-p}.
}
\usage{
dnormMix(x, mean=c(0,1), sd=c(1,1), p=1)
pnormMix(x, mean=c(0,1), sd=c(1,1), p=1)
qnormMix(x, mean=c(0,1), sd=c(1,1), p=1)
rnormMix(n, mean=c(0,1), sd=c(1,1), p=1)
}

\arguments{
  \item{x}{vector of quantiles or probabilities.
}
\item{n}{number of observations.
}
  \item{mean}{vector of two means.
}
  \item{sd}{vector of standard deviations.
}
  \item{p}{mixture proportion of the first distribution (the second has proportion \cite{1-p}).
  }
}

\value{
\code{dnormMix} gives the density, \code{pnormMix} gives the distribution function, \code{qnormMix} gives the quantile function, and \code{rnormMix} generates random deviates.}
\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen  (l.vanmaanen@uu.nl)}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## the default value for p reduces this function to a normal distribution
dnormMix(1) == dnorm(1)

## plot that illustrates the fixed-point property
p <- c(.8,.5,.2)
m <- c(0,1); s <- c(1,2)
plot(function(X) {dnormMix(X, m, s, p[1])}, -5,10, ylab="Density",xlab="x", bty='L')
for (i in 2:3) {
  plot(function(X) {dnormMix(X, m, s, p[i])},-5,10, add=TRUE, lty=i, lwd=2)
}
legend("topright", legend=paste("p=",p,sep=''), lty=1:3, lwd=2, bty='n')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }