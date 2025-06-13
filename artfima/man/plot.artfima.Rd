\name{plot.artfima}
\alias{plot.artfima}

\title{Plot Method for "arfima" Object}

\description{
Plots the observed periodogram and the fitted spectral density function.
}

\usage{\method{plot}{artfima}(x, which = c("all", "logsdf", "loglogsdf", "res"),
                 mainQ = TRUE, subQ = TRUE, lag.max = 30, ...) }

\arguments{
  \item{x}{ object of class "artfima" }
  \item{which}{"all", "logsd", "loglogsdf" or "res" plot }
  \item{mainQ}{ include plot title }
  \item{subQ}{ include subtitle }
    \item{lag.max}{ maximum lag in residual autocorrelation plot and test }
  \item{...}{ optional arguments }
}

\value{None. Plot produced is a side-effect.}

\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{\code{\link{artfima}}}

\examples{
z <- artsim(n=500, d=5/6, lambda=0.045)
ans <- artfima(z)
plot(ans)
plot(ans, which="loglogsdf", subQ=FALSE, mainQ=FALSE)
title(main="Simulated Series", sub="delta=5/6")
}

\keyword{ts}

