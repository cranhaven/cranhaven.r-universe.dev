\name{plot.simgeno}
\alias{plot.simgeno}

\title{ Plot function for \code{S3} class \code{"simgeno"} }

\description{ Visualizes the pattern of the true graph, the adjacency matrix, precison matrix and the covariance matrix of the simulated data.}

\usage{
\method{plot}{simgeno}(x, layout = layout.fruchterman.reingold, ...)
}

\arguments{
	\item{x}{An object of \code{S3} class \code{"simgeno"}, from function \code{\link{simgeno}}. }
	\item{layout}{The default is \code{"layout.fruchterman.reingold"}.}
	\item{\dots}{ System reserved (No specific usage)}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\references{
Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}

\seealso{\code{\link{simgeno}}}

\examples{
\dontrun{
# Generating discrete ordinal data with "genome-like" graph structure
data.sim <- simgeno(alpha = 0.01, beta = 0.02)
plot( data.sim )
}
}

