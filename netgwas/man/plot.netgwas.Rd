\name{plot.netgwas}
\alias{plot.netgwas}

\title{ plot for S3 class "netgwas" }

\description{
Plot the graph path which is the output of two functions \code{\link{netsnp}}, \code{\link{netphenogeno}}.
}

\usage{
\method{plot}{netgwas}( x, n.markers=NULL , ... )
}

\arguments{
	\item{x}{An object from "netgwas" class.}
	\item{n.markers}{A vector containing number of variables/markers in each group/chromosome. For example, the CviCol dataset that is provided in the package contains 5 chromosomes/ groups which the total number of markers is \eqn{p = 90}, where the first 24 markers belong into chromosome 1, the next 14 markers into chromosome 2, ..., and chromosome 5 contains 19 markers. Thus, n.mrkr = c(24,14,17,16,19). If \code{n.mrkr = NULL}, in the graph visualization all markers are represented same colour.}
	\item{\dots}{System reserved (No specific usage)}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}
}

\references{
Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}


\seealso{\code{\link{netmap}}, \code{\link{netsnp}}, \code{\link{netphenogeno}}.}

