\name{elgrin}
\alias{elgrin}
\title{
  Teasing apart the effects of environmental variations and biotic interactions on species distributions
}
\description{
  ELGRIN jointly models the presence and absence of all species in a given area in function of environmental covariates and the topological structure of the known metaweb
}
\usage{
elgrin(presence, metaweb, environmentvar, ncores=1)
}
\arguments{
  \item{presence}{A \code{matrix} with presence/absence of species (rows) at
    different locations (columns), coded in 0/1 format (1 for presence).}
  \item{metaweb}{A \code{matrix} or a \code{igraph} object. This
    represents the metaweb/metawnetwork of interactions as an adjacency
    matrix or as a graph respectively.}
  \item{environmentvar}{A \code{matrix} with environmental variables (columns) at
    different locations (rows).}
  \item{ncores}{The number of cores to use, i.e. at most how many
    threads will run simultaneously. Default is 1 (no parallelization).}
}
\details{
  ELGRIN (in reference to Charles Elton and Joseph Grinnell) can handle the effects of both environmental factors and  known ecological interactions (aka a metanetwork) on species distributions. We rely on Markov random fields, a family of flexible models that can handle dependencies between variables using a graph.
}
\value{
\code{elgrin} returns an object of class \code{list} with the following components:
\item{a}{A \code{vector} of species intercepts.}
\item{al}{A \code{vector} of location intercepts.}
\item{b}{A \code{matrix} of linear environmental parameters.}
\item{c}{A \code{matrix} of quadratic environmental parameters. }
\item{betaPres}{A \code{vector} of co-presence strength (or avoidance when <0)  at each location.}
\item{betaAbs}{A \code{vector} of co-absence strength (or avoidance when <0) at each location.}
\item{compat}{A \code{matrix} with niche compatibility of species (rows) at
    different locations (columns), coded in 0/1 format (1 for compatibility)..}
}
\references{
Vincent Miele, Catherine Matias, Marc Ohlmann, Giovanni Poggiato, St
ephane Dray and Wilfried Thuiller, Quantifying the overall effect of
biotic1interactions on species communities along2environmental
gradients, arXiv <doi:10.48550/arXiv.2103.10433>
}
\author{
Authors: Vincent Miele
Maintainer: Vincent Miele <vincent.miele@univ-lyon1.fr>
}
\examples{
\donttest{data(virtualComCompetition)
vcom <- virtualComCompetition

result <- elgrin(vcom$presence, vcom$metaweb, vcom$environmentvar)
str(result)

plot(result$betaPres, result$betaAbs, xlab="Co-presence", ylab="Co-absence")
abline(h=0, col=2)
abline(v=0, col=2)
}
}
