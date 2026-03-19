\name{cpness}
\alias{cpness}
\title{
  Core-peripheriness measure
}
\description{
  Computation of the cpness measure for a bipartite graph/network
}
\usage{
cpness(web, type=c("automatic","binary","integer","float"), plot=TRUE, fastplot=FALSE)
}
\arguments{
  \item{web}{A \code{matrix} with elements of a set (e.g., plants) as rows,
    elements of a second set (e.g., pollinators) as columns and
    number of interactions as entries. Species names can be indicated in
    the row or column names.}
  \item{type}{Type of matrix. This should be (an unambiguous
    abbreviation of) one of \code{automatic} (default),
    \code{binary}, \code{integer} or \code{float}. With \code{automatic}
    (default), the type of matrix is automatically deduced inside the
    function. Using \code{binary}, the matrix is binarized in any case. See the "Details" section.}
  \item{plot}{Plot the matrix reordered according to the core-periphery
    partitioning. \code{TRUE} by default.}
  \item{fastplot}{If \code{TRUE}, the matrix plot is performed using the
    fast \code{image} function instead of the \code{visweb} function from the
    \code{bipartite} package, without species names. Only for large matrices. \code{FALSE} by default. }
}
\details{
In a matrix displaying a core-periphery structure, there is a species ordering (i.e. an ordering in rows and columns) such that interactions are distributed in an L-shape. This L-shape is composed by four blocks of varying connectance: block C11 represents the core; blocks C12 and C21 include the interactions between core and periphery; block C22 includes the interactions that occur between peripheral species.
  
This fonction computes the core-peripheriness (CPness), as
  CPness=(E11+E12+E21)/E, where Eij is the number of interactions
  (edges)  or the sum of weights for each block (Eij for block ij) or
  for the entire network (E).  Here, we rely on a stochastic block model
  (SBM) to detect the four groups/blocks C11, C12, C21 and C22 when they
  actually exist. However, the SBM can fail in finding these blocks: in
  these cases, there is no core-periphery partition and the CPness value is set to \code{NA}.

This function can deal with binary and weighted networks with the appropriate statistical distribution (Bernouilli for \code{binary} data, Poisson for \code{integer} weights, and Gaussian for \code{float} weights). Note that it is often advisable to log-transform float data before running the \code{cpness} function.
}
\value{
\code{cpness} returns an object of class \code{list} with the following components:
\item{cpness}{The value of the core-peripheriness measure.  \code{NA} when no  core-periphery partition is found.}
\item{rowmembership}{An integer \code{vector} indicating the group to which species in rows are belonging. Group 1 is core and group 2 is periphery, unless there is only a single group.}
\item{colmembership}{Same as \code{rowmembership} for species in columns.}
}
\references{
Ana M. Martin Gonzalez, Diego P. Vazquez, Rodrigo Ramos-Jiliberto, Sang Hoon Lee & Vincent Miele, Core-periphery structure in mutualistic networks: an epitaph for nestedness? BiorXiv (2020) <doi:10.1101/2020.04.02.021691>
}
\author{
Authors: Vincent Miele
Maintainer: Vincent Miele <vincent.miele@univ-lyon1.fr>
}
\examples{
library(bipartite)
data(mosquin1967)
result <- cpness(mosquin1967, type="automatic", plot=TRUE)
print(result)

data(junker2013)
result <- cpness(junker2013, type="automatic", plot=TRUE, fastplot=TRUE)
print(result$cpness)
print(table(result$rowmembership))
print(table(result$colmembership))
}