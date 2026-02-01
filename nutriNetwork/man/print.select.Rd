\name{print.select}
\alias{print.select}
\title{
Print function for S3 class "select"
}
\description{
Print function for \code{\link{selectnet}}. 
}
\usage{
\method{print}{select}(x, ...)
}
\arguments{
  \item{x}{
An object with S3 class "select"
}
  \item{\dots}{
System reserved (No specific usage)
}
}

\author{ 
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\references{
Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}

\seealso{
\code{\link{selectnet}}
}

\examples{
 data(vfit)
out <- nutriNetwork(vfit, method ="npn")
sel <-  selectnet(out)
#A pxp adjacency matrix 
sel$opt.adj
}