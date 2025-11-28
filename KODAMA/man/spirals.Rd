\name{spirals}

\alias{spirals}

\title{Spirals Data Set Generator}

\description{Produces a data set of spiral clusters.}

\usage{
spirals(n=c(100,100,100),sd=c(0,0,0))
}



\arguments{
  \item{n}{a vector of integer. The length of the vector is the number of clusters and each number corresponds to the number of data points in each cluster.}
  \item{sd}{amount of noise for each spiral.}

}

\value{
    The function returns a two dimensional data set.
  }

\author{Stefano Cacciatore and Leonardo Tenori}

\references{
Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}

  \seealso{\code{\link{helicoid}},\code{\link{dinisurface}},\code{\link{swissroll}}}
  \examples{

v1=spirals(c(100,100,100),c(0.1,0.1,0.1))
plot(v1,col=rep(2:4,each=100))
v2=spirals(c(100,100,100),c(0.1,0.2,0.3))
plot(v2,col=rep(2:4,each=100))
v3=spirals(c(100,100,100,100,100),c(0,0,0.2,0,0))
plot(v3,col=rep(2:6,each=100))
v4=spirals(c(20,40,60,80,100),c(0.1,0.1,0.1,0.1,0.1))
plot(v4,col=rep(2:6,c(20,40,60,80,100)))
}

\keyword{dataset}
