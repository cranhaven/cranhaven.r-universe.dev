\name{helicoid}

\alias{helicoid}

\title{Helicoid Data Set Generator}

\description{This function creates a data set based upon data points distributed on a Helicoid surface.}

\usage{
helicoid(N=1000)
}

\arguments{
  \item{N}{Number of data points.}
}

\value{
    The function returns a three dimensional data set.
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
\seealso{\code{\link{swissroll}},\code{\link{dinisurface}},\code{\link{spirals}}}

\examples{
require("rgl")
x=helicoid()
open3d()
plot3d(x, col=rainbow(1000),box=FALSE,size=3)
}

\keyword{dataset}
