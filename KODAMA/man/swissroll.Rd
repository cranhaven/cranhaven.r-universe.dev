\name{swissroll}

\alias{swissroll}

\title{Swiss Roll Data Set Generator}

\description{Computes the Swiss Roll data set of a given number of data points.}

\usage{
swissroll(N=1000)
}

\arguments{
  \item{N}{Number of data points.}
}

\value{
    The function returns a three dimensional matrix.
  }

\author{Stefano Cacciatore and Leonardo Tenori}

  \references{

Balasubramanian M, Schwartz EL\cr
The isomap algorithm and topological stability. \cr
\emph{Science} 2002;295(5552):7.
\cr
\cr
Roweis ST, Saul LK\cr
Nonlinear dimensionality reduction by locally linear embedding.\cr
\emph{Science} 2000;290(5500):2323-6.
\cr
\cr
Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}

  \seealso{\code{\link{helicoid}},\code{\link{dinisurface}},\code{\link{spirals}}}

  \examples{
require("rgl")
x=swissroll()
open3d()
plot3d(x, col=rainbow(1000),box=FALSE,size=3)
}

\keyword{dataset}
