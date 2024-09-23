\name{plot.CAMAN.BIMIXALG.object}
\alias{plot.CAMAN.BIMIXALG.object}
\alias{plot.CAMAN.BIEM.object}
\title{Plot ellipses}
\description{Graphical display of the EM algorithm and bivariate MIXALG}
\usage{
\method{plot}{CAMAN.BIMIXALG.object}(x, ellipse, ...)
\method{plot}{CAMAN.BIEM.object}(x, ellipse, ...)
}
\arguments{
\item{x}{a \code{CAMAN.BIEM.object} or a \code{CAMAN.BIMIXALG.object}}
\item{ellipse}{logical. Plot ellipse? Defaults is \code{TRUE}}
\item{...}{further arguments. Currently ignored.}
}
\references{
Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models}. Berlin: Springer.
}
\author{
Peter Schlattmann 
}


\keyword{hplot}