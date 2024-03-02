\name{addPlotRoots}
\alias{addPlotRoots}
\title{Add Model Roots to a plot}
\description{

Calculate and plot roots of a model.
}
\usage{
    addPlotRoots(v, pch='*', fuzz=0)
}
\arguments{

    \item{v}{An object containing a TSmodel.}
    \item{pch}{Character to use for plotting.}
    \item{fuzz}{If non-zero then roots within fuzz distance are considered equal.}
}
\section{Side Effects}{
The roots are addeded to an existing plot.
}
\value{
The eigenvalues of the state transition matrix or the inverse of the roots of the
determinant of the AR polynomial are returned invisibly.
}
\seealso{
    \code{\link{plot.roots}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
plot(roots(model))
addPlotRoots(toSS(model))
}
\concept{DSE}
\keyword{ts}

