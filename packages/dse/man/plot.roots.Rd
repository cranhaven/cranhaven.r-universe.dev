\name{plot.roots}
\alias{plot.roots}
\title{Plot Model Roots}
\description{Calculate and plot roots of a model.}
\usage{
    \method{plot}{roots}(x, pch='*', fuzz=0, ...)}
\arguments{
    \item{x}{An object of class roots (a vector of complex (or real) 
values as returned by the function roots).}
    \item{pch}{character to be used for the plot (passed to plot.default).}
    \item{fuzz}{If non-zero then roots within fuzz distance are considered equal.}
    \item{...}{(further arguments, currently disregarded).}
}
\section{Side Effects}{The roots and a unit circle are plotted on the complex plane.}
\value{
The eigenvalues of the state transition matrix or the inverse of the roots of the
determinant of the AR polynomial are returned invisibly.
}
\seealso{
    \code{\link{addPlotRoots}}
    \code{\link{roots}}
    \code{\link{stability}}
    \code{\link{McMillanDegree}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    plot(roots(model))
}
\concept{DSE}
\keyword{ts}

