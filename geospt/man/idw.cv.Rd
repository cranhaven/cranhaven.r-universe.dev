\name{idw.cv}
\alias{idw.cv}

\title{
idw cross validation leave-one-out
}
\description{
Generate the RMSPE value, which is given by the idw function
with \code{p} smoothing parameter.
}
\usage{
idw.cv(formula, locations, data, nmax = Inf, nmin = 0, p = 2, progress=FALSE, ...)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \emph{z}, for a \emph{idw} detrended use \emph{z~1}}
\item{data}{SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.}
\item{locations}{object of class \emph{Spatial}, or (deprecated) formula defines the spatial data locations (coordinates) such as \emph{~x+y}}
\item{nmax}{
number of nearest observations that should be used for a \emph{idw} prediction, where nearest is defined in terms of the spatial locations. By default, all observations are used.
}
\item{nmin}{
if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated; see maxdist.
}
\item{p}{
value of smoothing parameter; we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation. Default is 2.
}
\item{progress}{logical. Use TRUE to see the percentage of progress of the process and FALSE otherwise). Default progress=FALSE.}
\item{...}{Other arguments passed to \emph{idw}}
}
\value{
returns the RMSPE value
}
\seealso{
\code{\link[gstat:krige]{gstat::idw()}}
}
\examples{
data(preci)
idw.cv(prec~1, ~x+y, preci, nmax=9, nmin=9, p=2, progress=TRUE)
}


\keyword{ spatial }
