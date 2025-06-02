\name{rbf.cv1}
\alias{rbf.cv1}

\title{
Generates a RMSPE value, result of cross validation leave-one-out
}
\description{
Generate the RMSPE value, which is given by the radial basis function
with smoothing parameter \emph{eta} and robustness parameter \emph{rho}.
}
\usage{
rbf.cv1(param, formula, data, n.neigh, func)
}

\arguments{
\item{param}{
vector starting points (\emph{eta} and \emph{rho} respectively) for searching the \emph{RMSPE} optimum.
}
\item{formula}{
formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \emph{z}, for a \emph{rbf} detrended use \emph{z~1}, for a \emph{rbf} with trend, suppose \emph{z} is linearly dependent on \emph{x} and \emph{y}, use the formula \emph{z~x+y} (linear trend).
}
\item{data}{
SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.
}
\item{n.neigh}{
number of nearest observations that should be used for a \emph{rbf} prediction, where nearest is defined in terms of the spatial locations
}
\item{func}{
radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
}
\value{
returns the RMSPE value
}
\seealso{
\code{\link{rbf}}
}
\examples{
\dontrun{
data(preci)
coordinates(preci) <- ~x+y
bobyqa(c(0.5, 0.5), rbf.cv1, lower=c(1e-05,0), upper=c(2,2), formula=prec~x+y, data=preci,
    n.neigh=9, func="TRI")
# obtained with the optimal values previously estimated
rbf.cv1(c(0.2126191,0.1454171), prec~x+y, preci, n.neigh=9, func="TRI")  
}
}

\keyword{ spatial }