\name{rbf.cv}
\alias{rbf.cv}

\title{
rbf cross validation leave-one-out
}
\description{
Generate the RMSPE value, which is given by the radial basis function
with smoothing parameter \code{eta} and robustness parameter \code{rho}.
}
\usage{
rbf.cv(formula, data, eta, rho, n.neigh, func)
}

\arguments{
\item{formula}{
formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \code{z}, for a \emph{rbf} detrended use \code{z~1}, for a \emph{rbf} with trend, suppose \code{z} is linearly dependent on \code{x} and \code{y}, use the formula \code{z~x+y} (linear trend).
}
\item{data}{
SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.
}
\item{eta}{
the optimal smoothing parameter; we recommend using the parameter found by minimizing the root-mean-square prediction errors using cross-validation
}
\item{rho}{
value of optimal robustness parameter; we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation.
\emph{eta} and \emph{rho} parameters can be optimized simultaneously, through the \code{bobyqa} function from \code{nloptr} or \code{minqa} packages
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
data(preci)
coordinates(preci)<-~x+y
rbf.cv(prec~1, preci, eta=0.2589, rho=0, n.neigh=9, func="M") 
}

\keyword{ spatial }
