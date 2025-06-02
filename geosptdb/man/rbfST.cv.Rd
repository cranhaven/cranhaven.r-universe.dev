\name{rbfST.cv}
\alias{rbfST.cv}

\title{
Leave-one-out cross validation for spatio-temporal radial basis function 
}
\description{
It generates the RMSPE value, which is given by the radial basis function
with smoothing \code{eta} and robustness \code{rho} parameters.
}
\usage{
rbfST.cv(formula, data, eta, rho, n.neigh, func)
}

\arguments{
\item{formula}{
formula that defines the dependent variable as a linear model of independent variables (covariates or the principal coordinates); suppose the dependent variable has name \eqn{z_{st}}, for a \emph{rbfST} detrended use \eqn{z_{st}}\code{~1}, for a \emph{rbfST} with trend, suppose \eqn{z_{st}} is linearly dependent on \code{x} and \code{y}, use the formula \eqn{z_{st}}\code{~x+y} (linear trend).
}
\item{data}{
SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.
}
\item{eta}{
the optimal smoothing parameter, we recommend using the parameter found by minimizing the root-mean-square prediction errors using cross-validation.
}
\item{rho}{
optimal robustness parameter, we recommend using the value obtained by minimizing the root-mean-square prediction errors with cross-validation. \emph{eta} and \emph{rho} parameters can be optimized simultaneously, through 
the \code{\link[minqa]{bobyqa}} function from \code{nloptr} or  \code{minqa} packages.
}
\item{n.neigh}{
number of nearest observations that should be used for a \emph{rbfST}
prediction, where nearest is defined in terms of the spatio-temporal locations.
}
\item{func}{
spatio-temporal radial basis function; model type: "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
}
\value{
returns the RMSPE value
}
\references{
Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 
}

\seealso{
\code{\link{rbfST}}, \code{\link{graph.rbfST}}
}
\examples{
data(croatiadb)
coordinates(croatiadb) <- ~x+y
rbfST.cv(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, croatiadb, eta=0.0108, rho=0.00004, 
          n.neigh=25, func="TPS")
}

\keyword{ spatial }
