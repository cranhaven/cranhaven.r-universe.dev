\name{rbfST.cv1}
\alias{rbfST.cv1}

\title{
RMSPE value result of leave-one-out cross validation for \emph{rbfST}
}
\description{
It generates the RMSPE value which is given by the spatio-temporal radial basis function
with smoothing \emph{eta} and robustness \emph{rho} parameters.
}
\usage{
rbfST.cv1(param, formula, data, n.neigh, func)
}

\arguments{
\item{param}{
vector starting points (\emph{eta} and \emph{rho} respectively) for searching the \emph{RMSPE} optimum.
}
\item{formula}{
formula that defines the dependent variable as a linear model of independent variables (covariates or the principal coordinates); suppose the dependent variable has name \eqn{z_{st}}, for a \emph{rbfST} detrended use \eqn{z_{st}}\code{~1}, for a \emph{rbfST} with trend, suppose \eqn{z_{st}} is linearly dependent on \code{x} and \code{y}, use the formula \eqn{z_{st}}\code{~x+y} (linear trend).
}
\item{data}{
SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.
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
\seealso{
\code{\link{rbfST}}, \code{\link{rbfST.cv}}, \code{\link{graph.rbfST}} 
}
\examples{
require(minqa)
data(croatiadb)
coordinates(croatiadb) <- ~x+y

\dontrun{
rbf.im <- bobyqa(c(0.5, 0.5), rbfST.cv1, lower=c(1e-05,0), upper=c(2,2), 
              formula=MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb, n.neigh=25, 
              func="IM", control=list(maxfun=50))         
}

# obtained with the optimal values previously estimated
rbfST.cv1(c(0.847050095690357,0.104157855356128), MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, 
           croatiadb, n.neigh=25, func="IM")
}

\keyword{ spatial }