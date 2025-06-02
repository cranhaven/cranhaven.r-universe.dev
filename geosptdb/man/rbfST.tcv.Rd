\name{rbfST.tcv}
\alias{rbfST.tcv}

\title{
table of rbf spatio-temporal cross validation, leave-one-out
}
\description{
It generates a table with the results of the evaluation of radial basis functions spatio-temporal (\emph{rbfST}): gaussian (GAU), exponential (EXPON), trigonometric (TRI), thin plate spline (TPS), completely regularized spline (CRS), spline with tension (ST), inverse multiquadratic (IM), and multiquadratic (M) from the leave-one-out cross validation method.
}
\usage{
rbfST.tcv(formula, data, eta, rho, n.neigh, func, progress)
}

\arguments{
\item{formula}{
formula that defines the dependent variable as a linear model of independent variables (covariates or the principal coordinates); suppose the dependent variable has name \eqn{z_{st}}, for a \emph{rbf.st} detrended use \eqn{z_{st}}\code{~1}, for a \emph{rbf.st} with trend, suppose \eqn{z_{st}} is linearly dependent on \code{x} and \code{y}, use the formula \eqn{z_{st}}\code{~x+y} (linear trend).
}
\item{data}{
SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.
}
\item{eta}{
the optimal smoothing parameter; we recommend using the parameter found by minimizing the root-mean-square prediction errors using cross-validation
}
\item{rho}{
optimal robustness parameter, we recommend using the value obtained by minimizing the root-mean-square prediction errors with cross-validation. \emph{eta} and \emph{rho} parameters can be optimized simultaneously, through the \code{\link[minqa]{bobyqa}} function from \code{nloptr} or  \code{minqa} packages
}
\item{n.neigh}{
number of nearest observations that should be used for a \emph{rbfST}
prediction, where nearest is defined in terms of the spatio-temporal locations.
}
\item{func}{
spatio-temporal radial basis function; model type: "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available
}
\item{progress}{
whether a progress bar shall be printed for spatio-temporal radial basis functions; default=TRUE
}
}
\details{
Leave-one-out cross validation (LOOCV) visits a data point, predicts
the value at that location by leaving out the observed value, and
proceeds with the next data point. The observed value is left out
because rbf.st would otherwise predict the value itself.
}
\value{
data frame contain prediction columns, observed values, residuals, 
the prediction variance, zscore (residual divided by standard error) 
which left with NA's, the fold column which is associated to 
cross-validation count, coordinates data and time. Prediction columns and residuals are obtained 
from cross-validation data points.
}
\seealso{
\code{\link{rbfST}}
}
\examples{
data(croatiadb)
coordinates(croatiadb) <- ~x+y
rbfST.tcv(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, croatiadb, eta=0.0108, rho=0.00004, 
          n.neigh=30, func="TPS")
}

\keyword{ spatial }
