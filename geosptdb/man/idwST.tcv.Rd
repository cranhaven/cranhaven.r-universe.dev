\name{idwST.tcv}
\alias{idwST.tcv}

\title{
table of idw spatio-temporal leave-one-out cross validation
}
\description{
Generates a table with the results of inverse distance weighting spatio-temporal interpolation (\emph{idwST}) from leave-one-out cross validation method.
}
\usage{
idwST.tcv(formula, data, n.neigh, C, factor.p, progress)
}

\arguments{
\item{formula}{formula that defines a detrended linear model, use \eqn{z_{st}}\code{~1}.}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{n.neigh}{number of nearest observations that should be used for a \emph{idwST}
prediction where nearest is defined in terms of the spatio-temporal locations}
\item{C}{numeric; associated to time factor, we recommend using the parameter found by
minimizing the root-mean-square prediction errors using cross-validation. Using \emph{idwST.cv} and \code{\link[stats]{optimize}}}
\item{factor.p}{numeric; specify the inverse distance weighting power (\emph{p} is the exponent that influences the weighting or optimal smoothing parameter)}
\item{progress}{whether a progress bar shall be printed for spatio-temporal inverse-distance weighted function; default=TRUE}
}
\details{
Leave-one-out cross validation (LOOCV) consists of removing data, one at a time, and then trying to predict it. Next, the predicted value can be compared to the actual (observed) value to assess how well the prediction is working. The observed value is left out because \emph{idwST} would otherwise predict the value itself.
}
\value{
data frame contain prediction columns, observed values, residuals, the prediction variance, zscore (residual divided by standard error)
which left with NA's, the fold column which is associated to cross-validation count, coordinates data and time. Prediction columns and residuals are obtained from cross-validation data points.
}
\seealso{
\code{\link{idwST}}
}
\examples{
\dontrun{
data(croatiadb)
coordinates(croatiadb) <- ~x+y
idw.t <- idwST.tcv(MTEMP~1, croatiadb, n.neigh=10, C=1.0054, factor.p=1.9585)
criteriaST.cv(idw.t)
}
}

\keyword{ spatial }
