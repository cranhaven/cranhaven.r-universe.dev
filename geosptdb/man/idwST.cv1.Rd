\name{idwST.cv1}
\alias{idwST.cv1}

\title{
Generate a RMSPE value, result of leave-one-out cross validation
}
\description{
Generate the RMSPE value which is given by the radial basis function spatio-temporal
with number of nearest observations\emph{n.neigh} associated to time factor \emph{C} and optimal smoothing parameter \emph{factor.p}.
}
\usage{
idwST.cv1(param, formula, data, n.neigh, progress)
}

\arguments{
\item{param}{vector starting points (\emph{C} and \emph{factor.p} respectively) for searching the \emph{RMSPE} optimum.}
\item{formula}{formula that defines a detrended linear model, use \eqn{z_{st}}\code{~1}.}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf.st}
prediction where nearest is defined in terms of the spatio-temporal locations}
\item{progress}{whether a progress bar shall be printed for spatio-temporal inverse-distance weighted function; default=FALSE}
}
\value{
returns the RMSPE value
}
\seealso{
\code{\link{idwST}}, \code{\link{idwST.cv}}, \code{\link{idwST.tcv}} 
}
\examples{
require(minqa)
data(croatiadb)
coordinates(croatiadb) <- ~x+y

\dontrun{
idwST.opt <- bobyqa(c(1, 2), idwST.cv1, lower=c(0,0.1), upper=c(2,4), formula=MTEMP~1, 
                    data=croatiadb[,1:2], n.neigh=10, progres=F, control=list(maxfun=50))         

# obtained with optimal values previously estimated (33 iterations)
idwST.cv1(c(1.00538675066736,1.95853920335545), MTEMP~1, data=croatiadb[,1:2], n.neigh=10, 
          progress=T)
}
}

\keyword{ spatial }