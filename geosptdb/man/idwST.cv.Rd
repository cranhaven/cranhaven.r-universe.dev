\name{idwST.cv}
\alias{idwST.cv}

\title{
IDW spatio-temporal leave-one-out cross validation
}
\description{
Generate the RMSPE value which is given by Inverse Distance Weighting (IDW) interpolation.
}
\usage{
idwST.cv(formula, data, n.neigh, C, factor.p, progress)
}

\arguments{
\item{formula}{formula that defines a detrended linear model, use \eqn{z_{st}}\code{~1}.}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf.st}
prediction, where nearest is defined in terms of the spatio-temporal locations}
\item{C}{numeric; associated to time factor, we recommend using the parameter found by
minimizing the root-mean-square prediction errors using cross-validation. Using \emph{idwST.cv} and \code{\link[stats]{optimize}}}
\item{factor.p}{numeric; specify the inverse distance weighting power (\code{p} is the exponent that influences the weighting or optimal smoothing parameter)}
\item{progress}{whether a progress bar shall be printed for spatio-temporal inverse-distance weighted function; default=TRUE}
}
\value{
returns the RMSPE value
}
\references{
Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 
}

\seealso{
\code{\link{idwST}}, \code{\link{rbfST}}
}
\examples{
\dontrun{
data(croatiadb)
coordinates(croatiadb) <- ~x+y
idwST.cv(MTEMP~1, croatiadb[,1:2], n.neigh=10, C=1, factor.p=2)
}
}

\keyword{ spatial }
