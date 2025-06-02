\name{graph.rbfST}
\alias{graph.rbfST}

\title{
Graph that describes the behavior of the optimized \emph{eta} and \emph{rho} parameters, associated with a spatio-temporal radial basis function.
}
\description{
Function for plotting the RMSPE for several values of the smoothing parameter
\emph{eta} with the same dataset. A curve is fitted to the points, and
then the optimal \emph{eta} that provides the smallest RMSPE is determined from the
curve, by the \code{\link[stats]{optimize}} function from the \code{stats}
package.
}
\usage{
graph.rbfST(formula, data, eta.opt, rho.opt, n.neigh, func, np, xo, eta.dmax,
rho.dmax, P.T, iter, ...)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables (covariates or the principal coordinates); suppose the dependent variable has name \eqn{z_{st}}, for a \emph{rbfST} detrended use \eqn{z_{st}}\code{~1}, for a \emph{rbfST} with trend, suppose \eqn{z_{st}} is linearly dependent on \code{x} and \code{y}, use the formula \eqn{z_{st}}\code{~x+y} (linear trend).}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{eta.opt}{logical, indicating whether the parameter \emph{eta} should be regarded as fixed (eta.opt = FALSE) or should be estimated (eta.opt = TRUE)}
\item{rho.opt}{logical, indicating whether the parameter \emph{rho} should be regarded as fixed (rho.opt = FALSE) or should be estimated (rho.opt = TRUE)}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbfST}
prediction, where nearest is defined in terms of the spatio-temporal locations}
\item{func}{function to be optimized. The following radial basis function spatio-temporal model types are currently available: gaussian "GAU", exponential "EXPON", trigonometric "TRI", thin plate spline "TPS", completely regularized spline "CRS", spline with tension "ST", inverse multiquadratic "IM", and multiquadratic "M", are currently available}
\item{np}{number of points, where the radial basis function spatio-temporal is calculated}
\item{xo}{starting point for searching the optimum. Defaults to c(0.5, 0.5), \emph{eta} and \emph{rho} respectively. Use this statement only if \emph{eta} and \emph{rho} are equal to TRUE.}
\item{eta.dmax}{maximum value of the range of the \emph{eta} parameter that will be evaluated by the \code{\link[stats]{optimize}} function.}
\item{rho.dmax}{maximum value of the range of the \emph{rho} parameter that will be evaluated by the \code{\link[stats]{optimize}} function.}
\item{P.T}{logical. Print table (TRUE) or not (FALSE). Default P.T=NULL.}
\item{iter}{The maximum allowed number of function evaluations.}
\item{...}{further parameters to be passed to the minimization functions \code{\link{optimize}} or \code{\link[minqa]{bobyqa}}, typically arguments of the type control() which control the behavior of the minimization algorithm. See documentation about the selected minimization function for further details.
}
}
\value{
Returns a graph that describes the behavior of the optimized \emph{eta} or \emph{rho} parameters and a table of values associated with the graph including optimal smoothing \emph{eta} or \emph{rho} parameters. If both \emph{eta} and \emph{rho} are FALSE simultaneously then the function returns a list with the best value obtained from the combinations smoothing \emph{eta} and \emph{rho} parameters and a lattice plot of class "trellis" with RMSPE pixel values associated with combinations of \emph{eta} and \emph{rho} parameters. Finally, if both \emph{eta} and \emph{rho} are TRUE, the function will return a list with the best combination of values of the smoothing \emph{eta} or \emph{rho} parameters and the RMSPE associated with these.
}

\references{
Johnston, K., Ver, J., Krivoruchko, K., Lucas, N. (2001). \emph{Using ArcGIS Geostatistical Analysis}. ESRI.

Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]}
}
\seealso{
\code{\link{rbfST}}, \code{\link{rbfST.cv}}
}

\examples{
\dontrun{
data(croatiadb)
coordinates(croatiadb)<-~x+y
# optimizing eta
graph.rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb, eta.opt=TRUE,
             rho.opt=FALSE, n.neigh=30, func="TPS", np=40,  eta.dmax=2, P.T=TRUE)
# optimizing rho
graph.rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb, eta.opt=FALSE,
             rho.opt=TRUE, n.neigh=30, func="M", np=20, rho.dmax=2, P.T=TRUE)
# optimizing eta and rho
tps.lo <- graph.rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb,
       eta.opt=TRUE,rho.opt=TRUE, n.neigh=25, func="TPS",  eta.dmax=0.2,
       rho.dmax=0.2, xo=c(0.1,0.1), iter=50)
tps.lo  # best combination of eta and rho obtained
# lattice of RMSPE values associated with a range of eta and rho, without optimization
tps.la <- graph.rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb,
       eta.opt=FALSE, rho.opt=FALSE, n.neigh=30,  func="TPS", np=10,  eta.dmax=0.2,
       rho.dmax=0.2)
tps.l$table  # best combination of eta and rho obtained
tps.l$spplot  # lattice of RMSPE
}
}

\keyword{ spatial }
