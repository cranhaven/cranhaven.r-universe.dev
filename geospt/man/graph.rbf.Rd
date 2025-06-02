\name{graph.rbf}
\alias{graph.rbf}

\title{
Graph that describes the behavior of the optimized \emph{eta} and \emph{rho} parameters, associated with a radial basis function
}
\description{
Function for plotting the RMSPE for several values of the smoothing parameter
eta with the same dataset. A curve is fitted to the points, and
then the optimal eta that provides the smallest
RMSPE is determined from the curve, by the \code{\link[stats]{optimize}} function from the \code{stats}
package.
}
\usage{
graph.rbf(formula, data, eta.opt, rho.opt, n.neigh, func, np, x0, eta.dmax,
rho.dmax, P.T, iter, ...)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \emph{z}, for a \emph{rbf} detrended use \emph{z~1}; for a \emph{rbf} with trend, suppose \emph{z} is linearly dependent on \emph{x} and \emph{y}, use the formula \emph{z~x+y} (linear trend).}
\item{data}{SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.}
\item{eta.opt}{logical, indicating whether the parameter \emph{eta} should be regarded as fixed (eta.opt = FALSE) or should be estimated (eta.opt = TRUE)}
\item{rho.opt}{logical, indicating whether the parameter \emph{rho} should be regarded as fixed (rho.opt = FALSE) or should be estimated (rho.opt = TRUE)}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf} prediction, where nearest is defined in terms of the spatial locations}
\item{func}{function to be optimized. The following radial basis function model types are currently available: gaussian "GAU", exponential "EXPON", trigonometric "TRI", thin plate spline "TPS", completely regularized spline "CRS", spline with tension "ST", inverse multiquadratic "IM", and multiquadratic "M", are currently available}
\item{np}{number of points, where the radial basis function is calculated}
\item{x0}{starting point for searching the optimum. Defaults to c(0.5, 0.5), \emph{eta} and \emph{rho} respectively. Use this statement only if eta and rho are equal to TRUE.}
\item{eta.dmax}{maximum value of the range of the \emph{eta} parameter that will be evaluated by the \code{\link[stats]{optimize}} function}
\item{rho.dmax}{maximum value of the range of the \emph{rho} parameter that will be evaluated by the \code{\link[stats]{optimize}} function}
\item{P.T}{logical. Print Table (TRUE) or not (FALSE). Default P.T=NULL.}
\item{iter}{The maximum allowed number of function evaluations.}
\item{...}{further parameters to be passed to the minimization functions \code{\link{optimize}} or \code{\link[minqa]{bobyqa}}, typically arguments of the type control() which control the behavior of the minimization algorithm. See documentation about the selected minimization function for further details.
}
}
\value{
Returns a graph that describes the behavior of the optimized \emph{eta} or \emph{rho} parameter, and a table of values associated with the graph including optimal smoothing \emph{eta} or \emph{rho} parameters. If both \emph{eta} and \emph{rho} are FALSE simultaneously, then the function returns a list with; the best value obtained from the combinations smoothing \emph{eta} and \emph{rho} parameters and a lattice plot of class "trellis" with RMSPE pixel values associated with combinations of \emph{eta} and \emph{rho} parameters. Finally if both \emph{eta} and \emph{rho} are TRUE, the function will return a list with the best combination of values of the smoothing \emph{eta} or \emph{rho} parameters and the RMSPE associated with these.
}
\references{
Johnston, K., Ver, J., Krivoruchko, K., Lucas, N. 2001. \emph{Using ArcGIS Geostatistical Analysis}. ESRI.
}
\examples{
data(preci)
coordinates(preci)<-~x+y
# optimizing eta
graph.rbf(prec~1, preci, eta.opt=TRUE, rho.opt=FALSE, n.neigh=9, func="TPS",
    np=40, eta.dmax=0.2, P.T=TRUE)
\dontrun{
# optimizing rho
graph.rbf(prec~x+y, preci, eta.opt=FALSE, rho.opt=TRUE, n.neigh=9, func="M",
    np=20, rho.dmax=2, P.T=TRUE)
# optimizing eta and rho
tps.lo <- graph.rbf(prec~1, preci, eta.opt=TRUE, rho.opt=TRUE, n.neigh=9, func="TPS",
    eta.dmax=2, rho.dmax=2, x0=c(0.1,0.1), iter=40)
tps.lo$Opt  # best combination of eta and rho obtained
# other optimization options
opt.u <- uobyqa(c(0.1,0.1), rbf.cv1, control = list(maxfun=40), formula=prec~1, data=preci,
                n.neigh=9, func="TPS")
opt.n <- newuoa(c(0.1,0.1), rbf.cv1, control = list(maxfun=40), formula=prec~1, data=preci,
                n.neigh=9, func="TPS")
# lattice of RMSPE values associated with a range of eta and rho, without optimization
tps.l <- graph.rbf(prec~1, preci, eta.opt=FALSE, rho.opt=FALSE, n.neigh=9, func="TPS",
    np=10, eta.dmax=2, rho.dmax=2)
tps.l$opt.table  # best combination of eta and rho obtained from lattice
tps.l$spplot     # lattice of RMSPE
}
}

\keyword{ spatial }
