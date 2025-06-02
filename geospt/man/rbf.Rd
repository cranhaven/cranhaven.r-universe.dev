\name{rbf}
\alias{rbf}

\title{
gaussian, exponential, trigonometric, thin plate spline, inverse multiquadratic, and multiquadratic radial basis function prediction
}
\description{               
Function for gaussian (GAU), exponential (EXPON), trigonometric (TRI), thin plate spline (TPS), completely regularized spline (CRS), 
spline with tension (ST), inverse multiquadratic (IM), and multiquadratic (M) radial basis function (\emph{rbf}),
where \emph{rbf} is in a local neighbourhood
}
\usage{
rbf(formula, data, eta, rho, newdata, n.neigh, func)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \code{z}, for a \emph{rbf} detrended use \code{z~1}, for a \emph{rbf} with trend, suppose \code{z} is linearly dependent on \code{x} and \code{y}, use the formula \code{z~x+y} (linear trend).}
\item{data}{SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.}
\item{eta}{the optimal smoothing parameter, we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation}
\item{rho}{the optimal parameter robustness, we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation.
eta and rho parameters can be optimized simultaneously, through the \code{bobyqa} function from \code{nloptr} or \code{minqa} packages}
\item{newdata}{data frame or spatial object with prediction/simulation locations; should contain attribute columns with the independent variables (if present) and (if locations is a formula) the coordinates with names, as defined in locations where you want to generate new predictions}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbf}
prediction, where nearest is defined in terms of the spatial locations}
\item{func}{radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
}
\details{
rbf function generates individual predictions from gaussian (GAU), exponential (EXPON), trigonometric (TRI)
thin plate spline (TPS), completely regularized spline (CRS), spline with tension (ST), 
inverse multiquadratic (IM), and multiquadratic (M) functions
}
\value{
Attributes columns contain coordinates, predictions, and the variance
column contains NA's
}
\examples{
data(preci)
coordinates(preci) <- ~x+y

# prediction case: one point
point <- data.frame(3,4)
names(point) <- c("x","y")
coordinates(point) <- ~x+y
rbf(prec~x+y, preci, eta=0.1460814, rho=0, newdata=point,n.neigh=10, func="TPS")

# prediction case: a grid of points
puntos<-expand.grid(x=seq(min(preci$x),max(preci$x),0.05), y=seq(min(preci$y),
max(preci$y),0.05))
coordinates(puntos) <- ~x+y
pred.rbf <- rbf(prec~x+y, preci, eta=0.1460814, rho=0, newdata=puntos, n.neigh=10, func="TPS")
coordinates(pred.rbf) = c("x", "y")
gridded(pred.rbf) <- TRUE

# show prediction map
spplot(pred.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
main = "rainfall map TPS", key.space=list(space="right", cex=0.8))
}
\keyword{ spatial }
