\name{rbfST}
\alias{rbfST}

\title{
gaussian, exponential, trigonometric, thin plate spline, inverse multiquadratic, and multiquadratic radial basis function for spatio-temporal prediction
}
\description{               
Function for spatio-temporal interpolation from radial basis function (\emph{rbfST}), where \emph{rbfST} is in a local neighbourhood.

\emph{exponential (EXPON)}\cr
\deqn{
\phi(\delta)=e^{-\eta \delta},   \eta>0
}

\emph{gaussiano (GAU)}\cr
\deqn{
\phi(\delta)=e^{-\eta \delta^{2}},    \eta\neq0
}

\emph{multiquadratic (M)}\cr
\deqn{
\phi(\delta)=\sqrt{\eta^2+\delta^2},   \eta\neq0
}

\emph{inverse multiquadratic (IM)}\cr
\deqn{
\phi(\delta)=1/\sqrt{\eta^2+\delta^2},   \eta\neq0
}

\emph{thin plate spline (TPS)}\cr
\deqn{\phi(\delta)=(\eta\cdot\delta)^{2}log(\eta\cdot\delta), if: \delta>0, \eta>0}
\deqn{\phi(\delta) = 0, otherwise}

\emph{completely regularized spline (CRS)}\cr
\deqn{\phi(\delta) = \ln(\eta\cdot \delta/2)^{2}+E_{1}(\eta\cdot \delta/2)^{2}+C_{E}, if: \delta>0, \eta>0}
\deqn{\phi(\delta) = 0, otherwise}
where \eqn{\ln} is natural logarithm, \eqn{E_{1}(x)} is the exponential integral function, and \eqn{C_{E}} is the Euler constant.

\emph{spline with tension (ST)}
\deqn{
\phi(\delta)=\ln(\eta\cdot \delta/2)+K_{0}(\eta\cdot \delta)+C_{E}, if: \delta>0
}
\deqn{\phi(\delta) = 0, otherwise}
where \eqn{K_{0}(x)} is the modified Bessel function and \eqn{C_{E}} is the Euler constant.

}
\usage{
rbfST(formula, data, eta, rho, newdata, n.neigh, func, progress)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables (covariates or principal coordinates); suppose the dependent variable has name \eqn{z_{st}} for a \emph{rbfST} detrended use \eqn{z_{st}}\code{~1}; for a \emph{rbfST} with trend suppose \eqn{z_{st}} is linearly dependent on \code{x} and \code{y}, use the formula \eqn{z_{st}}\code{~x+y} (linear trend).}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{eta}{the optimal smoothing parameter, we recommend using the parameter
found by minimizing the root-mean-square prediction errors using cross-validation}
\item{rho}{optimal robustness parameter, we recommend using the value obtained by minimizing the root-mean-square prediction errors with cross-validation. \emph{eta} and \emph{rho} parameters can be optimized simultaneously, through the \code{\link[minqa]{bobyqa}} function from \code{nloptr} or  \code{minqa} packages}
\item{newdata}{data frame or spatial object with prediction/simulation spatio-temporal locations; should contain attribute columns with the independent variables (if present) and (if locations is a formula) the coordinates and time with names, as defined in locations where you want to generate new predictions}
\item{n.neigh}{number of nearest observations that should be used for a \emph{rbfST}
prediction, where nearest is defined in terms of the spatio-temporal locations}
\item{func}{spatio-temporal radial basis function; model type: "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
\item{progress}{
whether a progress bar shall be printed for spatio-temporal radial basis functions; default=TRUE
}
}
\details{
rbf.st function generates individual spatio-temporal predictions from gaussian (GAU), exponential (EXPON), trigonometric (TRI) thin plate spline (TPS), completely regularized spline (CRS), spline with tension (ST), 
inverse multiquadratic (IM), and multiquadratic (M) functions
}
\value{
Attributes columns contain coordinates, time, predictions, and the variance
column contains NA's
}

\references{
Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 
}


\examples{
\dontrun{
# considering 10 principal coordinates (constructed from a distance-based regression model)
data(croatia.temp)
data(croatiadb)

# prediction case: one point
point <- data.frame(670863,5043464,5,170,200,15.7,3)
names(point) <- c("x","y","t","dem","dsea","twi","est")

croatia.temp[,7] <- as.factor(croatia.temp[,7])
dblm1 <- dblm(data=croatia.temp,y=croatiadb$MTEMP)         
newdata1 <- t(cp.xnews(newdata=point,eigenvalues=dblm1$ev, data=croatia.temp,trend=dblm1$cp))
colnames(newdata1) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
newdata1 <- data.frame(point[,1:3],newdata1)

data(croatiadb)
coordinates(croatiadb) <- ~x+y
coordinates(newdata1) <- ~x+y
rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=croatiadb, eta=0.010076, rho=0.00004, 
       newdata=newdata1, n.neigh=60, func="TPS")

# prediction case: a grid of points Croatia (month july)
data(croatia.grid7cp)
coordinates(croatia.grid7cp) <- ~x+y
rbf.t <- rbfST(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, croatiadb, eta=0.01076, rho=0.00004, 
                newdata=croatia.grid7cp, n.neigh=30, func="TPS")                  
coordinates(rbf.t) <- c("x", "y")
gridded(rbf.t) <- TRUE

# show prediction map
spplot(rbf.t["var1.pred"], cuts=30, col.regions=bpy.colors(40), main = "Earth's average 
       temperature TPS map\n (july month)", key.space=list(space="right", cex=0.8))
}
}
\keyword{ spatial }
