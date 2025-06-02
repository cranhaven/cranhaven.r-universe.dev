\name{graph.idw}
\alias{graph.idw}

\title{
Graph that describes the behavior of the optimized \emph{p} smoothing parameter.
}
\description{
Function for plotting the RMSPE for several values of the \emph{p} smoothing parameter with the same dataset. A curve is fitted to the points, and
then the optimal \emph{p} that provides the smallest
RMSPE is determined from the curve, by the \code{\link[stats]{optimize}} function from the \code{stats}
package.
}
\usage{
graph.idw(formula, data, locations, np, p.dmax, P.T=NULL, nmax=Inf, nmin=0, pleg, 
    progress=F, iter, ...)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name \emph{z}, for a \emph{idw} detrended use \emph{z~1}}
\item{data}{SpatialPointsDataFrame: should contain the dependent variable, independent variables, and coordinates.}
\item{locations}{object of class \emph{Spatial}, or (deprecated) formula defines the spatial data locations (coordinates) such as \emph{~x+y}}
\item{np}{number of points, where the idw is calculated}
\item{p.dmax}{maximum value of the range of the \emph{p} parameter that will be evaluated by the \code{\link[stats]{optimize}} function}
\item{P.T}{logical. Print Table (TRUE) or not (FALSE). Default P.T=NULL.}
\item{nmax}{maximum number of nearest observations that should be used for a \emph{idw} prediction, where nearest is defined in terms of the spatial locations. By default, all observations are used}
\item{nmin}{minimum number of nearest observations that should be used for a \emph{idw} prediction, where nearest is defined in terms of the spatial locations. see \code{\link[gstat]{krige}}}
\item{pleg}{the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by \code{\link[grDevices]{xy.coords}}, by default pleg="topright.}
\item{progress}{logical. Use TRUE to see the percentage of progress of the process and FALSE otherwise). Default progress=FALSE.}
\item{iter}{The maximum allowed number of function evaluations.}
\item{...}{further parameters to be passed to the minimization functions \code{\link{optimize}} or \code{\link[minqa]{bobyqa}}, typically arguments of the type control() which control the behavior of the minimization algorithm. See documentation about the selected minimization function for further details.
}
}
\value{
Returns a graph that describes the behavior of the optimized \emph{p} parameter associated with the RMSPE, and a table of values associated with the graph including optimal smoothing \emph{p} parameter, which generates the lowest RMSPE.
}
\references{
Johnston, K., Ver, J., Krivoruchko, K., Lucas, N. 2001. \emph{Using ArcGIS Geostatistical Analysis}. ESRI.
}
\examples{
\dontrun{
data(ariari)
data(ariprec)
# p optimization
gp <- graph.idw(PRECI_TOT~ 1, ~x+y, data=ariprec, np=50, p.dmax=4, nmax=15, 
    nmin=15,pleg = "center", progress=T)
gp
gp$p

library(sp)
library(fields)
plot(ariari)
gridAri <- spsample(ariari,20000,"regular")
plot(gridAri)

idw.p <- idw(PRECI_TOT~ 1, ~ x+y, ariprec, gridAri, nmax=15, nmin=15, idp=2)
pal2 <- colorRampPalette(c("snow3","royalblue1", "blue4"))

# Inverse Distance Interpolations Precipitation Weighted (P = 2)
p1 <- spplot(idw.p[1], col.regions=pal2(100), cuts =60, scales = list(draw =T), 
    xlab ="East (m)", ylab = "North (m)",
       main = "", auto.key = F)

split.screen( rbind(c(0, 1,0,1), c(1,1,0,1)))
split.screen(c(1,2), screen=1)-> ind
screen( ind[1])
p1
screen( ind[2])
image.plot(legend.only=TRUE, legend.width=0.5, col=pal2(100), 
    smallplot=c(0.6,0.68, 0.5,0.75), 
    zlim=c(min(idw.p$var1.pred),max(idw.p$var1.pred)), 
    axis.args = list(cex.axis = 0.7))
close.screen( all=TRUE)

}
}

\keyword{ spatial }
