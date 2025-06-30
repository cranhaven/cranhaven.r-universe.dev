\name{calculateMukv}
\alias{calculateMukv}
\title{MUKV computation}
\description{
Computes mean universal kriging variance (MUKV) for given geostatistical parameters
}
\usage{
calculateMukv(observations, predGrid, model, formulaString, fun, ...)
}
\arguments{
\item{observations}{ \code{\link[sp]{SpatialPoints}} or  \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} 
     with observation locations and possible covariates }
\item{predGrid}{ \code{\link[sp:Spatial-class]{Spatial}} object with coordinates of 
     prediction locations (usually \code{\link[sp]{SpatialGrid}} 
       or \code{\link[sp:SpatialGrid]{SpatialGridDataFrame}} when 
       independent covariate predictor variables are used) }
\item{model}{ Variogram model:object of class \code{variogramModel}, of the form
                created by \code{\link[gstat:vgm]{vgm}}}
\item{formulaString}{formula that defines the dependent variable as a linear model 
      of independent variables; suppose the dependent variable has name \code{z}, 
      for ordinary and simple kriging use the formula \code{z~1}; 
      for universal kriging, suppose \code{z} is linearly dependent on 
      \code{x} and \code{y}, use the formula \code{z~x+y}. The formulaString defaults
      to \code{"value~1"} if \code{value} is a part of the data set. 
      If not, the first column of the data set is used.}
\item{fun}{alternative penalty function, needs to be a function which can take the
      same arguments as \code{calculateMukv}}
\item{...}{ other arguments to be passed on at lower level functions }
}

\details{
This function computes kriging on the \code{predGrid} with 
\code{\link[gstat:krige]{krige}} function, and averages the kriging variance (MUKV). With covariates, 
the function takes a universal kriging model into account.
}

\value{
MUKV value
}

\author{ S.J. Melles, O. Baume, J. Skoien }
\examples{
\donttest{
# load data:
library(gstat)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
meuse.grid$soil = factor(meuse.grid$soil)

# estimate variogram:
smplvarUK = variogram(zinc~dist+ffreq+soil, meuse)
plot(smplvarUK)
vfitUK = fit.variogram(variogram(zinc~dist+ffreq+soil, meuse), vgm(1, "Exp", 300, 1))
plot(smplvarUK, vfitUK)

calculateMukv(meuse, meuse.grid, vfitUK, zinc~dist+ffreq+soil)

}
}

\keyword{spatial}