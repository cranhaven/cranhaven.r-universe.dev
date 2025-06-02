\name{seqPtsOptNet}
\alias{seqPtsOptNet}

\title{
Design of optimal sampling networks through the sequential points method
}
\description{
Search for the optimum location of one additional point to be added to an initial network, minimizing the average standard error of kriging through a genetic algorithm. It takes, as input for the optimization, the minimum and maximum values of the coordinates that enclose the study area. This function uses previous samples information to direct additional sampling. The location of the new point is searched randomly. 
}
\usage{
seqPtsOptNet(formula, loc=NULL, data, fitmodel, BLUE=FALSE, n=1, prevSeqs=NULL, 
popSize, generations, xmin, ymin, xmax, ymax, plotMap=FALSE, spMap=NULL, ...)
}

\arguments{
  \item{formula}{
formula that defines the interpolation method to be used. In this parameter, a dependent variable is defined as a linear model of independent variables. Suppose the dependent variable has name
  \code{z}, for ordinary and simple kriging use the formula \code{z~1};
  for simple kriging also define \code{beta}; for universal
  kriging, suppose \code{z} is linearly dependent on \code{x} and \code{y},
  use the formula \code{z~x+y}. See the \code{gstat} package for details
}
  \item{loc}{
object of class Spatial, or (deprecated) formula that defines the spatial data locations (coordinates) such as ~x+y; see the \code{gstat} package for details
}
  \item{data}{
data frame containing the dependent variable, independent variables, and coordinates; see the \code{gstat} package for details
}
  \item{fitmodel}{
variogram model of dependent variable (or its residuals), defined by a call to \code{vgm} or \code{fit.variogram}; see the \code{gstat} package for details
}
  \item{BLUE}{logical; if TRUE return the BLUE trend estimates only, if FALSE return the BLUP predictions (kriging); see \code{\link[gstat]{predict.gstat}} in the \code{gstat} package for details
}
  \item{n}{
by default, set to 1 for the sequential points method
}
  \item{prevSeqs}{
if NULL, the function finds the first optimum sequential point. Otherwise, an object of class \code{SpatialPoints} containing optimum points previously found must be provided
}
  \item{popSize}{
population size; see the \code{genalg} package for details
}
  \item{generations}{
number of iterations. Usually, hundreds or thousands are required. See the \code{genalg} package for details
}
  \item{xmin}{
minimum \emph{x}-coordinate of the study area
}
  \item{ymin}{
minimum \emph{y}-coordinate of the study area
}
  \item{xmax}{
maximum \emph{x}-coordinate of the study area
}
  \item{ymax}{
maximum \emph{y}-coordinate of the study area
}
  \item{plotMap}{
logical; if TRUE, the optimized spatial locations of additional points are plotted
}
  \item{spMap}{
an object of class Spatial; it must be provided if plotMap is set to TRUE
}
  \item{...}{
other arguments to be passed to \code{gstat} or \code{rbga}
}
}
\value{
an object of class rbga containing the population and the evaluation of the objective function for each chromosome in the last generation, the best and mean evaluation value in each generation, and additional information
}
\references{
Santacruz, A., Rubiano, Y., Melo, C., 2014. \emph{Evolutionary optimization of spatial sampling networks designed for the monitoring of soil carbon}. In: Hartemink, A., McSweeney, K. (Eds.). \emph{Soil Carbon. Series: Progress in Soil Science}. (pp. 77-84). Springer. \href{https://link.springer.com/chapter/10.1007/978-3-319-04084-4_8}{[link]}

Santacruz, A., 2011. \emph{Evolutionary optimization of spatial sampling networks. An application of genetic algorithms and geostatistics for the monitoring of soil organic carbon}. Editorial Academica Espanola. 183 p. ISBN: 978-3-8454-9815-7 (In Spanish)

Delmelle, E., 2005. \emph{Optimization of second-phase spatial sampling using auxiliary information}. Ph.D. Thesis, Dept. Geography, State University of New York, Buffalo, NY.
}
\seealso{
See \code{\link[genalg]{rbga}} in the \code{genalg} package and \code{\link[gstat]{krige}} in the \code{gstat} package
}
\examples{
\dontrun{
## Load data
data(COSha10)
data(COSha10map)
data(lalib)

## Calculate the sample variogram for data, generate the variogram model and  
## fit ranges and/or sills from the variogram model to the sample variogram
ve <- variogram(CorT~ 1, loc=~x+y, data=COSha10, width = 230.3647)
PSI <- 0.0005346756; RAN <- 1012.6411; NUG <- 0.0005137079
m.esf <- vgm(PSI, "Sph", RAN, NUG)
(m.f.esf <- fit.variogram(ve, m.esf))

## Optimize the location of the first additional point 
## Only 15 generations are evaluated in this example (using ordinary kriging)
## Users can visualize how the location of the additional point is optimized 
## if plotMap is set to TRUE
old.par <- par(no.readonly = TRUE)
par(ask=FALSE)
optpt <- seqPtsOptNet(CorT~ 1, loc=~x+y, COSha10, m.f.esf, popSize=30, 
    generations=15, xmin=bbox(lalib)[1], ymin=bbox(lalib)[2], xmax=bbox(lalib)[3], 
    ymax=bbox(lalib)[4], plotMap=TRUE, spMap=lalib)
par(old.par)

## Summary of the genetic algorithm results
summary(optpt, echo=TRUE)

## Graph of best and mean evaluation value of the objective function 
## (average standard error) along generations
plot(optpt)

## Find and plot the best set of additional points (best chromosome) in   
## the population in the last generation
(bnet1 <- bestnet(optpt))
l1 = list("sp.polygons", lalib)
l2 = list("sp.points", bnet1, col="green", pch="*", cex=5)
spplot(COSha10map, "var1.pred", main="Location of the optimized point", 
    col.regions=bpy.colors(100), scales = list(draw =TRUE), xlab ="East (m)", 
    ylab = "North (m)", sp.layout=list(l1,l2))

## Average standard error of the optimized sequential point
min(optpt$evaluations)

## Optimize the location of the second sequential point, taking into account 
## the first one
plot(lalib)
old.par <- par(no.readonly = TRUE)
par(ask=FALSE)
optpt2 <- seqPtsOptNet(CorT~ 1, loc=~x+y, COSha10, m.f.esf, prevSeqs=bnet1, 
    popSize=30, generations=15, xmin=bbox(lalib)[1], ymin=bbox(lalib)[2], 
    xmax=bbox(lalib)[3], ymax=bbox(lalib)[4], plotMap=TRUE, spMap=lalib)
par(old.par)

## Find the second optimal sequential point and use it, along with the first
## one, to find another optimal sequential point, and so on iteratively  

bnet2 <- bestnet(optpt2)
bnet <- rbind(bnet1, bnet2)

old.par <- par(no.readonly = TRUE)
par(ask=FALSE)
optpt3 <- seqPtsOptNet(CorT~ 1, loc=~x+y, COSha10, m.f.esf, prevSeqs=bnet,
    popSize=30, generations=25, xmin=bbox(lalib)[1], ymin=bbox(lalib)[2], 
    xmax=bbox(lalib)[3], ymax=bbox(lalib)[4], plotMap=TRUE, spMap=lalib)
par(old.par)
}

## Multivariate prediction is also enabled:
\dontrun{
ve <- variogram(CorT~ DA10, loc=~x+y, data=COSha10, width = 230.3647)
(m.f.esf <- fit.variogram(ve, m.esf))

optptMP <- seqPtsOptNet(CorT~ DA10, loc=~x+y, COSha10, m.f.esf, popSize=30, 
    generations=25, xmin=bbox(lalib)[1], ymin=bbox(lalib)[2], xmax=bbox(lalib)[3], 
    ymax=bbox(lalib)[4], plotMap=TRUE, spMap=lalib)
}
}

\keyword{ spatial }
