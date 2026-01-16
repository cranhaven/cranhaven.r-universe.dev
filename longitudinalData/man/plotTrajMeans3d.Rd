\name{plotTrajMeans3d,LongData}
\Rdversion{1.1}
\alias{plotTrajMeans3d}
\alias{plotTrajMeans3d,LongData3d}
\alias{plotTrajMeans3d,LongData3d-method}
\alias{plotTrajMeans3d,LongData3d,Partition-method}
\alias{plotTrajMeans3d,LongData3d,missing-method}
%\alias{plot3d,LongData3d,Partition-method}

\title{ ~ Function: plotTrajMeans3d for LongData3d ~ }
\description{
  Plot two variables of a \code{\linkS4class{LongData3d}} object in 3D, optionnaly
  relatively to a \code{\linkS4class{Partition}}.
}

\usage{
plotTrajMeans3d(x,y,varY=1,varZ=2,
   parTraj=parTRAJ(),parMean=parMEAN(type="n"),...)
}

\arguments{
  \item{x}{\code{[LongData3d]}: Object containing the trajectories to plot.}
  \item{y}{\code{[Partition]}: \code{Partition} that will be use to plot
    the object. If \code{y} is missing, a \code{Partition} with a single
  clusters is considered.}
  \item{varY}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the first variable to display. 1 by default.}
  \item{varZ}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the second variable to display. 2 by default.}
  \item{parTraj}{\code{[parLongData]}: Set the graphical parameters
    used to plot the trajectories of the \code{LongData3d}. See
  \code{\linkS4class{ParLongData}} and examples for details.}
  \item{parMean}{\code{[parLongData]}:  Set the graphical parameters
    used to plot the mean trajectories of each clusters \code{LongData3d}
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    and examples for details.}
%  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
%    be time consumming. This parameters fixe the maximum numbre of
%    trajectories (randomly chosen) that will be drawn.}
  \item{...}{Arguments to be passed to methods, such as graphical parameters.}
}

\details{
  Plot two variables of a \code{\linkS4class{LongData3d}} object in 3D. It
  use the \code{\link[rgl:rgl]{rgl}} library. The user can make the
  graphical representation turn using the mouse.
}



\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{\code{\linkS4class{LongData3d}}}


\examples{

##################
### Construction of the data

\donttest{
time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
h <- function(id,t)(id\%\%4-0.5)*(20-t)
ld <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g),outer(id2,time,h))+
   rnorm(120*8*3,0,3),dim=c(120,8,3)))
part <- partition(rep(1:6,20))

### Basic plotting
plotTrajMeans3d(ld)
plotTrajMeans3d(ld,part)

### Variable 1 and 3, then 2 and 3
plotTrajMeans3d(ld,part)
plotTrajMeans3d(ld,part,varY=3,varZ=2)
plotTrajMeans3d(ld,part,varY=1,varZ=3)

##################
### Changing graphical parameters 'par'

### Color individual according to its clusters (col="clusters")
plotTrajMeans3d(ld,part,parTraj=parTRAJ(col="clusters"))
plotTrajMeans3d(ld,part,parTraj=parTRAJ(col="clusters"),varY=1,varZ=3)

### No mean trajectories (type="n"), only few trajectories
### Color individual according to its clusters (col="clusters")
plotTrajMeans3d(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"),nbSample=10)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{aplot}