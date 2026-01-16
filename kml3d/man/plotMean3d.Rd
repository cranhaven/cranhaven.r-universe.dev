\name{plotMeans3d,ClusterLongData3d}
\alias{plotMeans3d}
%\alias{plotMeans3d,ClusterLongData3d-method}
%\alias{plotMeans3d,ClusterLongData3d,missing-method}
\alias{plotMeans3d,ClusterLongData3d,numeric-method}
%\alias{plotMeans3d,ClusterLongData3d,Partition-method}

\title{ ~ Function: plotMeans3d for ClusterLongData3d ~ }
\description{
  Plot the means of two variables of a \code{\linkS4class{ClusterLongData3d}} object in 3D
  relatively to a \code{Partition} (from package in \code{longitudinalData}).
}

\usage{
%\S4method{plotMeans3d}{ClusterLongData3d,missing}(x,y,varY=1,varZ=2,
%   parTraj=parTRAJ(),parMean=parMEAN(),nbSample=200,...)
\S4method{plotMeans3d}{ClusterLongData3d,numeric}(x,y,varY=1,varZ=2,
   parTraj=parTRAJ(type="n"),parMean=parMEAN(),...)
}

\arguments{
  \item{x}{\code{[ClusterLongData3d]}: Object containing the trajectories to plot.}
  \item{y}{\code{[numeric]} or \code{[vector2(numeric)]}: Define the \code{Partition} P that will be use to plot
    the object. \code{P} is a \code{Partition} hold in the field \code{c2},
    \code{c3}, ... \code{c26}. If \code{y=c(a,b)}, then \code{P} is
    the \code{Partition} number \code{b} with \code{a} clusters. If \code{y=a},
    then \code{P} is the partition number \code{1} with \code{a}
    clusters.}
  \item{varY}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the first variable to display. 1 by default.}
  \item{varZ}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the second variable to display. 2 by default.}
  \item{parTraj}{\code{[ParLongData]}: Set the graphical parameters
    used to plot the trajectories of the \code{ClusterLongData3d}. 
    See \code{ParLongData} in package \code{longitudinalData} for details.}
  \item{parMean}{\code{[ParLongData]}:  Set the graphical parameters
    used to plot the mean trajectories of each clusters \code{ClusterLongData3d}
    (only when \code{y} is non missing). 
    See \code{ParLongData} in package \code{longitudinalData} for details.}
%  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
%    be time consumming. This parameters fixe the maximum numbre of
%    trajectories (randomly chosen) that will be drawn.}
  \item{...}{Arguments to be passed to methods, such as graphical parameters.}
}

\details{
  Plot two variables of a \code{\linkS4class{ClusterLongData3d}} object in 3D. It
  use the \code{\link[rgl:rgl]{rgl}} library. The user can make the
  graphical representation turn using its mouse.
}

\seealso{\code{\linkS4class{ClusterLongData3d}}}


\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

##################
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
h <- function(id,t)(id\%\%4-0.5)*(20-t)
myCld <- clusterLongData3d(array(cbind(outer(id2,time,f),outer(id2,time,g),
   outer(id2,time,h))+rnorm(120*8*3,0,3),dim=c(120,8,3)))
kml3d(myCld,3:4,2)

### Basic plot
plotMeans3d(myCld,3)

### plotMeans3d, variable 1 and 3
plotMeans3d(myCld,4,varZ=3)
plotMeans3d(myCld,3,parTraj=parTRAJ(col="red"))

### Go back to current dir
setwd(wd)
}

\keyword{package}
\keyword{ts}
\keyword{aplot}
