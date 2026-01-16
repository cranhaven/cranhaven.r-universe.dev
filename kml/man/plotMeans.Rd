\name{plotMeans,ClusterLongData}
\alias{plotMeans}
%\alias{plotMeans-method}
\alias{plotMeans,ClusterLongData}
\alias{plotMeans,ClusterLongData,ANY-method}
\alias{plotMeans,ClusterLongData,missing-method}
\alias{plotMeans,ClusterLongData,numeric-method}
\alias{plotMeans,ClusterLongData,Partition-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plotMeans for ClusterLongData ~ }
\description{
  \code{plotMeans} plots the means' trajectories of an object 
  %\code{\linkS4class{ClusterLongData}} relatively  to a \code{\linkS4class{Partition}}.
  \code{ClusterLongData} relatively to a \code{Partition}.
}
\usage{
\S4method{plotMeans}{ClusterLongData,ANY}(x,y,parMean=parMEAN(),
   parWin=windowsCut(x['nbVar'],addLegend=TRUE),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[ClusterLongData]}: Object containing the trajectories to plotMeans.}

  \item{y}{\code{[numeric]} or \code{[vector(numeric)]}: Give the \code{Partition}
    to represent. If \code{y} is a number,
    the first \code{Partition} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Partition} of the sublist \code{c-y[1]} is selected (so \code{y=c(2,3)} select the partition
    with 2 clusters, the third one).}

  \item{parMean}{\code{[ParLongData]}: Specification of the plotting
    parameters of the mean trajectories. Fields that can be changes are
    'type','col','pch','pchPeriod' and 'cex'. See
    \code{ParLongData} in package \code{longitudinalData} for details.}

  \item{parWin}{\code{[parWindows]}: Set the graphical display of
    the windows. See \code{ParWindows} in package \code{longitudinalData} for details.}

  \item{...}{Some other parameters can be passed to the method.}
}

\details{
  \code{plotMeans} plots the means' trajectories of an object \code{\linkS4class{ClusterLongData}} relativly
  to the 'best' \code{Partition}, or to the
  \code{Partition} define by \code{y}.

  Graphical option (col, type, pch,
  pchPeriod and cex) can be change using \code{parMean}. For more
  detail on  \code{parTraj} and  \code{parMean}, see object of
  class \code{ParLongData} in package \code{longitudinalData}.

}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}} \cr
  PlotMeans : \code{\link[kml:plotMeans]{plotMeans: overview}}, \code{\link[longitudinalData]{plotCriterion}}
}

\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

##################
### Construction of the data

ld <- gald(10)
kml(ld,3:4,2)

### Basic plotMeansting
plotMeans(ld,3)

### Go back to current dir
setwd(wd)
}

\keyword{dplotMeans}    % Computations Related to PlotMeansting
\keyword{iplotMeans}    % Interacting with PlotMeanss
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


