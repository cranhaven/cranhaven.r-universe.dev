\name{plotTraj,ClusterLongData}
\alias{plotTraj}
%\alias{plotTraj-method}
%\alias{plotTraj,ClusterLongData}
\alias{plotTraj,ClusterLongData,ANY-method}
%\alias{plotTraj,ClusterLongData,missing-method}
\alias{plotTraj,ClusterLongData,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plotTraj for ClusterLongData ~ }
\description{
  \code{plotTraj} plot the trajectories of an object 
  %\code{\linkS4class{ClusterLongData}} relatively to a \code{\linkS4class{Partition}}.
  \code{ClusterLongData} relatively to a \code{Partition}.
}
\usage{
\S4method{plotTraj}{ClusterLongData,ANY}(x,y,parTraj=parTRAJ(col="clusters"),
   parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[ClusterLongData]}: Object containing the trajectories to plotTraj.}

  \item{y}{\code{[numeric]} or \code{[couple(numeric)]}: Give the \code{Partition}
    to represent. If \code{y} is missing, the \code{Partition} with
    the highest quality criterion (the actif one) is selected. If \code{y} is a number,
    the first \code{Partition} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Partition} of the sublist \code{c-y[1]} is selected (so \code{y=c(2,3)} select the partition
    with 2 clusters, the third one).}

  \item{parTraj}{\code{[ParLongData]}: Specification of the plotting
    parameters of the individual trajectories. Fields that can be changes are
    'type','col','pch','xlab' and 'ylab'. In addition to the standard
    possible values, the option \code{col="clusters"} (the default) can be use to color the individual
    trajectories according to their clusters (exemple:
    \code{parTraj=parTRAJ(type="o",col="clusters")}). See
    \code{ParLongData} in package \code{longitudinalData} for details.}

  \item{parWin}{\code{[parWindows]}: Set the graphical display of
    the windows. See \code{ParWindows} in package \code{longitudinalData} for details.}

  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
    be time consumming. This parameters fixe the maximum number of
    trajectories (randomly chosen) that will be drawn.}

  \item{...}{Some other parameters can be passed to the method.}
}

\details{
  \code{plotTraj} the trajectories of an object \code{\linkS4class{ClusterLongData}} relativly
  to the 'best' \code{Partition}, or to the
  \code{Partition} define by \code{y}.

  Graphical option (col, type, pch
  and xlab) can be change using \code{parTraj}.
  For more
  detail on  \code{parTraj}, see object of
  class \code{ParLongData} in package \code{longitudinalData} .

}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}} \cr
  PlotTraj : \code{\link[kml:plotTraj]{plotTraj: overview}}, \code{\link[longitudinalData]{plotCriterion}}
}

\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

##################
### Construction of the data

ld <- gald()
kml(ld,3:4,1)

### Basic plotTrajting
plotTraj(ld,3)

### Go back to current dir
setwd(wd)
}

\keyword{dplotTraj}    % Computations Related to PlotTrajting
\keyword{iplotTraj}    % Interacting with PlotTrajs
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


