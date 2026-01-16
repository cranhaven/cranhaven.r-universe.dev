\name{plot,ClusterLongData}
\alias{plot}
%\alias{plot-method}
\alias{plot,ClusterLongData}
\alias{plot,ClusterLongData,ANY-method}
\alias{plot,ClusterLongData,missing-method}
\alias{plot,ClusterLongData,numeric-method}
\alias{plot,ClusterLongData,Partition-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plot for ClusterLongData ~ }
\description{
  \code{plot} the trajectories of an object 
  %\code{\linkS4class{ClusterLongData}} relatively to a \code{\linkS4class{Partition}}.
  \code{ClusterLongData} relatively to a \code{Partition}.
}
\usage{
\S4method{plot}{ClusterLongData,ANY}(x,y=NA,parTraj=parTRAJ(),parMean=parMEAN(),
   addLegend=TRUE, adjustLegend=-0.12,toPlot="both",criterion=x["criterionActif"],
   nbCriterion=1000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[ClusterLongData]}: Object containing the trajectories to plot.}

  \item{y}{\code{[numeric]} or \code{[vector(numeric)]}: Give the \code{Partition}
    to represent. If \code{y} is missing, the \code{Partition} with
    the highest quality criterion (the actif one) is selected. If \code{y} is a number,
    the first \code{Partition} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Partition} of the sublist \code{c-y[1]} is selected.}

  \item{parTraj}{\code{[ParLongData]}: Specification of the plotting
    parameters of the individual trajectories. Fields that can be changes are
    'type','col','pch','xlab' and 'ylab'. In addition to the standard
    possible values, the option \code{col="clusters"} can be use to color the individual
    trajectories according to their clusters (exemple:
    \code{parTraj=parTRAJ(type="o",col="clusters")}). See
    \code{ParLongData} in package \code{longitudinalData} for details.}

  \item{parMean}{\code{[ParLongData]}: Specification of the plotting
    parameters of the mean trajectories. Fields that can be changes are
    'type','col','pch','pchPeriod' and 'cex'. See
    \code{ParLongData} in package \code{longitudinalData} for details.}

  \item{toPlot}{\code{[character]}: either 'traj' for plotting
    trajectories alone, 'criterion' for plotting criterion alone,
    'both' for plotting both or 'none' for not display anything (faster).}

  \item{criterion}{\code{[character]} or \code{[vector(character)]}:
    criterion to display (only if 'toPlot' is 'criterion' or 'both'). If
    a single criterion is given, it will be display for all the
    \code{Partition}. If several criterion are used, they will be display for
    the first \code{Partition} for each clusters' numbers.}

  \item{nbCriterion}{\code{[numeric]}: if a single criterion is given to
    \code{criterion} (and thus is displayed for 'all' the
    \code{Partition}), this slot alows to fix a limit on the number of
    points that will be display.}

  \item{addLegend}{\code{[logical]}: should the legend be displayed?}

  \item{adjustLegend}{\code{[numeric]}: fix the hight of the legend}

  \item{...}{Some other parameters can be passed to the method (like
    "xlab" or "ylab".}
}

\details{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterLongData}} relativly
  to the 'best' \code{Partition}, or to the
  \code{Partition} define by \code{y}.

  Graphical option concerning the individual trajectory (col, type, pch
  and xlab) can be change using \code{parTraj}.
  Graphical option concerning the cluster mean trajectory (col, type, pch,
  pchPeriod and cex) can be change using \code{parMean}. For more
  detail on  \code{parTraj} and  \code{parMean}, see object of
  class \code{ParLongData} in package \code{longitudinalData}.

}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}} \cr
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[longitudinalData]{plotCriterion}}
}

\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

##################
### Construction of the data

ld <- gald()

### Basic plotting
plot(ld)


##################
### Changing graphical parameters 'par'

kml(ld,3:4,1)

### No letters on the mean trajectories
plot(ld,3,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(ld,4,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(ld,3,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(ld,4,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(ld,3,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(ld,4,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))

### Go back to current dir
setwd(wd)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


