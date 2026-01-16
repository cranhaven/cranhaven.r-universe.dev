\name{plot,ClusterLongData3d}
\alias{plot}
\alias{plot,ClusterLongData3d}
\alias{plot,ClusterLongData3d,ANY-method}
\alias{plot,ClusterLongData3d,missing-method}
\alias{plot,ClusterLongData3d,numeric-method}
%\alias{plot,ClusterLongData3d,Partition-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plot for ClusterLongData3d ~ }
\description{
  \code{plot} the trajectories of an object
  %\code{\linkS4class{ClusterLongData3d}} relativly to a
  %\code{\linkS4class{Partition}}. 
  \code{ClusterLongData} relatively to a \code{Partition}.
  One graph for each variable is displayed.
}
\usage{
\S4method{plot}{ClusterLongData3d,ANY}(x,y=NA,parTraj=parTRAJ(),parMean=parMEAN(),
   addLegend=TRUE,adjustLegend=-0.05,toPlot="both",nbCriterion=1000,...)

%,toPlot=c("both"),criterion=x["criterionActif"],nbCriterion=100,standardized = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[ClusterLongData3d]}: Object containing the joint-trajectories to plot.}

  \item{y}{\code{[numeric]} or \code{[vector2(numeric)]}: Give the \code{Partition}
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
    parameters of the mean trajectories (only when y is non missing). Fields that can be changes are
    'type','col','pch','pchPeriod' and 'cex'. See
    \code{ParLongData} in package \code{longitudinalData} for details.}

%  \item{parWin}{\code{[parWindin]}: Set the graphical display of
%    the windows. See \code{\linkS4class{ParWindows}} for details.}

%  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
%    be time consumming. This parameters fixe the maximum number of
%    trajectories (randomly chosen) that will be drawn.}

  \item{toPlot}{\code{[character]}: either 'traj' for plotting
    trajectories alone, 'criterion' for plotting criterion alone,
    'both' for plotting both or 'none' for not display anything.}

  \item{nbCriterion}{\code{[numeric]}: if a single criterion is given to
    \code{criterion} (and thus is displayed for 'all' the
    \code{Partition}), this slot alows to fix a limit on the number of
    points that will be display.}

  \item{addLegend}{\code{[logical]}: should the legend be displayed?}

  \item{adjustLegend}{\code{[numeric]}: fix the hight of the legend}

  \item{...}{Some other parameters can be passed to the method (like
    "xlab" or "ylab".}

%  \item{xlab}{\code{[character]}: label of the x axe}
%  \item{ylab}{\code{[vector(character)]}: labels of the y axe, one for
 %   each variables.}

  %\item{...}{Arguments to be passed to methods, such as graphical parameters.}
}

\details{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterLongData3d}} relativly
  %to the 'best' \code{\linkS4class{Partition}}, or to the
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
  Overview: \code{\link{kml3d-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData3d}} \cr
  Plot : \code{\link[kml]{plotTraj}}, 
         \code{\link[longitudinalData]{plotCriterion}}
}

\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

##################
### Construction of the data

myCld <- gald3d()

### Basic plotting
plot(myCld)

##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
kml3d(myCld,2:7,2)
plot(myCld,2,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(myCld,3,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(myCld,4,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(myCld,5,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(myCld,6,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(myCld,7,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))

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


