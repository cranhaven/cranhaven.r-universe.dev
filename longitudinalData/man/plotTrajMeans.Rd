\name{plotTrajMeans,LongData}
\Rdversion{1.1}
\alias{plotTrajMeans}
\alias{plotTrajMeans,LongData}
\alias{plotTrajMeans,LongData-method}
\alias{plotTrajMeans,LongData,missing-method}
\alias{plotTrajMeans,LongData,Partition-method}
%\alias{plotTrajMeans,LongData3d}
%\alias{plotTrajMeans,LongData3d-method}
%\alias{plotTrajMeans,LongData3d,missing-method}
%\alias{plotTrajMeans,LongData3d,Partition-method}

\title{ ~ Function: plotTrajMeans for LongData ~ }
\description{
  Plot the \code{\linkS4class{LongData}} or \code{\linkS4class{LongData3d}} optionnaly relatively
  to a \code{\linkS4class{Partition}}. For joint trajectories, one
  graphe for each variable trajectory is displayed.
}

\usage{
plotTrajMeans(x, y, parTraj=parTRAJ(), parMean=parMEAN(),...)
}

\arguments{
  \item{x}{\code{[LongData]} or \code{[LongData3d]}: Object containing the trajectories
    to plot.}
  \item{y}{\code{[numeric]}: \code{Partition} that will be use to plot
    the object. If \code{y} is missing, a
    \code{Partition} with a single cluster is considered.}
  \item{parTraj}{\code{[ParLongData]}: Set the graphical parameters
    used to plot the trajectories. See
  \code{\linkS4class{ParLongData}} and examples
    for details.}
  \item{parMean}{\code{[ParLongData]}: Set the graphical parameters
    used to plot the mean trajectories of each clusters
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    and examples for details.}
%  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
%    be time consumming. This parameters fixe the maximum number of
%    trajectories (randomly chosen) that will be drawn.}
  \item{...}{Arguments to be passed to methods, such as graphical parameters.}
}

\details{
  Plot either a \code{\linkS4class{LongData}},
  or each variable of a \code{\linkS4class{LongData3d}}
  optionnaly according to the \code{Partition} define by \code{y}.

  Graphical option concerning the individual trajectory (col, type, pch
  and xlab) can be change using \code{parTraj}.
  Graphical option concerning the cluster mean trajectory (col, type, pch,
  pchPeriod and cex) can be change using \code{parMean}. For more
  detail on  \code{parTraj} and  \code{parMean}, see object of
  class \code{\linkS4class{ParLongData}}.
}

\seealso{\code{\linkS4class{LongData}}, \code{\linkS4class{LongData3d}}, \code{\link{plotTrajMeans3d}}.}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


\examples{
##################
### Construction of the data
data(artificialLongData)
ld <- longData(artificialJointLongData)
part <- partition(rep(1:3,each=50))

### Basic plotting
plotTrajMeans(ld)
plotTrajMeans(ld,part,xlab="Time")


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plotTrajMeans(ld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plotTrajMeans(ld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plotTrajMeans(ld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plotTrajMeans(ld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plotTrajMeans(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plotTrajMeans(ld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))


##################
### single variable trajectory

data(artificialLongData)
ld2 <- longData(artificialLongData)
part2 <- partition(rep(1:4,each=50))
plotTrajMeans(ld2)
plotTrajMeans(ld2,part2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{aplot}