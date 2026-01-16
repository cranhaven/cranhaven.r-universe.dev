\name{generateArtificialLongData3d}
\alias{gald3d}
\alias{generateArtificialLongData3d}


\title{~ Function: generateArtificialLongData3d (or gald3d) ~}

\description{
  This function builp up an artificial longitudinal data set (joint
  trajectories) an turn them
  into an object of class \code{ClusterLongData} (from package \code{longitudinalData}).
}
\usage{
gald3d(nbEachClusters=50,time=0:10,varNames=c("V","T"),
    meanTrajectories=list(function(t){c(0,0)},
       function(t){c(10,10)},function(t){c(10-t,10-t)}),
    personalVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    residualVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    decimal=2,percentOfMissing=0)

generateArtificialLongData3d(nbEachClusters=50,time=0:10,varNames=c("V","T"),
    meanTrajectories=list(function(t){c(0,0)},
       function(t){c(10,10)},function(t){c(10-t,10-t)}),
    personalVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    residualVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    decimal=2,percentOfMissing=0)
}

\arguments{
  \item{nbEachClusters}{\code{[vector(numeric)]}: number of trajectories that each
    cluster must contain. If a single
    number is given, it is duplicated for all groups.}

  \item{time}{\code{[vector(numeric)]}: time at which measures are made.}
  \item{varNames}{\code{[vector(character)]}: names of the variables.}
  \item{meanTrajectories}{\code{[list(function)]}: lists the functions
    that define the average trajectories of each cluster.
    Each functions shall return a vector containing one value for each variable of \code{varNames}.}

  \item{personalVariation}{\code{[function]} or \code{[list(function)]}: lists the functions
    defining the personnal variation between an individual and the mean
    trajectories of its cluster. Note that these function should be
    constant function (the personal variation can not evolve with time). If
    a single function is given, it is duplicated for all groups (see detail).}

  \item{residualVariation}{\code{[function]} or
    \code{[list(function)]}: lists the functions
    generating the noise of each trajectory within its own cluster.
    Each functions shall return a vector containing one value for each variable of \code{varNames}.
    If a single function is given, it is duplicated for all groups.}

  \item{decimal}{\code{[numeric]}: number of decimals used to round up values.}
  \item{percentOfMissing}{\code{[numeric]}: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups. The missing values are
    Missing Completly At Random (MCAR).}
}

\details{
  \code{generateArtificialLongData3d} (\code{gald3d} in short) is a
  function that contruct a set of artificial joint longitudinal data.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time.
  These functions (one per group) are given via the argument \code{meanTrajectories}.

  Within a group, the individual undergoes individal
  variations. Individual variations are given via the argument \code{residualVariation}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly (MCAR) striking the
  data thanks to \code{percentOfMissing}.
}

\value{
  Object of class \code{ClusterLongData} (see package \code{longitudinalData}).
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

\seealso{
  \code{\linkS4class{ClusterLongData3d}}, \code{\link{clusterLongData3d}}, \code{\link[kml]{generateArtificialLongData}}
}

\examples{
#####################
### Default example

ex1 <- generateArtificialLongData3d()
plot3d(ex1,parTraj=parTRAJ(col=rep(2:4,each=50)))


#####################
### 4 lines with unbalanced groups

ex2 <- generateArtificialLongData3d(
  nbEachClusters=c(5,10,20,40),
  meanTrajectories=list(
     function(t)c(t,t^3/100),
     function(t)c(0,t),
     function(t)c(t,t),
     function(t)c(0,t^3/100)
  ),
  residualVariation = function(t){c(rnorm(1,0,1),rnorm(1,0,1))}
)

plot3d(ex2,parTraj=parTRAJ(col=rep(1:4,time=c(5,10,20,40))))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
