\name{generateArtificialLongData}
\alias{gald}
\alias{generateArtificialLongData}


\title{~ Function: generateArtificialLongData (or gald) ~}

\description{
  This function builp up an artificial longitudinal data set (single
  variable-trajectory) an turn it
  into an object of class \code{\linkS4class{ClusterLongData}}.
}
\usage{
gald(nbEachClusters=50,time=0:10,varNames="V",
    meanTrajectories=list(function(t){0},function(t){t},
       function(t){10-t},function(t){-0.4*t^2+4*t}),
    personalVariation=function(t){rnorm(1,0,2)},
    residualVariation=function(t){rnorm(1,0,2)},
    decimal=2,percentOfMissing=0)


generateArtificialLongData(nbEachClusters=50,time=0:10,varNames="V",
    meanTrajectories=list(function(t){0},function(t){t},
       function(t){10-t},function(t){-0.4*t^2+4*t}),
    personalVariation=function(t){rnorm(1,0,2)},
    residualVariation=function(t){rnorm(1,0,2)},
    decimal=2,percentOfMissing=0)
}

\arguments{
  \item{nbEachClusters}{[numeric] or [vector(numeric)]: number of trajectories that each
    cluster must contain. If a single
    number is given, it is duplicated for all groups.}
  \item{time}{[vector(numeric)]: time at which measures are made.}
  \item{varNames}{[character]: name of the variable.}
  \item{meanTrajectories}{[list(function)]: lists the functions
    define the average trajectories of each cluster.}
  \item{personalVariation}{[function] or [list(function)]: lists the functions
    defining the personnal variation between an individual and the mean
    trajectories of its cluster. Note that these function should be
    constant function (the personal variation can not evolve with time). If
    a single function is given, it is duplicated for all groups (see detail).}
  \item{residualVariation}{[function] or [list(function)]: lists the functions
    generating the noise of each trajectory within its own cluster. If
    a single function is given, it is duplicated for all groups (see detail).}
  \item{decimal}{[numeric]: number of decimals used to round up values.}
  \item{percentOfMissing}{[numeric]: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups. The missing values are
    Missing Completly At Random (MCAR).}
}

\details{
  \code{generateArtificialLongData} (\code{gald} in short) is a
  function that contruct a set of artificial longitudinal data.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time. These functions (one per group) are given via the argument \code{meanTrajectories}.

  Within a group, the individual undergoes individal variations. Individual variations are given via the argument \code{residualVariation}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly (MCAR) striking the
  data thanks to \code{percentOfMissing}.
}

\value{
  An object of class \code{\linkS4class{ClusterLongData}}.
}

\seealso{
  \code{\linkS4class{ClusterLongData}}, \code{\link{clusterLongData}}
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



\examples{
par(ask=TRUE)


#####################
### Default example

(ex1 <- generateArtificialLongData())
plot(ex1)
plot(ex1,parTraj=parTRAJ(col=rep(2:5,each=50)))


#####################
### Three diverging lines

ex2 <- generateArtificialLongData(meanTrajectories=list(function(t)0,function(t)-t,function(t)t))
plot(ex2,parTraj=parTRAJ(col=rep(2:4,each=50)))


#####################
### Three diverging lines with high variance, unbalance groups and missing value

ex3 <- generateArtificialLongData(
   meanTrajectories=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(100,30,10),
   residualVariation=function(t){rnorm(1,0,3)},
   percentOfMissing=c(0.25,0.5,0.25)
)
part3 <- partition(rep(1:3,c(100,30,10)))
plot(ex3,parTraj=parTRAJ(col=rep(2:4,c(100,30,10))))


#####################
### Four strange functions

ex4 <- generateArtificialLongData(
    nbEachClusters=c(300,200,100,100),
    meanTrajectories=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},
       function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    residualVariation=function(t){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0.3)
plot(ex4,parTraj=parTRAJ(col=rep(2:5,c(300,200,100,100))))


#####################
### To get only longData (if you want some artificial longData
###    to deal with another algorithm), use the getteur ["traj"]

ex5 <- gald(nbEachCluster=3,time=1:3)
ex5["traj"]

par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
