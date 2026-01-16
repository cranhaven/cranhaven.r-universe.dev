\name{calculTrajMean3d}
\Rdversion{1.1}
\alias{calculTrajMean3d}
\title{
  ~ Function: calculTrajMean3d ~
}
\description{
  Given some joint longitudinal data and a cluster affectation,
  \code{calculTrajMean3d} computes the mean joint-trajectories of each cluster.
}
\usage{
calculTrajMean3d(traj, clust,centerMethod=function(x){mean(x,na.rm=TRUE)})
}

\arguments{
  \item{traj}{\code{[array(numeric)]}: joint longitudinal data. Each line is an individual,
    each column is a time measurement, the third dimension is for variables.}
  \item{clust}{\code{[vector(numeric)]}: affectation of each individual.}
  \item{centerMethod}{\code{[joint-trajectory <- function(array(numeric))]}: function used
    to compute the clusters' centers.}
}

\details{
  Given a vector of affectation to a cluster, the function
  \code{calculTrajMean3d} compute the "central" trajectory of each
  clusters. The "center" can be define using the argument \code{centerMethod}.

  \code{affectIndiv3d} used with \code{\link{calculTrajMean3d}} simulates one k-means step.
}

\value{
  An array of dimension \code{(k,t,v)} with \code{k} number of groups, \code{t} number of
  time mesurement and \code{v} number of variables.
}


\examples{

#######################
### calculTrajMean3d

### Some LongitudinalData3d
traj <- gald3d()["traj"]

### A partition
part <- floor(runif(150,1,5))
plot(clusterLongData3d(traj),parTraj=parTRAJ(col=part+1))

### Clusters center
(center <- calculTrajMean3d(traj,part))


#################
### K-means simulation (4 steps)
plot(clusterLongData3d(traj),parTraj=parTRAJ(col=part+1))
for (i in 1:4){
    part <- affectIndiv3d(traj,center)
    center <- calculTrajMean3d(traj,part)
    plot(clusterLongData3d(traj),parTraj=parTRAJ(col=part+1))
}

}

