\name{calculTrajMean}
\Rdversion{1.1}
\alias{calculTrajMean}
\alias{calculTrajMeanC}
\title{
~ Functions: calculTrajMean & calculTrajMeanC ~
}
\description{
Given some longitudinal data and a cluster affectation,
  \code{calculTrajMean} and \code{calculTrajMeanC} compute the mean trajectories of each cluster.
}
\usage{
calculTrajMean(traj, clust, centerMethod = function(x){mean(x, na.rm =TRUE)})
calculTrajMeanC(traj, clust)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[matrix(numeric)]}: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{clust}{\code{[vector(numeric)]}: affectation of each
    individual.}
  \item{centerMethod}{\code{[trajectory <- function(matrix(numeric))]}:
    function that compute the mean trajectory of a group of trajectories.}
}

\details{
  Given a vector of affectation to a cluster, the function
  \code{calculTrajMean} compute the "central" trajectory of each
  clusters. The "center" can be define using the argument \code{centerMethod}.
  \code{calculTrajMeanC} does the same but
  assume that the center definition is the classic "mean".
  \code{calculTrajMeanC} is writen in C (and is therefor much faster).

  \code{affectIndiv} used with \code{\link{calculTrajMean}} simulates one k-means step.
}

\value{
  A matrix with k line and t column containing k clusters centers. Each
  line is a center, each column is a time measurement.
}






\examples{
#######################
### calculMean

### Some trajectories
traj <- gald()["traj"]

### A cluster affectation
clust <- initializePartition(3,200,"randomAll")

### Computation of the cluster's centers
system.time(centers <- calculTrajMean(traj,clust))
system.time(centers <- calculTrajMeanC(traj,clust))
}

