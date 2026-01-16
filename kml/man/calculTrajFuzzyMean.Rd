\name{calculTrajFuzzyMean}
\Rdversion{1.1}
\alias{calculTrajFuzzyMean}
\title{
~ Function: calculTrajFuzzyMean ~
}
\description{
Given some longitudinal data and a group's membership,
  \code{calculFuzzyMean} computes the mean trajectories of each cluster.
}
\usage{
calculTrajFuzzyMean(traj, fuzzyClust)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[matrix]}: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{fuzzyClust}{\code{[matrix(numeric)]}: membership matrix of each individual.}
}

\details{
  Given a matrix of individual membership, the function
  \code{calculTrajFuzzyMean} compute the mean trajectory of each
  clusters.

  \code{\link{affectFuzzyIndiv}} used with \code{calculTrajFuzzyMean}
  simulates one fuzzy k-means step.
}

\value{
  A matrix with k line and t column containing k clusters centers. Each
  line is a center, each column is a time measurement.
}






\examples{
#######################
### calculTrajFuzzyMean

### Some LongitudinalData
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
membership <- affectFuzzyIndiv(traj,center)

### Computation of the mean's trajectories
calculTrajFuzzyMean(traj,membership)
}

