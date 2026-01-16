\name{affectFuzzyIndiv}
\Rdversion{1.1}
\alias{affectFuzzyIndiv}

\title{
~ Function: affectFuzzyIndiv ~
}
\description{
Given some longitudinal data (trajectories) and k cluster's centers, \code{affectFuzzyIndiv}
compute the matrix of individual membership (according to the algorithm
fuzzy k-means).
}
\usage{
affectFuzzyIndiv(traj, clustersCenter, fuzzyfier=1.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[matrix]}: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{clustersCenter}{\code{[matrix]}: cluster's centers. Each line is a
    cluster's center, each column is a time measurement.}

  \item{fuzzyfier}{\code{[numeric]}: value of the fuzzyfier used to
  compute individual's memberships.}

}
\details{
  Given a matrix of clusters center \code{clustersCenter} (each line is
  a cluster center), the function \code{affectFuzzyIndiv} compute for
  each individual and each cluster a "membership".

  \code{affectFuzzyIndiv} used with \code{\link{calculTrajFuzzyMean}}
  simulates one fuzzy k-means step.

}
\value{
  Matrix of the membership. Each line is an individual, column are for clusters.
}


\examples{
#######################
### affectFuzzyIndiv

### Some LongitudinalData
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
affectFuzzyIndiv(traj,center)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

