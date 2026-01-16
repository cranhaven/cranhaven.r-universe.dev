\name{affectIndiv}
\Rdversion{1.1}
\alias{affectIndiv}
\alias{affectIndivC}

\title{~ Functions: affectIndiv & affectIndivC ~}

\description{
  Given some longitudinal data (trajectories) and k clusters' centers,
  \code{affectIndiv} and \code{affectIndivC}
  affect each individual to the cluster whose centre is the closest.
}

\usage{
affectIndiv(traj, clustersCenter, distance = function(x,y){dist(rbind(x, y))})
affectIndivC(traj, clustersCenter)
}

\arguments{
  \item{traj}{\code{[matrix(numeric)]}: longitudinal data. Each line is an individual,
    each column is a time measurement.}

  \item{clustersCenter}{\code{[matrix(numeric)]}: clusters centre. Each line is a
    cluster center, each column is a time measurement.}

  \item{distance}{\code{[numeric <- function(trajectory,trajectory)]}: use to estimate the distance between an
    individual and a clusters center.}
}

\details{
  Given a matrix of clusters center \code{clustersCenter} (each line is
  a cluster center), the function \code{affectIndiv} affect each
  individual of the matrix \code{traj} to the closest clusters
  (according to \code{distance}). \code{affectIndivC} does the same but
  assume that the distance is the Euclidean
  distance. \code{affectIndivC} is writen in C (and is therefor much faster).

  \code{affectIndiv} used with \code{\link{calculTrajMean}} simulates one k-means step.
}

\value{
  Object of class\code{\link[longitudinalData:Partition-class]{Partition}}.
}


\examples{
#######################
### affectIndiv

### Some trajectories
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
system.time(part <- affectIndiv(traj,center))
system.time(part <- affectIndivC(traj,center))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

