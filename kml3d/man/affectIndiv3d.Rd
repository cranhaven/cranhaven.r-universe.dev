\name{affectIndiv3d}
\Rdversion{1.1}
\alias{affectIndiv3d}

\title{
  ~ Function: affectIndiv3d ~
}
\description{
  Given some longitudinal data (trajectories) and k clusters centers, \code{affectIndiv3d}
  affects each individual to the cluster whose center is the closest.
}
\usage{
affectIndiv3d(traj, clustersCenter, distance = dist3d)
}

\arguments{
  \item{traj}{\code{[array(numeric)]}: longitudinal data. Each line is an individual,
    each column is a time measurement, each plan of the third dimension is
    for one variable.}

  \item{clustersCenter}{\code{[array(numeric)]}: cluster center. Each line is a
    cluster centers, each column is a time measurement, each plan of the third dimension is
    for one variable. .}

  \item{distance}{\code{[numeric <- function(joint-trajectory,joint-trajectory)]}: distance between an
    individual and a clusters centre.}
}

\details{
  Given an array of clusters center \code{clustersCenter} (each plan of
  the first dimension is
  a cluster center, that is clusterCenter[2,,] is the second cluster
  center), the function \code{affectIndiv3d} affect each
  individual of the array \code{traj} to the closest clusters,
  according to \code{distance}.

  \code{affectIndiv3d} used with \code{\link{calculTrajMean3d}}
  simulates one k-means 3D step.
}

\value{
  Object of class\code{\link[longitudinalData:Partition-class]{Partition}}.
}


\examples{
#######################
### affectIndiv

### Some trajectories
traj <- gald3d()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),,]

### Affectation of each individual
part <- affectIndiv3d(traj,center)


#################
### K-means simulation (3 steps)
plot(clusterLongData3d(traj),parTraj=parTRAJ(col=part+1))
for (i in 1:3){
    center <- calculTrajMean3d(traj,part)
    part <- affectIndiv3d(traj,center)
    plot(clusterLongData3d(traj),parTraj=parTRAJ(col=part+1))
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

