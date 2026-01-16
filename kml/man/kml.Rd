\name{kml}
\alias{kml}
%\alias{kml-method}
\alias{kml,ClusterLongData-method}

\title{~ Algorithm kml: K-means for Longitidinal data ~}

\description{
  \code{kml} is a implementation of k-means for longitudinal data (or trajectories). This algorithm is able to deal with missing value and
  provides an easy way to re roll the algorithm several times, varying the starting conditions and/or the number of clusters looked for.

  Here is the description of the algorithm. For an overview of the package, see \link{kml-package}.
}

\usage{
kml(object,nbClusters=2:6,nbRedrawing=20,toPlot="none",parAlgo=parALGO())
}

\arguments{
  \item{object}{[ClusterLongData]: contains trajectories to cluster as
    well as previous \code{Partition}.}

  \item{nbClusters}{[vector(numeric)]: Vector containing the number of clusters
    with which \code{kml} must work. By default,
    \code{nbClusters} is \code{2:6} which indicates that \code{kml} must
    search partitions with respectively 2, then 3, ... up to 6
    clusters. Maximum number of cluster is 26.}

  \item{nbRedrawing}{[numeric]: Sets the number of time that k-means
    must be re-run (with different starting conditions) for each number of clusters.}

  \item{toPlot}{\code{[character]}: either 'traj' for plotting
    trajectories alone, 'criterion' for plotting criterion alone,
    'both' for plotting both or 'none' for not display anything (faster).}

  \item{parAlgo}{\code{[\linkS4class{ParKml}]}: parameters used to run
    the algorithm. They can be change using the function
    \code{\link{parKml}}. Option are mainly 'saveFreq', 'maxIt',
    'imputationMethod', 'distance' and 'startingCondition'. See
    \linkS4class{ParKml} for details.}
}

\details{
  \code{kml} works on object of class \code{ClusterLongData}.
  For each number included in \code{nbClusters}, \code{kml} computes a
  \code{Partition} then stores it in the field
  \code{cX} of the object \code{ClusterLongData} according to the number
  of clusters 'X'.   The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a \code{Partition} has been found, it is added to the
  corresponding slot c1,
  c2, c3, ... or c26. The sublist cX stores the all \code{Partition} with
  X clusters. Inside a sublist, the
  \code{Partition} can be sorted from the biggest quality criterion to
  the smallest (the best are stored first, using
  \code{ordered,ListPartition}), or not.

  Note that \code{Partition} are saved throughout the algorithm. If the user
  interrupts the execution of \code{kml}, the result is not lost. If the
  user run \code{kml} on an object, then runnig \code{kml} again on the same object
  will add some new \code{Partition} to the one already found.

  The possible starting conditions are defined in \code{\link[longitudinalData]{initializePartition}}.
}

\section{Optimisation}{
  Behind kml, there are two different procedures :
  \enumerate{
    \item Fast: when the parameter \code{distance} is set to "euclidean"
      and \code{toPlot} is set to 'none' or
      'criterion', \code{kml} call a C
      compiled (optimized) procedure.
    \item Slow: when the user defines its own distance or if he wants
    to see the construction of the clusters by setting \code{toPlot} to
  'traj' or 'both', \code{kml} uses a R non compiled
    programmes.
  }
  The C prodecure is 25 times faster than the R one.

  So we advice to use the R procedure 1/ for trying some new method
  (like using a new distance) or 2/ to "see" the very first clusters
  construction, in order to check that every thing goes right. Then it
  is better to
  switch to the C procedure (like we do in \code{Example} section).

  If for a specific use, you need a different distance, feel free to
  contact the author.
}

\value{
  A \code{\linkS4class{ClusterLongData}} object, after having added
  some \code{Partition} to it.
}










\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}}, 
  %\code{\linkS4class{Partition}} \cr  
  \code{Partition} in package longitudinalData \cr
  Methods : \code{\link{clusterLongData}}, \code{\link{choice}}
}

\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Generation of some data
cld1 <- generateArtificialLongData(25)

### We suspect 3, 4 or 6 clusters, we want 3 redrawing.
###   We want to "see" what happen (so printCal and printTraj are TRUE)
kml(cld1,c(3,4,6),3,toPlot='both')

### 4 seems to be the best. We want 7 more redrawing.
###   We don't want to see again, we want to get the result as fast as possible.
kml(cld1,4,10)

### Go back to current dir
setwd(wd)
}
}



\keyword{dplot}    % Computations Related to Plotting
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
