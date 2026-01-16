\name{kml3d}
\alias{kml3d}
\alias{kml3d-method}
\alias{kml3d,ClusterLongData-method}

\title{~ Algorithm kml3d: K-means for Joint Longitidinal data ~}

\description{
  \code{kml3d} is a new implementation of k-means for joint longitudinal
  data (or joint trajectories). This algorithm is able to deal with missing value and
  provides an easy way to re roll the algorithm several times, varying the starting conditions and/or the number of clusters looked for.

  Here is the description of the algorithm. For an overview of the package, see \link{kml3d-package}.
}

\usage{
kml3d(object, nbClusters = 2:6, nbRedrawing = 20, toPlot = "none",
    parAlgo = parKml3d())
}

\arguments{
  \item{object}{[ClusterLongData3d]: contains trajectories to clusterize
    and some \code{Partition} (see package \code{longitudinalData}).
  }
  \item{nbClusters}{[vector(numeric)]: Vector containing the number of clusters
    with which \code{kml3d} must work. By default,
    \code{nbClusters} is \code{2:6} which indicates that \code{kml3d} must
    search partitions with respectively 2, then 3, ... up to 6
    clusters. Maximum number of cluster is 26.
  }
  \item{nbRedrawing}{[numeric]: Sets the number of time that k-means
    must be re-run (with different starting conditions) for each number
    of clusters.
  }
  \item{toPlot}{\code{[character]}: during computation, \code{kml3d} can
    display some graphes. If \code{toPlot="traj"},
    then the trajectories are plot (like with function
    \code{plot, ClusterLongData}).
    If \code{toPlot="criterion"}, the quality criterions are plot (like with function
    \code{\link[longitudinalData]{plotCriterion}}). If \code{toPlot="both"}, the graphic
    windows is split in two and both graphs are displayed. If "none",
    there is no graphical display.
  }
  \item{parAlgo}{\code{[ParKml]} (in package \code{kml}): set the option used
    by \code{kml3d} (like the starting condition, the imputation methods,
    the save frequency, the maximum number of iteration, , the distance
    used...) See \code{ParKml} in package \code{kml} for details.
    The default values are described in \code{\link{parKml3d}}.
  }
}

\details{
  \code{kml3d} works on object of class \code{ClusterLongData}.
  For each number \code{i} included in \code{nbClusters}, \code{kml3d} computes a
  \code{Partition} then stores it in the field
  \code{cX} of the object \code{ClusterLongData} according to its number
  of clusters 'X'.
  The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a \code{Partition} has been found, it is added to the slot
  c1, c2, c3, ... or c26. \code{cX} stores the all \code{Partition} with
  X clusters. Inside a sublist, the
  \code{Partition} are sorted from the biggest quality criterion to
  the smallest (the best are stored first, using
  \code{ordered,ListPartition}), or not.

  Note that \code{Partition} are saved throughout the algorithm. If the user
  interrupts the execution of \code{kml3d}, the result is not lost. If the
  user run \code{kml3d} on an object, then running \code{kml3d} again on
  the same object will add some new \code{Partition} to the one already
  found.

  The possible starting conditions are defined in \code{\link[longitudinalData]{initializePartition}}.
}

\section{Optimisation}{
  Behind \code{kml3d}, there are two different procedures :
  \enumerate{
    \item Fast: when the parameter \code{distance} is set to "euclidean3d"
      and \code{toPlot} is set to 'none' or
      'criterion', \code{kml3d} call a C
      compiled (optimized) procedure.
    \item Slow: when the user defines its own distance or if he wants
    to see the construction of the clusters by setting \code{toPlot} to
    'traj' or 'both', \code{kml3d} uses a R non compiled
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
  A \code{\linkS4class{ClusterLongData3d}} object, after having added
  some \code{Partition} to it.
}



\seealso{
  Overview: \code{\link{kml3d-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData3d}}, \code{Partition} in package \code{longitudinalData} \cr
  Methods : \code{\link{clusterLongData3d}}, \code{\link[kml]{choice}}
}

\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()
  
### Generation of some data
cld1 <- generateArtificialLongData3d(15)

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
###   We want to "see" what happen (so toPlot="both")
kml3d(cld1,2:5,3,toPlot="both")

### 3 seems to be the best.
###   We don't want to see again, we want to get the result as fast as possible.
###   Just, to check the overall process, we plot the criterion evolution
kml3d(cld1,3,10,toPlot="criterion")

### Go back to current dir
setwd(wd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


\keyword{dplot}    % Computations Related to Plotting
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Partition
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
