\name{kml3d-package}
\alias{kml3d-package}
\alias{[,ParChoice-method}
\alias{[<-,ParChoice-method}
\title{~ Overview: KmL3D, K-means for joint Longitudinal data ~}

\description{
  \code{KmL3D} is a new implementation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package.
}
\details{
  \tabular{ll}{
    Package: \tab KmL3D\cr
    Type: \tab Package\cr
    Version: \tab 2.4.2\cr
    Date: \tab 2017-08-01\cr
    License: \tab GPL (>= 2)\cr
    LazyData: \tab yes\cr
    Depends: \tab methods,graphics,rgl,misc3d,longitudinalData(>=2.2),KmL(>=2.2)\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To cluster data, \code{KmL3D} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" clusterization.

    \item Exporting results

    \item Visualizing and exporting 3D object
  }
}


\section{1. Data preparation}{
  \code{kml3d} works on object of class \code{\linkS4class{ClusterLongData3d}}.
  Data preparation therefore simply consists in transforming data into an object \code{ClusterLongData3d}.
  This can be done via function
  \code{\link[=clusterLongData3d]{clusterLongData3d}}
  (\code{\link[=clusterLongData3d]{cld3d}} in short) that
  converts a \code{data.frame} or an \code{array} into a \code{\linkS4class{ClusterLongData3d}}.

  Working on several variables mesured on different scales can give to
  much weight to one of the dimension. So the function \code{\link{scale}} normalizes data.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with \code{\link{generateArtificialLongData3d}}
  (\code{\link{gald3d}} in short).

}

\section{2. Building "optimal" clustering}{
  Once an object of class \code{\linkS4class{ClusterLongData3d}} has been created, the algorithm
  \code{\link{kml3d}} can be run.

  Starting with a \code{ClusterLongData3d}, \code{kml3d} built several \code{Partitions} (see package \code{longitudinalData}).
  An object of class \code{Partition} is a partition of trajectories
  into subgroups. It also contains some information like the
  percentage of trajectories contained in each group or some quality critetion (like the Calinski &
  Harabasz).\cr

  \code{k-means} is a "hill-climbing" algorithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Partition},
  it is better to execute the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml3d} executes the hill climbing algorithm 20 times.

  To date, it is not possible to know the optimum number of clusters
  even if the calculatous of some qualities criterion can gives some
  clues. \code{kml3d} computes various of them.

  In the end, \code{kml3d} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{\link{kml3d}} has constructed some
  \code{Partition}, the user can examine them one by one and choose
  to export some. This can be done via function
  \code{\link[kml]{choice}}. \code{choice} opens a graphic windows showing
  various information including the trajectories cluterized by a specific
  \code{Partition}.

  When some \code{Partition} has been selected (the user can select
  more than 1), it is possible to
  save them. The clusters are therefore exported towards the file
  \code{name-cluster.csv}. Criteria are exported towards
  \code{name-criteres.csv}. The graphs are exported according to their
  extension.
}

\section{4. Visualizing and exporting 3D object}{
  \code{KmL3D} also propose tools to visualize the trajectories in
  3D. \code{\link{plot3d}} using the library \code{rgl} to plot two
  variables according to time (either the all set of joint-trajectories, or
  just the mean joint-trajectories). Then the user can make the
  graphical representation turn using the mouse. \code{\link{plot3dPdf}} build an
  \code{\link[misc3d:makeTriangles]{Triangles}} object. These kind of
  object can be include in a \code{pdf} file using
  \code{\link[longitudinalData]{saveTrianglesAsASY}} and the software
  \code{asymptote}. Once again, it is possible to make the image in the
  pdf file move using the mouse -so the reader gets real 3D-.
}



\section{How to get help?}{
  For those who are not familiar with S4 programming:
  In S4 programming, each function can be adapted for some specific
  arguments.
  \itemize{
    \item{To get help on a function (for example \code{plot}), use:
      \code{?(plot)}.}
    \item{To get help on a function adapted to its argument (for example \code{plot}
      on argument \code{ClusterLongData}), used: \code{?"plot,ClusterLongData"}.}
  }
}

\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### 1. Data Preparation
data(pregnandiol)
names(pregnandiol)
cld3dPregTemp <- cld3d(pregnandiol,timeInData=list(temp=1:30*2,preg=1:30*2+1))

### 2. Building "optimal" clusteration (with only 2 redrawings)
###    Real analysis needs at least 20 redrawings
kml3d(cld3dPregTemp,3:5,nbRedrawing=2,toPlot="both")

### 3. Exporting results
try(choice(cld3dPregTemp))

### 4. Visualizing in 3D
plotMeans3d(cld3dPregTemp,4)

### Go back to current dir
setwd(wd)
}
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Partition
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
\keyword{models}   % Statistical Models



