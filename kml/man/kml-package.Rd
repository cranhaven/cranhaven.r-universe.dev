\name{kml-package}
\alias{kml-package}
\docType{package}
\title{~ Overview: K-means for Longitudinal data ~}

\description{
  This package is a implementation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package. For the description of the
  algorithm, see \code{\link{kml}}.
}
\details{
  \tabular{ll}{
    Package: \tab kml\cr
    Type: \tab Package\cr
    Version: \tab 2.4.1\cr
    Date: \tab 2016-02-02\cr
    License: \tab GPL (>= 2)\cr
    LazyData: \tab yes\cr
    Depends: \tab methods,clv,longitudinalData(>= 2.1.2)\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To cluster data, \code{KmL} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" partition

    \item Exporting results
  }
}


\section{1. Data preparation}{
  \code{KmL} works on object of class \code{\linkS4class{ClusterLongData}}.
  Data preparation therefore simply consists in transforming data into an object \code{\linkS4class{ClusterLongData}}.
  This can be done via function
  \code{\link[=clusterLongData]{clusterLongData}}
  (\code{\link[=clusterLongData]{cld}} in short).
  It converts a \code{data.frame} or a \code{matrix} into a \code{\linkS4class{ClusterLongData}}.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with
  \code{\link{generateArtificialLongData}}
  (\code{\link{gald}} in
  short).
}

\section{2. Building "optimal" partition}{
  Once an object of class \code{\linkS4class{ClusterLongData}} has been created, the algorithm
  \code{\link[=kml]{kml}} can be run.

  Starting with a \code{ClusterLongData}, \code{kml} built a 
  \code{Partition}, a class in package longitudinalData. 
  %\code{\linkS4class{Partition}}.
  An object of class \code{Partition} is a partition of trajectories
  into subgroups. It also contains some information like the
  percentage of trajectories contained in each group or some quality critetion.\cr

  \code{kml} is a "hill-climbing" algorithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.
  To maximize one's chances of getting a quality \code{Partition}, it is better to run the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Partition} maximizing the determinant of the matrix between.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate
  clues that will enable us to choose.
  In the end, \code{kml} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{\link{kml}} has constructed some
  \code{Partition}, the user can examine them one by one and choose
  to export some. This can be done via function
  \code{\link{choice}}. \code{choice} opens a graphic windows showing
  various information including the trajectories clutered by a specific
  \code{Partition}.

  When some \code{Partition} has been selected (the user can select
  more than 1), it is possible to
  save them. The clusters are therefore exported towards the file
  \code{name-cluster.csv}. Criteria are exported towards
  \code{name-criteres.csv}. The graphs are exported according to their
  extension.

  It is also possible to extract a partition from the object
  \code{\linkS4class{ClusterLongData}} using the function \code{getClusters}.
}


\seealso{
  Classes : \code{\linkS4class{ClusterLongData}}, 
  \code{Partition} in package longitudinalData \cr
  % \code{\linkS4class{Partition}} \cr
  % \code{\link[=Partition-class]{[longitudinalData]Partition}} \cr
  % \code{\link[longitudinalData=Partition-class]{Partition}} \cr
  Methods : \code{\link{clusterLongData}}, \code{\link{kml}}, \code{\link{choice}}\cr
  Plot : \code{\link[=plot,ClusterLongData]{plot(ClusterLongData)}}
}

\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### 1. Data Preparation
data(epipageShort)
names(epipageShort)
cldSDQ <- cld(epipageShort,timeInData=3:6,time=c(3,4,5,8))

### 2. Building "optimal" clusteration (with only 3 redrawings)
kml(cldSDQ,nbRedrawing=3,toPlot="both")

### 3. Exporting results
### To check the best's cluster numbers
plotAllCriterion(cldSDQ)


# To see the best partition
try(choice(cldSDQ))

### 4. Further analysis
epipageShort$clust <- getClusters(cldSDQ,4)
summary(glm(gender~clust,data=epipageShort,family="binomial"))

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
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
\keyword{models}   % Statistical Models



