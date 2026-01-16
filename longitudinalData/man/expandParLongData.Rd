\name{expandParLongData}
\alias{expandParLongData}
%\alias{expandParLongData,ParLongData,Partition}
\alias{expandParLongData,ParLongData,Partition-method}
%\alias{expandParLongData,ParLongData,numeric}
\alias{expandParLongData,ParLongData,numeric-method}

\title{
~ Function: expandParLongData ~
}
\description{
  Prepare the values of an object
  \code{\linkS4class{ParLongData}} to make them being usable by a
  plotting function.
}
\usage{
expandParLongData(xParLongData, y)
}

\arguments{
  \item{xParLongData}{\code{[ParLongData]}: The object to expand.}
  \item{y}{\code{[Partition]} or \code{[numeric]}: see detail.}
}
\details{
  \code{\linkS4class{ParLongData}} object can hold values that are easy
  to specify (like \code{col="clusters"} or \code{pch="symbol"}) but that can not
  be directly used by graphical functions \link{plotTrajMeans} and
  \link{plotTrajMeans3d}. This function modify theses values to
  make them fit with \code{plotTrajMeans} and \code{plotTrajMeans3d} expectations.

  The field \code{col} and \code{pch} are the ones concern by this
  function.

  If \code{y} is a \code{Partition}, \code{col} and \code{pch} are
  extanded to fit with the number of individual. If \code{y} is a number of
  clusters, \code{col} and \code{pch} are extanded to fit with the
  number of clusters.

  If \code{col='clusters'}, a color is affected to each clusters. Then
  the field \code{col} receive a vector of color such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding color.

  If \code{pch='letters'}, a letters is affected to each clusters. Then
  the field \code{pch} receive a vector of letters such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding letters.

  Same if \code{pch='symbols'}.
}
\value{
An object of class \code{\linkS4class{ParLongData}}
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


\examples{
###################
### Some parameters for trajectories
(paramTraj <- parTRAJ(col="clusters"))

### Expand to a small partition with 3 clusters
part <- partition(LETTERS[rep(1:3,4)])
expandParLongData(paramTraj,part)


###################
### Some parameters for the mean trajectories
paramMean <- parMEAN()

### If there is 3 clusters :
expandParLongData(paramMean,3)

### If there is 5 clusters :
expandParLongData(paramMean,5)
}

