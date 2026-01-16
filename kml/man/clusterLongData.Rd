\name{clusterLongData}
\alias{cld}
\alias{clusterLongData}
%\alias{clusterLongData,ANY,ANY,ANY,ANY,ANY,ANY}
\alias{clusterLongData,ANY,ANY,ANY,ANY,ANY,ANY-method}
%\alias{clusterLongData,missing,missing,missing,missing,missing,missing}
\alias{clusterLongData,missing,missing,missing,missing,missing,missing-method}
\title{ ~ Function: clusterLongData (or cld) ~}
\description{
  \code{clusterLongData} (or \code{cld} in short) is the constructor
  for \code{\linkS4class{ClusterLongData}} object.
}
\usage{
clusterLongData(traj, idAll, time, timeInData, varNames, maxNA)
cld(traj, idAll, time, timeInData, varNames, maxNA)
}

\arguments{
  \item{traj}{\code{[matrix(numeric)]} or \code{[data.frame]}:
    structure containning the trajectories. Each line is the trajectory
    of an individual. The columns refer to the time during which
    measures were made.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each
    trajectory (ie each individual). Note that the identifiers are of type
    \code{character} (that allow to deal identifiers like \code{XUK32-612},
    identifiers that our favorite epidemiologists are so good at
    providing). If \code{idAll} are \code{numeric}, they are converted into \code{characters}.}
  \item{time}{\code{[vector(numeric)]}: time at which measures were made.}
  \item{timeInData}{\code{[vector(numeric)]}: precise the column
    containing the trajectories.}
  \item{varNames}{\code{[character]}: name of the variable being measured.}
  \item{maxNA}{\code{[numeric]}: maximum number of NA that are tolerates
    on a trajectory. If a trajectory has more
    missing than \code{maxNA}, then it is remove from the analysis.}
}


\details{
  \code{clusterLongData} construct a object of class \code{\linkS4class{ClusterLongData}}.
  Two cases can be distinguised:

  \describe{
    \item{\code{traj} is an \code{array}:}{ lines are individual. Column
      are time of measurment.

      If \code{idAll} is missing, the individuals are labelled \code{i1},
      \code{i2}, \code{i3},...

      If \code{timeInData} is missing, all the column
      are used (\code{timeInData=1:ncol(traj)}).}
    \item{If \code{traj} is a \code{data.frame}:}{ lines are
      individual. Column are time of measurement.

      If \code{idAll} is missing, then the first column of the
      \code{data.frame} is used for \code{idAll}

      If \code{timeInData} is missing and \code{idAll} is missing, then
      all the columns but the first are used for \code{timeInData} (the
      first is omited since it is already used for \code{idAll}): \code{idAll=traj[,1],timeInData=2:ncol(traj)}.

      If \code{timeInData} is missing but \code{idAll} is not missing,
      then all the column including the first are used for \code{timeInData}: \code{timeInData=1:ncol(traj)}.
    }
  }
}
\value{
  An object of class \code{\linkS4class{ClusterLongData}}.
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

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}} \cr
  Methods : \code{\link{choice}}, \code{\link{kml}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterLongData)}}
}


\examples{
#####################
### From matrix

### Small data
mat <- matrix(c(1,NA,3,2,3,6,1,8,10),3,3,dimnames=list(c(101,102,104),c("T2","T4","T8")))
clusterLongData(mat)
(ld1 <- clusterLongData(traj=mat,idAll=as.character(c(101,102,104)),time=c(2,4,8),varNames="V"))
plot(ld1)

### Big data
mat <- matrix(runif(1051*325),1051,325)
(ld2 <- clusterLongData(traj=mat,idAll=paste("I-",1:1051,sep=""),time=(1:325)+0.5,varNames="R"))

####################
### From data.frame

dn <- data.frame(id=1:3,v1=c(NA,2,1),v2=c(NA,1,0),v3=c(3,2,2),v4=c(4,2,NA))

### Basic
clusterLongData(dn)

### Selecting some times
(ld3 <- clusterLongData(dn,timeInData=c(1,2,4),varNames=c("Hyp")))

### Excluding trajectories with more than 1 NA
(ld3 <- clusterLongData(dn,maxNA=1))
}
\keyword{classes}
