\name{longData3d}
\alias{longData3d}
\alias{longData3d,ANY,ANY,ANY,ANY,ANY,ANY-method}
\alias{longData3d,missing,missing,missing,missing,missing,missing-method}

\title{ ~ Function: longData3d ~}
\description{
\code{longData3d} is a constructor of the class \code{\linkS4class{LongData}}.
It create object \code{\linkS4class{LongData}} containing several joint trajectory (two
or more variable-trajectories). For creating a single
variable-trajectory, see \code{\link{longData}}.
}
\usage{
longData3d(traj, idAll, time, timeInData,varNames,maxNA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[array(numeric)]} or \code{data.frame}: structure containning the variable-trajectories.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).}
  \item{time}{\code{[vector(numeric)]}: time at which measures were made.}
  \item{timeInData}{\code{[list(vector(numeric))]}:  Precise the column containing the
    trajectories.  If \code{traj} is a \code{data.frame}, it sould be a list.}

  \item{varNames}{\code{[character]}: name of the variable-trajectories being measured.}
  \item{maxNA}{\code{[vector(numeric)]}: maximum number of NA that are tolerates
    on a trajectory (one for each variable). If a trajectory has more
    missing than maxNA, then it is remove from the analysis.}
}

\details{
  \code{longData3d} construct a object of class
  \code{\linkS4class{LongData}}. Two cases can be distinguised:

  \describe{
    \item{\code{traj} is an \code{array}:}{ the first dimension (line) are
      individual. The second dimension (column) are time at which the
      measurement are made. The third dimension are the differents
      variable-trajectories. For example, \code{traj[,,2]} is the second variable-trajectory.

      If \code{idAll} is missing, the individuals are labelled \code{i1},
      \code{i2}, \code{i3},...

      If \code{timeInData} is missing, all the column
      are used (\code{1:ncol(traj)}).}

    \item{If \code{traj} is a \code{data.frame}:}{ lines are individual. Time of
      measurement and variables should be provide through
      \code{timeInData}. \code{timeInData} is a list.
      The label of the list are the
      variable-trajectories names. Elements of the list are the column
      containning the trajectories. For example, if
      \code{timeInData=list(V=c(2,3,4),W=c(6,8,12))}, then the first
      variable-trajectory is 'V', its measurement are in column 2,3 and
      4. The second variable-trajectory is 'W', its measurement are in column
      6,8 and 12.

      If \code{idAll} is missing, the first column of the \code{data.frame}
      is used.}
  }
}

\value{
  An object of class \code{\linkS4class{LongData}}.
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

\seealso{\linkS4class{LongData}}

\examples{
#################
### From array

mat <- array(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),dim=c(3,3,2))
longData3d(mat)
(ld1 <- longData3d(mat,varNames=c("Hyp","Col"),idAll=c("i101","i104","i105")))
plotTrajMeans3d(ld1)

#################
### From data.frame

dn <- data.frame(id=1:3,v1=c(2,2,1),t1=c(20,21,22),v1=c(3,2,2),t2=c(23,20,28),t3=c(25,24,29))
longData3d(dn,timeInData=list(c(2,4),c(3,5)),varNames=c("V","T"))
(ld3 <- longData3d(dn,timeInData=list(V=c(2,4,NA),T=c(3,5,6))))
plotTrajMeans3d(ld3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{cluster}
\keyword{methods}



