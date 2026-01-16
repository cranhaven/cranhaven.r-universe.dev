\name{clusterLongData3d}
\alias{cld3d}
\alias{clusterLongData3d}
\alias{clusterLongData3d,ANY,ANY,ANY,ANY,ANY,ANY}
\alias{clusterLongData3d,ANY,ANY,ANY,ANY,ANY,ANY-method}
\alias{clusterLongData3d,missing,missing,missing,missing,missing,missing}
\alias{clusterLongData3d,missing,missing,missing,missing,missing,missing-method}

\title{ ~ Function: clusterLongData3d (or cld3d) ~}
\description{
  \code{clusterLongData3d} (or \code{cld3d} in short) is the constructor
  for \code{\linkS4class{ClusterLongData3d}} object.
}
\usage{
clusterLongData3d(traj, idAll, time, timeInData, varNames, maxNA)
cld3d(traj, idAll, time, timeInData, varNames, maxNA)
}

\arguments{
  \item{traj}{\code{[array(numeric)]} or \code{[data.frame]}:
    structure containning the joint-trajectories. Each line
    (\code{traj[i,,]}) is a joint-trajectory
    of an individual ; columns (\code{traj[,j,]}) refer to the time during which
    measures were made ; the third dimensions (\code{traj[,,l]}) are for
    variables.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each
    trajectory (ie each individual). Note that the identifiers are of type
    \code{character} (that allow to deal identifiers like \code{XUK32-612},
    identifiers that our favorite epidemiologists are so good at
    providing). If \code{idAll} are \code{numeric}, they are converted into \code{characters}.}
  \item{time}{\code{[vector(numeric)]}: time at which measures were made.}
  \item{timeInData}{\code{[list(vector(numeric))]}: precise the column
    containing the trajectories. The list labels are the names of the
    variables (like \code{list(A=c(2,3,4),B=c(5,7,9))}).}
  \item{varNames}{\code{[character]}: name of the variable being measured.}
  \item{maxNA}{\code{[numeric]} or \code{[vector(numeric)]} : maximum number of NA that are tolerates
    on a trajectory. If a trajectory has more
    missing than \code{maxNA}, then it is remove from the analysis. Note
    the \code{maxNA} can take diffents values for each
    variable-trajectories.
    The default value is \code{length(time)-2}.}
}

\details{
  \code{clusterLongData3d} construct a object of class
  \code{ClusterLongData} (from package \code{kml}). Two cases can be distinguised:

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
      variable-trajectory is 'V', its mesearment are in column 2,3 and
      4. The second variable-trajectory is 'W', its measurment are in column
      6,8 and 12.

      If \code{idAll} is missing, the first column of the \code{data.frame}
      is used.}
  }
}

\value{
  An object of class \code{\linkS4class{ClusterLongData3d}}.
}

\examples{
###############
### Building an array
tr1n <- array(c(1,2,NA, 1,4,NA, 6,1,8, 10,NA,2, 3,NA,NA,
                4,NA,5,  6,3,4, 3,4,4, 4,NA,NA, 5,5,4),
            dim=c(3,5,2))


###############
### clusterLongData

### With maxNA=3
clusterLongData3d(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
)

### With maxNA=2
### Individual 104 is exclude
clusterLongData3d(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=2
)

}

