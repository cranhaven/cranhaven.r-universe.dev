\name{ClusterLongData3d-class}
\docType{class}
\alias{ClusterLongData3d-class}
\alias{[,ClusterLongData3d-method}
\alias{[,ClusterLongData3d,ANY,missing-method}
\alias{[<-,ClusterLongData3d-method}
\alias{[<-,ClusterLongData3d,character,missing,missing-method}
\alias{show,ClusterLongData3d-method}

\title{~ Class: ClusterLongData3d ~}

\description{
  \code{ClusterLongData3d} is an object containing joint-trajectories and
  associated \code{Partition} (from package \code{longitudinalData}).
}

\section{Objects from the Class}{
  \code{\link{kml3d}} is an algorithm that builds a set of \code{Partition}
  from joint longitudinal data. \code{ClusterLongData3d}
  is the object containing the original joint longitudinal data and all the \code{Partition} that \code{kml3d} finds.\cr

  When created, an \code{ClusterLongData3d} object simply contains initial
  data (the joint-trajectories).
  After the execution of \code{kml3d}, it contains
  the original data and the \code{Partition} which has
  just been found by \code{kml3d}.

  Note that if \code{kml3d} is executed several times, every new \code{Partition}
  are added to the original ones, no pre-existing \code{Partition} is erased.
}

\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the joint-trajectory (each individual). Usefull for exporting clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for details.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[vector(character)]}: Names of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the joint longitudianl data. Each horizontal plan (first dimension) corresponds to the trajectories of an
      individual. Vertical plans (second dimension) refer to the time at which measures
      are made. Transversal plans (the third dimension) are for variables.}
    \item{\code{dimTraj}}{\code{[vector3(numeric)]}: size of the array
      \code{traj} (ie \code{c(length(idFewNA),length(time),length(varNames))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain more missing value than
      \code{maxNA} are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. When \code{maxNA} is a single number, it is
      used for all the variables.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: contain the
      mean (first line) and the standard deviation (second line) used to
      normalize the data. Usefull to restaure the original data after a
      scaling operation.}

    \item{\code{criterionActif}}{[character]: Store the criterion name
      that will be used by functions that need a single criterion
      (like \link[longitudinalData]{plotCriterion} or \link[longitudinalData]{ordered}).}
    \item{\code{initializationMethod}}{[vector(chararcter)]: list all
      the initialization method that has allready been used to find some
      \code{Partition}
      (usefull to not run several time a deterministic method).}
    \item{\code{sorted}}{\code{[logical]}: are the \code{Partition}
      curently hold in the object sorted in decreasing order ?}
    \item{\code{c1}}{[list(Partition)]: list of
      \code{Partition} with 1 clusters.}
    \item{\code{c2}}{[list(Partition)]: list of
      \code{Partition} with 2 clusters.}
    \item{\code{c3}}{[list(Partition)]: list of
      \code{Partition} with 3 clusters.}
    \item{\code{...}}{}
    \item{\code{c26}}{[list(Partition)]: list of
      \code{Partition} with 26 clusters.}
  }
}
\section{Extends}{
  Class \code{LongData3d} in package\code{longitudinalData}, directly.
  Class \code{ListPartition} in package\code{longitudinalData}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. Inherit from  \code{LongData3d} and
      \code{ListPartition} (in package \code{longitudinalData}).}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.
      \code{xxx}. Inherit from  \code{ListPartition}.}
    \item{\code{\link[=plot,ClusterLongData3d]{plot}}}{Display the \code{ClusterLongData3d}, one
      graph for each variable, according to a \code{Partition}.}
    \item{\code{\link{plot3d}}}{Display two
      variables of the \code{ClusterLongData3d} in 3D according to a
      \code{Partition}.}
    \item{\code{\link{plot3dPdf}}}{Export the AZY code for displaying two
      variables of the \code{ClusterLongData3d} in a 3D pdf graph.}
  }
}

\section{Special thanks}{
   Special thanks to Boris Hejblum for debugging the '[' and '[<-' operators (the
   previous version was not compatible with the matrix package, which is
   used by lme4).
}

\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Building longData
traj <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

myCld <- clusterLongData3d(
    traj=traj,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
)

### Show
myCld

### Get
myCld['varNames']

### Set
myCld['criterionActif']<-"Davies.Bouldin"

### Plot
plot(myCld)

### Go back to current dir
setwd(wd)
}
\keyword{classes}
