\name{ClusterLongData-class}
\docType{class}
\alias{ClusterLongData-class}
\alias{[<-,ClusterLongData,character,missing,missing-method}
%\alias{[<-,ClusterLongData-method}
\alias{[,ClusterLongData-method}
%\alias{[,ClusterLongData,ANY,missing-method}
%\alias{[,ClusterLongData,ANY-method}
\alias{is.na,ClusterLongData-method}
%\alias{show,ClusterLongData-method}

\title{~ Class: ClusterLongData ~}
%\alias{addClusterization,ClusterizLongData-method}
%\alias{addPartition,ClusterizLongData-method}
%\alias{choice,ClusterizLongData-method}
%\alias{getCalinskiCriterion,ClusterizLongData-method}
%\alias{getClusterizList,ClusterizLongData,missing,missing-method}
%\alias{getClusterizList,ClusterizLongData,numeric,missing-method}
%\alias{getClusterizList,ClusterizLongData,numeric,numeric-method}
%\alias{KmL,ClusterizLongData-method}
%\alias{plot,ClusterizLongData,ANY-method}
%\alias{print,ClusterizLongData-method}
%\alias{show,ClusterizLongData-method}

\description{
  \code{ClusterLongData} is an object containing trajectories and associated 
  % \code{\linkS4class{[longitudinalData]}{Partition}}
  %\code{\link[=Partition-class]{[longitudinalData]}{Partition}} 
  %\code{"\link[=Partition-class]{[longitudinalData]}{Partition}"} 
  \code{Partition} (from package LongitudinalData).
}

\section{Objects from the Class}{
  \code{\link{kml}} is an algorithm that builds a set of 
  %\code{\linkS4class{[longitudinalData]}{Partition}}
  \code{Partition} 
  from longitudinal data. \code{ClusterLongData}
  is the object containing the original longitudinal data 
  and all the \code{Partition} that \code{kml} finds.\cr

  When created, an \code{ClusterLongData} object simply contains initial
  data (the trajectories). After the execution of \code{kml}, it
  contains
  the original data and the \code{Partition} 
  %\code{\linkS4class{[longitudinalData]}{Partition}} 
  which has just been calculated by \code{kml}.

  Note that if \code{kml} is executed several times, every new \code{Partition}
  is added to the original ones, no pre-existing \code{Partition} is erased.
}

\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the trajectory (each individual). Usefull for exporting clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for details.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[character]}: Name of the variable measured.}
    \item{\code{traj}}{\code{[matrix(numeric)]}: Contains
      the longitudianl data. Each lines is the trajectories of an
      individual. Each column is the time at which measures
      are made.}
    \item{\code{dimTraj}}{\code{[vector2(numeric)]}: size of the matrix
      \code{traj} (ie \code{dimTraj=c(length(idFewNA),length(time))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain 'too many' missing value
      are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. 'too many' is define by \code{maxNA}: a
      trajectory with more missing than \code{maxNA} is exclude.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: if the trajectories
      are scale using the function \code{\link{scale}}, the 'scaling
      parameters' (probably mean and standard deviation) are saved in
      \code{reverse}. This is usefull to restaure the original data after a
      scaling operation.}

    \item{\code{criterionActif}}{[character]: Store the criterion name
      that will be used by functions that need a single criterion
      (like \link[longitudinalData]{plotCriterion} or \link{ordered}).}
    \item{\code{initializationMethod}}{[vector(chararcter)]: list all
      the initialization method that has already been used to find some
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
  %Class \code{\linkS4class{[longitudinalData]}{LongData}}, directly.
  %Class \code{\linkS4class{[longitudinalData]}{ListPartition}}, directly.
  Class \code{LongData}, directly.
  Class \code{ListPartition}, directly.
}

\section{Construction}{
  Class \code{ClusterizLongData} objects can be constructed via function
  \code{\link{clusterLongData}} that turn a \code{data.frame} or a \code{matrix}
  into a \code{ClusterLongData}. Note that some artificial data can be
  generated using \code{\link{gald}}.
}

\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. Inherit from \code{LongData} and \code{ListPartition}.
      %\code{\linkS4class{[longitudinalData]}{LongData}} and
      %\code{\linkS4class{[longitudinalData]}{ListPartition}}.
      }
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.
      \code{xxx}. Inherit from class \code{ListPartition}.
      %\code{\linkS4class{[longitudinalData]}{ListPartition}}.
      }
    \item{\code{\link[=plot,ClusterLongData]{plot}}}{Display the
      \code{ClusterLongData} according to a class \code{Partition}.
      %\code{\linkS4class{[longitudinalData]}{Partition}}.
      }
  }
}


\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : 
  %\code{\linkS4class{[longitudinalData]}{Partition}}, 
  %\code{\linkS4class{[longitudinalData]}{LongData}}, 
  %\code{\linkS4class{[longitudinalData]}{ListPartition}}
  classes Partition, LongData, ListPartition  \cr
  Methods : \code{\link{clusterLongData}}, \code{\link{kml}}, \code{\link{choice}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterLongData)}},
  \code{\link[longitudinalData]{plotCriterion}}
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

################
### Creation of some trajectories

traj <- matrix(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),6)

myCld <- clusterLongData(
    traj=traj,
    idAll=as.character(c(100,102,103,109,115,123)),
    time=c(1,2,4,8,15),
    varNames="P",
    maxNA=3
)

################
### get and set
myCld["idAll"]
myCld["varNames"]
myCld["traj"]

################
### Creation of a Partition
part2 <- partition(clusters=rep(1:2,3),myCld)
part3 <- partition(clusters=rep(1:3,2),myCld)


################
### Adding a clusterization to a clusterizLongData
myCld["add"] <- part2
myCld["add"] <- part3
myCld

### Go back to current dir
setwd(wd)
}
\keyword{classes}
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Partition
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
