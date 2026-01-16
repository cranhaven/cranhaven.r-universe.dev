\name{LongData3d-class}
\docType{class}
\alias{LongData3d}
\alias{LongData3d-class}
\alias{[,LongData3d-method}
\alias{[,LongData3d-ANY,ANY,ANY}
\alias{[,LongData3d-ANY,ANY,ANY-method}
\alias{[<-,LongData3d-method}
\alias{[<-,LongData3d-ANY,ANY,ANY-method}
\alias{LongData3d_show}
\alias{show,LongData3d-method}
\alias{is.na,LongData3d-method}


\title{~ Class: LongData3d ~}
\description{\code{LongData3d} is an objet containing joint longitudinal
  data and some associate value (like time, individual
  identifiant,...).
}

\section{Objects from the Class}{
  Object \code{LongData3d} can be created using
  the fonction \code{\link{longData3d}} on a \code{data.frame} or on an \code{array}.
}

\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the longData3d (each individual). Usefull to export clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for 'too many' definition.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[vector(character)]}: Names of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the joint variable-trajectories. Each horizontal plan (first
      dimension) corresponds to the joint-trajectories of an
      individual. Vertical plans (second dimension) refer to the time at which measures
      are made. Transversal plans (the third dimension) are for variables.}
    \item{\code{dimTraj}}{\code{[vector3(numeric)]}: size of the array
      \code{traj} (ie \code{dimTraj=c(length(idFewNA),length(time),length(varNames))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain 'too many' missing value
      are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. 'too many' is define by \code{maxNA}: a
      trajectory with more missing than \code{maxNA} is exclude.
      When \code{maxNA} is a single number, it is
      recycled for all the variables.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: if the trajectories
      are scale using the function \code{\link{scale}}, the 'scaling
      parameters' (probably mean and standard deviation) are saved in
      \code{reverse}. This is usefull to restore the original data after a
      scaling operation.}
  }
}

\section{Construction}{
  \code{LongData3d} can be created by calling
  the fonction \code{\link{longData3d}} on a \code{data.frame} or on an \code{array}.
}

\section{Get [}{
  \describe{
    \item{Object["idAll"]}{[vecteur(character)]: Gets the full list of individual
      identifiant (the value of the slot \code{idAll})}
    \item{Object["idFewNA"]}{[vecteur(character)]: Gets the list of individual
      identifiant with not too many missing values (the value of the slot \code{idFewNA})}
    \item{Object["varNames"]}{[character]: Gets the name(s) of the variable (the value of the slot \code{varNames})}
    \item{Object["time"]}{[vecteur(numeric)]: Gets the times (the value of the slot \code{time})}
    \item{Object["traj"]}{[array(numeric)]: Gets all the joint trajectories (the value of the slot \code{traj})}
    \item{Object["dimTraj"]}{[vector3(numeric)]: Gets the dimension of \code{traj}.}
    \item{Object["nbIdFewNA"]}{[numeric]: Gets the first dimension of
      \code{traj} (ie the number of individual include in the analysis).}
    \item{Object["nbTime"]}{[numeric]: Gets the second dimension of
      \code{traj} (ie the number of time measurement).}
    \item{Object["nbVar"]}{[numeric]: Gets the third dimension of
      \code{traj} (ie the number of variables).}
    \item{Object["maxNA"]}{[vecteur(numeric)]: Gets maxNA.}
    \item{Object["reverse"]}{[matrix(numeric)]: Gets the matrix of the
      scaling parameters.}
  }
}


% \section{Set [<-}{
%   \describe{
%     \item{Object["time"]<-value}{[vecteur(numeric)]: Sets the slot \code{time} to
%       \code{value}.}
%     \item{Object["varNames"]<-value}{[character]: Sets the name of the
%       variable to \code{value}.}
%     \item{Object["traj"]}{[array(numeric)]: Sets the trajectories
%       to \code{values}. Value can be either an array or a matrix (that
%       will be turn into an array)}
%     \item{Object["traj"][3,,]<-value}{[vecteur(numeric)]: Sets the trajectory of
%       the individual 3 to \code{value}.}
%   }
% }



\section{Methods}{
  \describe{
    \item{\code{\link{scale}}}{scale the trajectories. Usefull to
      normalize variable trajectories measured with different units.}
    \item{\code{\link{restoreRealData}}}{restore original data that
      have been modified after a scaling operation.}
%    \item{\code{\link{generateArtificialLongData3d}} (or
 %     \code{\link{gald3d}})}{Generate a artifial dataset of some joint
  %    variable-trajectory.}
    \item{\code{\link{longDataFrom3d}}}{Create a
  \code{\linkS4class{LongData}} by extracting a single variable trajectory
      form a dataset of joint variable-trajectories.}
    \item{\code{plotTrajMeans}}{plot all the variable of the \code{LongData3d}, optionnaly according to a \code{\linkS4class{Partition}}.}
    \item{\code{plotTrajMeans3d}}{plot two variables of the \code{LongData3d} in
      a 3 dimensions graph, optionnaly according to a \code{\linkS4class{Partition}}.}
    \item{\code{plot3dPdf}}{create 'Triangle objects' representing in
      3D the cluster's center according to a
      \code{\linkS4class{Partition}}. 'Triangle object' can latter be
      include in a LaTeX file to get a dynamique (rotationg) pdf
      figure.}
    \item{\code{imputation}}{Impute the missing values of the trajectories.}
    \item{\code{qualityCriterion}}{Compute some quality criterion that
      can be use to compare the quality of differents \code{\linkS4class{Partition}}.}
  }
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
  Overview: \code{\link{longitudinalData-package}} \cr
  Methods: \code{\linkS4class{LongData}},  \code{\link{longData3d}}, \code{\link{imputation}}, \code{\link{qualityCriterion}}\cr
  Plot: \code{\link{plotTrajMeans}},
  \code{\link{plotTrajMeans3d}},  \code{\link{plot3dPdf}}
}


\examples{
#################
### building joint trajectories

dn <- data.frame(id=1:3,v1=c(11,14,16),t1=c(1,5,7),v2=c(12,10,13),t2=c(2,5,0),t3=c(3,6,8))
(ld <- longData3d(dn,timeInData=list(Vir=c(2,4,NA),Tes=c(3,5,6))))

### Scaling
scale(ld)
(ld)

### Plotting
plotTrajMeans3d(ld)
restoreRealData(ld)
}
\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
