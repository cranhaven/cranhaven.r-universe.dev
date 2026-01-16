\name{fuzzyKmlSlow}
\alias{fuzzyKmlSlow}

\title{~ Algorithm fuzzy kml: Fuzzy k-means for Longitidinal data ~}

\description{
  \code{fuzzyKmlSlow} is a new implementation of fuzzy k-means for longitudinal data (or trajectories).
}

\usage{
fuzzyKmlSlow(traj, clusterAffectation, toPlot = "traj",
   fuzzyfier = 1.25, parAlgo = parALGO())
}
\arguments{
  \item{traj}{\code{[matrix(numeric)]}: Matrix holding the longitudinal data}
  \item{clusterAffectation}{\code{[vector(numeric)]}: Initial starting condition}
  \item{toPlot}{\code{[character]}: if "traj",
    then the trajectories are plot. If "none", there is no graphical
    display (faster).}
  \item{fuzzyfier}{\code{[numeric]}: value of the fuzzy k-means algorithm fuzzyfier.}
  \item{parAlgo}{\code{[\linkS4class{ParKml}]}: default parameters for
  the algorithm.}
}

\details{
  \code{fuzzyKmlSlow} is a new implementation of fuzzy k-means for
  longitudinal data (or trajectories). To date, it is writen in R (and
  not in C, this explain the "slow")
}
\value{
The matrix of the individual membership.
}

\seealso{
\code{\link{kml}}
}
\examples{
### Data generation
traj <- gald(25)["traj"]
partInit <- initializePartition(3,100,"kmeans--",traj)

### fuzzy Kml
partResult <- fuzzyKmlSlow(traj,partInit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques

