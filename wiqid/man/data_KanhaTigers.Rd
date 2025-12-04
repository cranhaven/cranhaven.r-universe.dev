\name{KanhaTigers}
\alias{KanhaTigers}
\docType{data}
\title{
Capture history matrix for camera-trapped tigers
}
\description{
Capture history matrix for camera-trapped tigers
}
\usage{data(KanhaTigers)}
\format{
A matrix with 26 rows for animals trapped and 10 columns for the trapping occasions. KanhaTigers[i, j] = 1 if animal i was trapped on occasion j, zero otherwise.
}
\source{
Karanth, Nichols, Kumar, Link, Hines (2004) Tigers and their prey: Predicting carnivore densities from prey abundance. PNAS 101:4854-4858
}
\examples{
data(KanhaTigers)
dim(KanhaTigers)
closedCapMt(KanhaTigers)
}
\keyword{datasets}
