\name{aphids}
\alias{aphids}
\title{
Data on the rate of infection of tobacco plants by a virus
spread by aphids.
}
\description{
The data set \code{aphids} is a data frame with 51 rows and
2 columns.  The rows correspond to 51 independent experiments
in which varying numbers of aphids were released in a flight
chamber containing 12 infected and 69 healthy tobacco plants.
The resulting number of infected plants (amongst those previously
healthy) was recorded.
}
\usage{
aphids
}
\format{
The data frame \code{aphids} contains the following columns:
\describe{
\item{\code{aphRel}}{
The number of aphids released in the flight chamber in
each instance.
}
\item{\code{plntsInf}}{
The resulting number (out of a possible 69) of infected plants
}}}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\source{
These data appear courtesy of Gilles Boiteau and George Tai of the
Potato Research Centre, Agriculture and Agri-Food Canada,
Fredericton, New Brunswick.  Any published work using these data
should cite the paper given in the \bold{References}.
}
\references{
Boiteau, G., M. Singh, R. P. Singh, G. C. C. Tai, and T. R. Turner
(1998).  Rate of spread of PVY-n by alate Myzus persicae (Sulzer)
from infected to healthy plants under laboratory conditions.  Potato
Research, vol. 41, pp. 335 -- 344.
}
\section{Determination of Infection Count}{
After 24 hours from the time that the aphids were released, the
flight chamber was fumigated to kill the aphids, and the previously
healthy plants were moved to a greenhouse and monitored to detect
symptoms of infection.  The number of plants displaying such symptoms
was recorded.
}
\keyword{datasets}
