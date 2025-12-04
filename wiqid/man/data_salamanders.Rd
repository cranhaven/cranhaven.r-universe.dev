\name{salamanders}
\alias{salamanders}
\docType{data}
\title{
Occupancy data for blue ridge salamanders
}
\description{
Detection/non-detection data for blue ridge salamanders (Eurycea wilderae) in Great Smoky Mountains National Park.
}
\usage{data(salamanders)}
\format{
A matrix with 39 rows corresponding to sites and 5 columns corresponding to survey occasions. 1 means that one or more salamanders were observed at the site/survey, 0 means none were seen.
}
\source{
Described in MacKenzie et al (2006) p99. The data are distributed with the software package PRESENCE.
}
\references{
MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.
}
\examples{
data(salamanders)

occSStime(salamanders, p ~ .time)
}
\keyword{datasets}
