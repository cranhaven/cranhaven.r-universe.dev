\name{GrandSkinks}
\alias{GrandSkinks}
\docType{data}
\title{
Multi-season detection data for grand skinks
}
\description{
Results of an occupancy survey of 352 rocky outcrops ("tors") looking for grand skinks. Tors were surveyed up to 3 times per year for 5 years. The surrounding terrain was characterised as natural grassland or pasture.
}
\usage{data(GrandSkinks)}
\format{
  A data frame with 352 observations on the following 16 variables.
  \describe{
    \item{A1, A2, A3, B1, B2, B3, C1, C2, C3, D1, D2, D3, E1, E2, E3}{an array of observations of detection (1) or nondetection (0) of skinks for each of 3 occasions in 5 years. NA indicates occasions when a tor was not visited. }
    \item{habitat}{a factor indicating the type of grassland surrounding the tor.}
  }
}
\details{
The data are provided as a data frame, such as would result from reading in data from a text file. Further formatting is needed before using these for analysis: see the examples. 
}
\source{
Data distributed with PRESENCE.
}
\references{
MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.
}
\examples{
data(GrandSkinks)

# Extract detection histories:
DH <- GrandSkinks[, 1:15]
occMS0(DH, 3)


}
\keyword{datasets}
