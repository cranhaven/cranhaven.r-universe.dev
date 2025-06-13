\name{impactfactor}
\alias{impactfactor}
\docType{data}
\title{
Data set on metrics for scientific journals: 
}
\description{
A subset of 443 journals of the sections Computer Science Software, Decision
Sciences Statistics, Probability and Uncertainty and Mathematics Statistics and Probability and their scores for 3 metrics recorded each year from 1999 to 2013: IPP impact per publication, SNIP source normalized impact per paper (tries to weight
by the number of citations per subject fieeld to adjust for different citation cultures) and the
SJR SCImago journal rank (tries to capture average prestige per publication). This data
contains 31 percent of missing values.
}
\usage{data(impactfactor)
}

\format{
A data frame with 443 observations and  45 continuous variables
}

\source{
journalmetrics.com
}

\examples{
data(impactfactor)
\dontrun{ada.NA <- imputeada(impactfactor, lambda = 4.46, gamma = 1.9)
impactfactorcomp <- ada.NA$completeObs}
}
\keyword{datasets}



