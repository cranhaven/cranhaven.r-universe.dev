\name{CAdata}
\alias{CAdata}
\docType{data}
\title{
Deidentified Survival Data for California
}
\description{
Survival time, censoring status, and geolocations (jittered to preserve anonymity) for 5000 ovarian cancer patients in California, using a Lambert projection (in meters).  \code{\link{CAmap}} is a map of California using the same projection.}

\usage{data(CAdata)}
\format{
  A data frame with 5000 observations on the following 6 variables.
  \describe{
    \item{\code{time}}{survival time.}
    \item{\code{event}}{censoring status.}
    \item{\code{X}}{projected X coordinate.}
    \item{\code{Y}}{projected Y coordinate.}
    \item{\code{AGE}}{patient age.}
    \item{\code{INS}}{insurance type: \code{Mng} (Managed Care: managed care, HMO, PPO and other private insurance), \code{Mcr} (Medicare), \code{Mcd} (Medicaid), \code{Oth}: (Other Insurance: military, county-funded), \code{Uni} (Not Insured, self-pay) and \code{Unk} (Unknown).}
  }
}

\examples{
data(CAdata)
summary(CAdata)
plot(CAdata$X,CAdata$Y) 
}

\keyword{datasets}
