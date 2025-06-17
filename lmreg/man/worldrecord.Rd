\name{worldrecord}
\alias{worldrecord}
\docType{data}
\title{
World record running times data
}
\description{
Men's and women's world record times for various out-door running distances, recognized by the International Association of Athletics Federations (IAAF) as of 17 November, 2017.
}
\usage{data(worldrecord)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{Distance}}{Running distance (in meters)}
    \item{\code{MenRecord}}{Men's record time (in seconds)}
    \item{\code{WomenRecord}}{Women's record time (in seconds)}
  }
}

\source{
International Association of Athletics Federations (https://www.iaaf.org/records/by-category/world-records).
}

\examples{
data(worldrecord)
head(worldrecord)
}
\keyword{datasets}
